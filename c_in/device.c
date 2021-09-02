/*
	Copyright (c) 2019,2021 Daniel T. Borelli <danieltborelli@gmail.com>

	This file is part of ArchIsoMyDrive.

	ArchIsoMyDrive is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	ArchIsoMyDrive is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with ArchIsoMyDrive.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "device.h"

#define xstrncpy(dst, src, n) do {	\
		strncpy(dst, src, n);	\
		dst[n-1] = '\0';	\
} while (0)


static struct udev *m_udev = NULL;
static struct udev_device *m_device = NULL;
static struct udev_monitor *m_monitor = NULL;
static char node_path[41] = {'\0'};
static char str_error[81] = {'\0'};
static size_t dev_blksize = 512;

/* Estilo: las funciones locales sin el prefijo Device_
 * */
static int IsDisk(struct udev_device *dev);
static struct udev_device *IsDiskUSB (struct udev_device *dev );
static void MakeDeviceProperty();
static void SetDeviceSize();
static void SetReadAHead();
static void *GetMemoryAlign(const size_t size);



struct CopyData {
	uint64_t buff_size;
	uint64_t max_read;
	float percent;
	float total;
	ssize_t len;
	int fd_device;
	int fd_iso;
	bool open;
	uint8_t *buff;
};

static struct CopyData copy;


char *Device_GetError()
{
	return str_error;
}

static void NewCopy()
{
	copy = (struct CopyData){1.0f,0.0f,-1,-1,0,0,0,false,NULL};
}

static void NewCopyBuffer(uint64_t size, const DeviceProperty *prop)
{
	const uint64_t minsz = 128 * 1024;
	const uint64_t readsz = prop->readahead * dev_blksize;
	copy.buff_size = minsz > readsz ? minsz : readsz;
	copy.buff = (uint8_t*)GetMemoryAlign(copy.buff_size);
	assert(NULL != copy.buff);
	copy.max_read = size;
	copy.total = (float)copy.max_read;
}


int Device_CopyDeviceToIso(const DeviceProperty *prop, const char *iso)
{
	if (copy.open)
		return E_NO;

	NewCopy();

	errno = 0;
	if (-1 == (copy.fd_iso = open(iso, O_WRONLY | O_TRUNC | O_CREAT))) {
		snprintf(str_error, 80, "%s", strerror(errno));
		return E_ISO_OW;
	}

	const char *device = prop->node_path;
	errno = 0;
	if (-1 == (copy.fd_device = open(device, O_RDONLY  | O_DIRECT))) {
		close(copy.fd_iso);
		snprintf(str_error, 80,  "%s", strerror(errno));
		return E_DEV_OR;
	}

	NewCopyBuffer(prop->totalbytes, prop);

	return E_NO;
}

int Device_CopyIsoToDevice(const char *iso, const uint64_t iso_size, const DeviceProperty *prop)
{
	if (copy.open)
		return E_NO;

	NewCopy();
	
	const char *device = prop->node_path;

	errno = 0;
	if (-1 == (copy.fd_device = open(device, O_WRONLY | O_DIRECT))) {
		snprintf(str_error, 80, "%s", strerror(errno));
		return E_DEV_OW;
	}

	errno = 0;
	if (-1 == (copy.fd_iso = open(iso, O_RDONLY))) {
		close(copy.fd_device);
		snprintf(str_error, 80, "%s", strerror(errno));
		return E_ISO_OR;
	}

	NewCopyBuffer(iso_size, prop);

	return E_NO;
}

int Device_ContinueCopyIsoToDevice()
{
	if (copy.max_read && (copy.len != -1)) {

		errno = 0;
		copy.len = read(copy.fd_iso, copy.buff, copy.buff_size);

		if (copy.len <= 0) {
			snprintf(str_error, 80, "%s", strerror(errno));
			Device_StopCopy();
			return E_ISO_R;
		}

		errno = 0;
		if (-1 == write(copy.fd_device, copy.buff, copy.len)) {
			snprintf(str_error, 80, "%s", strerror(errno));
			Device_StopCopy();
			return E_DEV_W;
		}

		copy.max_read -= copy.len;
		copy.percent = ((float)(copy.total - copy.max_read) / copy.total) * 100.0f;
	}

	return E_NO;
}

int Device_ContinueCopyDeviceToIso()
{
	if (copy.max_read && (copy.len != -1)) {

		errno = 0;
		copy.len = read(copy.fd_device, copy.buff, copy.buff_size);

		if (copy.len <= 0) {
			snprintf(str_error, 80, "%s", strerror(errno));
			Device_StopCopy();
			return E_DEV_R;
		}

		errno = 0;
		if (-1 == write(copy.fd_iso, copy.buff, copy.len)) {
			snprintf(str_error, 80, "%s", strerror(errno));
			Device_StopCopy();
			return E_ISO_W;
		}

		copy.max_read -= copy.len;
		copy.percent = ((float)(copy.total - copy.max_read) / copy.total) * 100.0f;
	}

	return E_NO;
}


uint64_t Device_GetLenCopy()
{
	return copy.max_read;
}

char *Device_GetStrPercentCopy()
{
	static char str[8];
	snprintf(str, 8, " %01.0f %%", copy.percent);
	return str;
}

float Device_GetPercentCopy()
{
	return copy.percent;
}

float Device_GetPercentTotalCopy()
{
	return copy.total;
}

void Device_StopCopy()
{
	free(copy.buff);
	close(copy.fd_device);
	close(copy.fd_iso);
	NewCopy();
}


static int IsDisk(struct udev_device *dev)
{
	char const *stype = udev_device_get_devtype(dev);
	return (stype && (0 == strncmp(stype, "disk", 4))) ? 1 : 0;
}


static struct udev_device *IsDiskUSB(struct udev_device *dev )
{
	struct udev_device *parent;
	parent = udev_device_get_parent_with_subsystem_devtype(
			dev,
			"usb",
			"usb_device");
	return parent;
}


DeviceProperty *Device_GetProperty()
{
	static DeviceProperty dev_prop = {0};
	return &dev_prop;
}

char *Device_GetDevicePath()
{
	return node_path;
}

const char* Device_GetAttribute(const char *attr)
{
	return  udev_device_get_sysattr_value(m_device, attr);
}


static void MakeDeviceProperty()
{
	xstrncpy(Device_GetProperty()->node_path,    node_path, 40);
	xstrncpy(Device_GetProperty()->id_vendor,    Device_GetAttribute("idVendor"), 40);
	xstrncpy(Device_GetProperty()->id_product,   Device_GetAttribute("idProduct"), 40);
	xstrncpy(Device_GetProperty()->product,      Device_GetAttribute("product"), 40);
	xstrncpy(Device_GetProperty()->manufacturer, Device_GetAttribute("manufacturer"), 40);
	xstrncpy(Device_GetProperty()->version,      Device_GetAttribute("version"), 40);
	xstrncpy(Device_GetProperty()->max_power,    Device_GetAttribute("bMaxPower"), 40);
	xstrncpy(Device_GetProperty()->bus,          Device_GetAttribute("busnum"), 40);

	const char *serial = Device_GetAttribute("serial");
	
	// Validar el serial
	unsigned char const *pc = (unsigned char const *)serial;

	for (; pc && *pc != '\0'; pc++) {
		if (*pc < 0x20 || *pc > 0x7f || *pc == ',') {
			serial = "NN"; // serial no valido
			break;
		}
	}

	xstrncpy(Device_GetProperty()->serial, serial, 40);

	/* XXX: testeando una falla en la funcion setDeviceSize */
	sleep(1);
	/**/

	SetDeviceSize();
	SetReadAHead();
}


int32_t Device_ReceiveEvents()
{
	assert(NULL != m_monitor);

	int32_t ret = RECEVENT_NONE;

	struct udev_device *dev = udev_monitor_receive_device(m_monitor);

	if (0 == IsDisk(dev)) {
		udev_device_unref(dev);
		return ret;
	}

	struct udev_device *parent;

	if ((parent = IsDiskUSB(dev))) {
		const char *action = udev_device_get_action(dev);
		xstrncpy(node_path, udev_device_get_devnode(dev), 40);
		m_device = parent;
		if (!strncmp(action, "add", 3)) {
			ret = RECEVENT_ADD;
			MakeDeviceProperty();
		} else if (!strncmp(action, "remove", 6)) {
			ret = RECEVENT_REMOVE;
		}
	}
	udev_device_unref(dev);
	return ret;
}


int32_t Device_GetMonitorEvent()
{
	assert(NULL != m_udev);
	m_monitor = udev_monitor_new_from_netlink(m_udev, "udev");
	udev_monitor_filter_add_match_subsystem_devtype(m_monitor, "block", NULL);
	udev_monitor_enable_receiving(m_monitor);
	return udev_monitor_get_fd(m_monitor);
}


void Device_Initialize()
{
	m_udev = udev_new();
	assert(NULL != m_udev);
}


void Device_Unitialize()
{
	if (m_monitor) {
		udev_monitor_unref(m_monitor);
		m_monitor = NULL;
	}

	if (m_udev) {
		udev_unref(m_udev);
		m_udev = NULL;
	}
	
	m_device = NULL;
}


static uint64_t GetSysFileSize(const char *path, bool dev)
{
	uint64_t size = -1;
	int ret;
	int fd = open(path, O_RDONLY, 0);

	assert(-1 != fd);

	if (dev) {
		ret = ioctl(fd, BLKGETSIZE64, &size);
		assert(-1 != ret);

		ret = ioctl(fd, BLKPBSZGET, &dev_blksize);
		assert(-1 != ret);

		close(fd);
	} else {
		struct stat statbuf;

		ret = fstat(fd, &statbuf);
		assert(-1 != ret);

		close(fd);
		size = statbuf.st_size;
	}

	return size;
}

uint64_t Device_GetFileSize(const char *path)
{
	return GetSysFileSize(path, false);
}

static void SetReadAHead()
{
	uint64_t readahead = 0;

	int fd = open(node_path, O_RDONLY, 0);

	assert(-1 != fd);

	int ret = ioctl(fd, BLKRAGET, &readahead );

	close(fd);

	assert(-1 != ret);

	assert(0 != readahead);

	Device_GetProperty()->readahead = readahead;
}


static void SetDeviceSize()
{
	char *size = NULL;
	Device_GetProperty()->totalbytes = GetSysFileSize(node_path, true);
	size = Device_GetFileSizeStr(Device_GetProperty()->totalbytes);
	xstrncpy(Device_GetProperty()->size, size, 40);
	free(size);
}

char *Device_GetFileSizeStr(const uint64_t size)
{
	float bytes = (float)size;
	/*
	 * Basado en: https://codegolf.stackexchange.com/a/52202
	 * */
	char const *units = " kMGTPEZY";
	while ((bytes /= 1024 ) >= .95)
		++ units;

	char const *const format = (*units-' ') ? "%.1f %ciB" : "%.0f B";
	/* * */

	char *str = NULL;

	int n = asprintf(&str, format, bytes * 1024, *units);

	assert(-1 != n);

	return str;
}

void Device_ListOfDevices(ListDevicesCallback list_dev_cb)
{
	assert(NULL != m_udev);
	assert(NULL != list_dev_cb);

	struct udev_enumerate* enumerate;
	struct udev_list_entry* devices, *dev_list_entry;
	struct udev_device * parent;

	enumerate = udev_enumerate_new(m_udev);
	udev_enumerate_add_match_subsystem(enumerate, "block");
	udev_enumerate_scan_devices(enumerate);
	devices = udev_enumerate_get_list_entry(enumerate);

	udev_list_entry_foreach(dev_list_entry, devices) {
		const char* path = udev_list_entry_get_name(dev_list_entry);
		struct udev_device* dev = udev_device_new_from_syspath(m_udev, path);
		
		if (!dev) continue;

		if (1 == IsDisk(dev)) {
			if (NULL != (parent = IsDiskUSB(dev))) {
				xstrncpy(node_path, udev_device_get_devnode(dev), 40);
				m_device = parent;
				MakeDeviceProperty();
				// call pascal func.
				(*list_dev_cb)();
			}
		}
		udev_device_unref(dev);
	}
	udev_enumerate_unref(enumerate);
}

static void *GetMemoryAlign(const size_t size)
{
	const size_t align = sysconf(_SC_PAGESIZE);
	return aligned_alloc(align, size);
}


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

#pragma once

#define _GNU_SOURCE	// asprintf
			// aligned_alloc
			// tamaño de archivos >2GiB,offset=64

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <libudev.h>
#include <blkid/blkid.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* Estas macros esta definidas en blockdev.h */

#define BLKRAGET	_IO(0x12,99)		/* get current read ahead setting */
#define BLKGETSIZE64	_IOR(0x12,114,size_t)	/* return device size in bytes (u64 *arg) */
#define BLKPBSZGET	_IOR(0x12,112,size_t)	/* IO block */

// Constantes para ReceiveEvents.
#define RECEVENT_NONE 0		// ningun evento.
#define RECEVENT_ADD  1		// nuevo dispositivo.
#define RECEVENT_REMOVE 2	// dispositivo removido.


#define E_NO		0
#define E_DEV_R		1
#define E_DEV_W		2
#define E_DEV_OR	3
#define E_DEV_OW	4
#define E_ISO_R		5
#define E_ISO_W		6
#define E_ISO_OR	7
#define E_ISO_OW	8


typedef struct {
	char node_path		[41];
	char id_vendor		[41];
	char id_product		[41];
	char product		[41];
	char manufacturer	[41];
	char serial		[41];
	char version		[41];
	char max_power		[41];
	char bus		[41];
	char size		[41];
	uint64_t totalbytes;
	uint64_t readahead;
} DeviceProperty;

typedef void (*ListDevicesCallback)(void);


/* Estilo: las funciones globales con el prefijo Device_ 
 * indicando a que módulo corresponden.
 * */

void Device_Initialize();

void Device_Unitialize();

int32_t Device_GetMonitorEvent();

const char *Device_GetAttribute(const char *attr);

int32_t Device_ReceiveEvents();

DeviceProperty *Device_GetProperty();

char *Device_GetPath();

void Device_GetListOfDevices(ListDevicesCallback list_dev_cb);

uint64_t Device_GetFileSize(const char *path);

char *Device_GetFileSizeStr(const uint64_t size);

int Device_CopyDeviceToIso(const DeviceProperty *prop, const char *iso);

int  Device_ContinueCopyDeviceToIso();

int Device_CopyIsoToDevice(const char *iso, const uint64_t iso_size, const DeviceProperty *prop);

int Device_ContinueCopyIsoToDevice();

void Device_StopCopy();

char *Device_GetStrPercentCopy();

float Device_GetPercentCopy();

float Device_GetTotalPercentCopy();

uint64_t Device_GetLenCopy();

char *Device_GetError();

char *Device_GetDevicePath();


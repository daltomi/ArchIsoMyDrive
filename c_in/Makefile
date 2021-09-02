ifeq ("$(shell which pkg-config  2> /dev/null)","")
$(error 'pkg-config' NOT FOUND)
endif

ifeq ("$(shell pkg-config --libs libudev 2> /dev/null)","")
$(error 'libudev' NOT FOUND)
endif

ifeq ("$(shell pkg-config --libs mount 2> /dev/null)","")
$(error 'libmount' NOT FOUND)
endif

ifeq ("$(shell pkg-config --libs blkid 2> /dev/null)","")
$(error 'libblkid' NOT FOUND)
endif

CC = gcc

# Nota: --cflags mount ya incluye los flags de blkid.
# Nota: --cflags udev no retorna valor alguno. fix: no es necesario.
CFLAGS = $(shell pkg-config --cflags udev) $(shell pkg-config --cflags mount)

OBJ := c_device.o c_mnt_iso.o

release: CFLAGS+= -O2 -DNDEBUG
release: CLIBS+= -Wl,-s
release: $(OBJ)

debug: CFLAGS+= -pedantic -Wall -Wextra -O0 -ggdb -DDEBUG
debug: $(OBJ)

c_device.o: device.c
	$(CC) $(CFLAGS) -c $< -o ../$@

c_mnt_iso.o: mnt_iso.c
	$(CC) $(CFLAGS) -c $< -o ../$@

.PHONY: release

clean:
	rm -v ../c_device.o ../c_mnt_iso.o

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

#include "mnt_iso.h"

typedef struct part_info part_info;

void mnt_iso_init(const char *source);

void mnt_iso_free(void);

char *mnt_iso_get_error(void);

int mnt_iso_mount(int id, part_info *part, const char *target);

int mnt_iso_umount(int id, const part_info *part);

int mnt_iso_find_partitions(void);

part_info *mnt_iso_get_list_of_partitions(void);


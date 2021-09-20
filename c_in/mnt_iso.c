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
#define _GNU_SOURCE
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>
#include <blkid.h>
#include <libmount.h>

#define xstrncpy(dst, src, n) do {	\
		strncpy(dst, src, n);	\
		dst[n-1] = '\0';	\
} while (0)

typedef struct part_info {
	blkid_loff_t start;
	blkid_loff_t size;
	blkid_loff_t offset;
	int partno;
	int mounted;
	char uuid[32];
	char target[PATH_MAX];
} part_info;


static struct mnt_iso {
	blkid_partlist partlst;
	blkid_probe probe;
	int partitions;
	char error[200];
	char *source;
} *mnt = NULL;

static part_info *part_info_to_free = NULL;


void mnt_iso_init(const char *source)
{
	assert(NULL != source);

	mnt = calloc(1, sizeof(*mnt));
	assert(NULL != mnt);

	mnt->source = strdup(source);
}


void mnt_iso_free(void)
{
	assert(NULL != mnt);

	if (part_info_to_free) {
		free(part_info_to_free);
		part_info_to_free = NULL;
	}

	if (mnt->source) {
		free(mnt->source);
		mnt->source = NULL;
	}
	
	free(mnt);
	mnt = NULL;
}


int mnt_iso_mount(int id, part_info *part, const char *target)
{
	assert(NULL != mnt);
	assert(NULL != part);
	assert(NULL != target);

	struct libmnt_context* ctx = mnt_new_context();
	assert(NULL != ctx);

	mnt_reset_context(ctx);

	char *options = NULL;

#ifdef __i386__
	int n = asprintf(&options, "loop,offset=%lld", part[id].offset);
#else
	int n = asprintf(&options, "loop,offset=%ld", part[id].offset);
#endif
	assert(-1 != n);

	mnt_context_set_options(ctx, options);
	
	free(options);

	mnt_context_set_mflags(ctx, MS_RDONLY);
	mnt_context_set_source(ctx, mnt->source);
	mnt_context_set_target(ctx, target);
	
	const int ret = mnt_context_mount(ctx);

	if (ret == 0 && mnt_context_get_status(ctx) == 1) {
		xstrncpy(part[id].target, target, PATH_MAX);
		part[id].mounted = 1;

	} else {
#if (LIBMOUNT_MAJOR_VERSION == 2) && (LIBMOUNT_MINOR_VERSION >= 30)
		mnt_context_get_excode(ctx, ret, mnt->error, sizeof(mnt->error));
#else
		xstrncpy(mnt->error, strerror(errno), sizeof(mnt->error));
#endif
		part[id].mounted = 0;
	}

	mnt_free_context(ctx);

	return part[id].mounted;
}


char *mnt_iso_get_error(void)
{
	assert(NULL != mnt);
	return mnt->error;
}

int mnt_iso_umount(int id, part_info *part)
{
	assert(NULL != part);

	struct libmnt_context* ctx = mnt_new_context();
	assert(NULL != ctx);

	mnt_reset_context(ctx);
	mnt_context_set_source(ctx, mnt->source);
	mnt_context_set_target(ctx, part[id].target);

	int ret = mnt_context_umount(ctx);

	errno = 0;
	if (ret == 0 && mnt_context_get_status(ctx) == 1) {
		part[id].mounted = 0;
	} else {
#if (LIBMOUNT_MAJOR_VERSION == 2) && (LIBMOUNT_MINOR_VERSION >= 30)
		mnt_context_get_excode(ctx, ret, mnt->error, sizeof(mnt->error));
#else
		xstrncpy(mnt->error, strerror(errno), sizeof(mnt->error));
#endif
		part[id].mounted = 1;
	}

	mnt_free_context(ctx);

	return part[id].mounted;
}


int mnt_iso_find_partitions(void)
{
	assert(NULL != mnt);

	if (!mnt->probe)
		mnt->probe = blkid_new_probe_from_filename(mnt->source);

	if (!mnt->probe)
		return 0;

	if (!mnt->partlst)
		mnt->partlst = blkid_probe_get_partitions(mnt->probe);
	
	if (!mnt->partlst)
		return 0;

	mnt->partitions = blkid_partlist_numof_partitions(mnt->partlst);

	return mnt->partitions;
}


part_info *mnt_iso_get_list_of_partitions(void)
{
	assert(NULL != mnt);

	part_info_to_free = calloc(mnt->partitions, sizeof(*part_info_to_free));
	assert(NULL != part_info_to_free);

	part_info *part = part_info_to_free;

	blkid_loff_t sector_size = blkid_probe_get_sectorsize(mnt->probe);

	blkid_partition blkpart;

	for (int i = 0; i < mnt->partitions; ++i) {
		blkpart = blkid_partlist_get_partition(mnt->partlst, i);
		assert(NULL != blkpart);

		part[i].partno = blkid_partition_get_partno(blkpart);
		part[i].start = blkid_partition_get_start(blkpart);
		part[i].size = blkid_partition_get_size(blkpart);
		part[i].offset = part[i].start * sector_size;
		
		const char *uuid = blkid_partition_get_uuid(blkpart);
		
		if (uuid) {
			xstrncpy(part[i].uuid, uuid, 32);
		}
	}

	return part;
}

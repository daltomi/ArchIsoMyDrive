{
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
}

{
 Representa la envoltura del archivo c_in/mnt_iso.c
}
unit uMntIso;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, BaseUnix;

type
  {$PACKRECORDS C}

        PPartInfo = ^TPartInfo;

        TPartInfo = record
                start : cint64;
                size  : cint64;
                offset : cint64;
                partno : cint;
                mounted : cint;
                uuid : array[0..31] of char;
                target : array[0..PATH_MAX -1] of char;
        end;

        procedure CFree(P: pointer) ;cdecl;
        procedure mnt_iso_init(source : PChar); cdecl;
        procedure mnt_iso_free; cdecl;
        function mnt_iso_mount(id : cint; part: PPartInfo; target : PChar): cint; cdecl;
        function mnt_iso_get_error: PChar; cdecl;
        function mnt_iso_umount(id:cint; part: PPartInfo): cint; cdecl;
        function mnt_iso_find_partitions: cint; cdecl;
        function mnt_iso_get_list_of_partitions: PPartInfo; cdecl;

implementation

{$linklib c}
{$linklib blkid}
{$linklib mount}
{$link c_mnt_iso.o}

        procedure CFree(P: pointer) ;cdecl; external name 'free';
        procedure mnt_iso_init(source : PChar); cdecl; external;
        procedure mnt_iso_free; cdecl; external;
        function mnt_iso_mount(id : cint; part: PPartInfo; target : PChar): cint; cdecl; external;
        function mnt_iso_get_error: PChar; cdecl; external;
        function mnt_iso_umount(id:cint; part: PPartInfo): cint; cdecl; external;
        function mnt_iso_find_partitions: cint; cdecl; external;
        function mnt_iso_get_list_of_partitions: PPartInfo; cdecl; external;
end.


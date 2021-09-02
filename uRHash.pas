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
unit uRHash;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, CTypes;

const

RHASH_MD5        = $04;
RHASH_SHA1       = $08;
RHASH_SHA256     = $20000;
RHASH_SHA384     = $40000;
RHASH_SHA512     = $80000;


function rhash_file(id: cuint; filepath: PChar; hash : pcuchar): cint; cdecl;

implementation

{$linklib rhash}

function rhash_file(id: cuint; filepath: PChar; hash : pcuchar): cint; cdecl; external;

end.


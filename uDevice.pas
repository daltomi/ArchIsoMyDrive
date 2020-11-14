{
        Copyright (c) 2019,2020 Daniel T. Borelli <daltomi@disroot.org>

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
 Representa la envoltura del archivo c_in/device.c
}

unit uDevice;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, CTypes, uResources;


const
         E_NO           = 0;
         E_DEV_R        = 1;
         E_DEV_W        = 2;
         E_DEV_OR       = 3;
         E_DEV_OW       = 4;
         E_ISO_R        = 5;
         E_ISO_W        = 6;
         E_ISO_OR       = 7;
         E_ISO_OW       = 8;

type
  {$PACKRECORDS C}

  PDeviceProperty = ^TDeviceProperty;

  TDeviceProperty = record
          node_path : array[0..40] of char;
          id_vendor : array[0..40] of char;
          id_product : array[0..40] of char;
          product : array[0..40] of char;
          manufacturer : array[0..40] of char;
          serial : array[0..40] of char;
          version : array[0..40] of char;
          max_power : array[0..40] of char;
          bus : array[0..40] of char;
          size: array[0..40] of char;
          totalbytes: cuint64;
          readahead: cuint64;
  end;

        function Device_ProcessError(eid : cint): string;

        type TListDevicesCallback = procedure; cdecl;

        procedure CFree(P: pointer) ;cdecl;
        procedure Device_Initialize; cdecl;
        procedure Device_Unitialize; cdecl;
        function Device_GetMonitorEvent : ctypes.cint32; cdecl;
        function Device_ReceiveEvents : ctypes.cint32; cdecl;
        procedure Device_ListOfDevices(cb : TListDevicesCallback); cdecl;
        function Device_GetAttribute(attr : PChar) : PChar; cdecl;
        function Device_GetProperty : PDeviceProperty; cdecl;
        function Device_GetFileSize(path : PChar) : cuint64; cdecl;
        function Device_GetFileSizeStr(size : cuint64) : PChar; cdecl;
        function Device_CopyIsoToDevice(iso: PChar; isosize: cuint64; prop: PDeviceProperty): cint; cdecl;
        function Device_ContinueCopyIsoToDevice: cint; cdecl;

        function Device_CopyDeviceToIso(prop: PDeviceProperty; iso: PChar): cint; cdecl;
        function Device_ContinueCopyDeviceToIso: cint; cdecl;

        procedure Device_StopCopy; cdecl;
        function Device_GetLenCopy: cuint64; cdecl;
        function Device_GetStrPercentCopy: PChar; cdecl;
        function Device_GetPercentCopy: cfloat; cdecl;
        function Device_GetPercentTotalCopy: cfloat; cdecl;
        function Device_GetError: PChar; cdecl;
        function Device_GetDevicePath: PChar; cdecl;

implementation

{$linklib c}            { implica libc    }
{$linklib udev}         { implica libudev }
{$link c_device.o}      { linker only     }

        procedure CFree(P: pointer) ;cdecl; external name 'free';
        procedure Device_Initialize; cdecl; external;
        procedure Device_Unitialize; cdecl;  external;
        function Device_GetMonitorEvent : cint32; cdecl;  external;
        function Device_ReceiveEvents : cint32; cdecl;  external;
        procedure Device_ListOfDevices(cb : TListDevicesCallback); cdecl;  external;
        function Device_GetAttribute(attr : PChar) : PChar; cdecl;  external;
        function Device_GetProperty : PDeviceProperty; cdecl;  external;
        function Device_GetFileSize(path : PChar) : cuint64; cdecl;  external;
        function Device_GetFileSizeStr(size : cuint64) : PChar; cdecl; external;
        function Device_CopyIsoToDevice(iso: PChar; isosize: cuint64; prop: PDeviceProperty): cint; cdecl ; external;
        function Device_ContinueCopyIsoToDevice: cint; cdecl; external;

        function Device_CopyDeviceToIso(prop: PDeviceProperty; iso: PChar): cint; cdecl; external;
        function Device_ContinueCopyDeviceToIso: cint; cdecl; external;

        procedure Device_StopCopy; cdecl; external;
        function Device_GetLenCopy: cuint64; cdecl; external;
        function Device_GetStrPercentCopy: PChar; cdecl; external;
        function Device_GetPercentCopy: cfloat; cdecl; external;
        function Device_GetPercentTotalCopy: cfloat; cdecl; external;
        function Device_GetError: PChar; cdecl; external;
        function Device_GetDevicePath: PChar; cdecl; external;


        function Device_ProcessError(eid : cint): string;
        var
                error : PChar;
        begin
                error := Device_GetError;

                case eid of
                         E_DEV_R:   Result := rsE_DEV_R + error;
                         E_DEV_W:   Result := rsE_DEV_W + error;
                         E_DEV_OR:  Result := rsE_DEV_OR + error;
                         E_DEV_OW:  Result := rsE_DEV_OW + error;
                         E_ISO_R:   Result := rsE_ISO_R + error;
                         E_ISO_W:   Result := rsE_ISO_W + error;
                         E_ISO_OR:  Result := rsE_ISO_OR + error;
                         E_ISO_OW:  Result := rsE_ISO_OW + error;
                end;
         end;

end.


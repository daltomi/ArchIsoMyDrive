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
unit uResources;

{$mode objfpc}{$H+}

interface

uses SysUtils;

const
        APP_VER = '6.1';
        APP_TITLE = 'ArchIsoMyDrive';
{$ifopt D+}
        APP_TITLE_VER = APP_TITLE + ' v' + APP_VER + ' (devel)';
{$else}
        APP_TITLE_VER = APP_TITLE + ' v' + APP_VER;
{$endif}
        APP_BIN = 'archisomydrv';

ResourceString

        rsErrorDlgTitle = 'Error - ' + APP_TITLE;
        rsErrNotRoot = 'You must have administrator permissions.';
        rsErrUniqueInstance = APP_TITLE + ' is already running.';
        rsErrorSizeDevice = 'The size of the device is smaller' + LineEnding +
                            'than the one in the ISO file.';

        rsErrorIsoCero = 'The size of the ISO file is equal to zero.';
        rsErrorIsoNotOpen = 'Unable to open file.';
        rsErrorFileNotExist = 'The file does not exists:';

        rsErrorDropFile = 'Only .iso or .img files are accepted.';

        rsVersion = 'Version:';
        rsConfirmDlgTitle = 'Confirmation - ' + APP_TITLE;

        rsCopyMessage = 'Do you want to start with the format?' + LineEnding +
                        'You will lose the device data.';

        rsOverwriteFile = 'The file already exists.' + LineEnding +
                          'Do you want to overwrite the file?';

        rsAskExit = 	'The ISO file is currently being copied to device.' +
        	  	LineEnding + 'Do you want to leave?';

        rsEndCopyOk = 'The copy finished successfully.';

        // no todos los estatus estan en main, algunos
        // se encuentran en otros diálogos de control.
        rsStatusTerminate = 'Finished.';
        rsStatusCopying = 'Copying...';
        rsStatusError = 'Error.';
        rsStatusPause = 'Pause...';
        rsStatusCancel = 'Cancelled.';
        rsStatusWaiting = 'Waiting.';
        rsStatusFormatting = 'Formatting...';

        rsUsbUnity = 'Device: ';
        rsUsbUnityEmpty = '<none>';

        rsProgress = 'Progress: ';

        rsParamUnknown = 'Unknown parameter.';
        rsParamCorrect = 'Valid parameter';
        rsParamHelp = 'Help.';

        rsChecksumMatch = 'The checksum matches.';
        rsChecksumNotMatch = 'The checksum does not matches.';

        rsChecksumTitle = 'Checksums - ' + APP_TITLE;

        rsMountTitle = 'Mount ISO - ' + APP_TITLE;
        rsMountErrorNoPartitions = 'The ISO file has no partition.';
        rsMountErrorNoMount = 'Failed to mount the partition.';
        rsMountErrorNoUmount = 'Failed to umount the partition.';
        rsMountErrorSaveReport = 'Failed to save report.';
        rsMountErrorNoClose = 'There is a mounted partition, unmount it before leaving.';
        rsMountErrorMountOnlyOne = 'There is a mounted partition.' + LineEnding +
                                   'Only one partition at a time.';

        rsFormatTitle = 'Format Device - ' + APP_TITLE;
        rsFormatDescription = 'Device: %s' +  LineEnding +
                               'Manufacturer: %s %s' + LineEnding +
                               'Size: %s';
        rsFormatErrorNoMkfs = 'No program to format was found.';
        rsFormatErrorNoProgramFound = 'The program was not found: ';
        rsFormatWarningSlow = 'Formatting with this file system is VERY slow.';
        rsFormatError = 'Formatting failed. Your device is corrupt.';

        rsCloneTitle = 'Clone Device - ' + APP_TITLE;
        rsCloneErrorCancel = 'The cloning of the device has been canceled.';
        rsCloneDescription = 'Device: %s' + LineEnding + LineEnding + 'ISO: %s';
        rsCloneProgress = 'Progress: ';

        rsE_DEV_R = 'Could not read from the device: ';
        rsE_DEV_W = 'Could not write to the device: ';
        rsE_DEV_OR = 'The device could not be opened for reading: ';
        rsE_DEV_OW = 'The device could not be opened for writing: ';

        rsE_ISO_R = 'The ISO file could not be read: ';
        rsE_ISO_W = 'Could not write to the ISO file: ';
        rsE_ISO_OR = 'The ISO file could not be opened for reading: ';
        rsE_ISO_OW = 'The ISO file could not be opened for writing: ';

        rsSaveIsoFile = 'Save ISO file - ' + APP_TITLE;
        rsClose = '&Close';
        rsSave = '&Save...';
        rsFormat = '&Format...';
        rsDevice = 'Device:';
        rsSize = 'Size:';
        rsVendor = 'Vendor ID:';
        rsProduct = 'Product:';
        rsProductID = 'Product ID:';
        rsManufacturer = 'Manufacturer:';
        rsSerial = 'Serial:';
        rsPowerMax= 'Power max.:';

        rsDlgOpenIsoTitle = 'Open an ISO / IMG file - ' + APP_TITLE;




implementation

end.


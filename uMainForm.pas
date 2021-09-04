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

unit uMainForm;

{$mode objfpc}{$H+}

{$rangechecks on} {debug}

interface

uses
        Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
        Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, clocale,
        uDevice, LCLIntf, InterfaceBase, LCLType, EditBtn, CTypes, Contnrs,
        uDeviceInfoForm, uResources, uChecksumsForm, uMountForm, uMntIso,
        uCloneDeviceForm, uFormatForm, DateUtils;

type

        { TMainForm }

        TMainForm = class(TForm)
                Bevel1: TBevel;
                btnIsoOpen: TBitBtn;
                btnStart: TBitBtn;
                btnClose: TBitBtn;
                btnStop: TBitBtn;
                edtIsoName: TEdit;
                Image1: TImage;
                imgList: TImageList;
                Label1: TLabel;
                Label2: TLabel;
                Label3: TLabel;
                Label4: TLabel;
                Label5: TLabel;
                Label6: TLabel;
                lblStatus: TLabel;
                lblCopyPercent: TLabel;
                lblIsoSize: TLabel;
                lblElapsed: TLabel;
                lblUsbSize: TLabel;
                lblIsoName: TLabel;
                lblUsbName: TLabel;
                lstBoxDevices: TListBox;
                MainMenu: TMainMenu;
                menuFile: TMenuItem;
                menuFileOpenISO: TMenuItem;
                menuToolDeviceFormat: TMenuItem;
                menuToolDevice: TMenuItem;
                menuToolDeviceClone: TMenuItem;
                menuToolISOMount: TMenuItem;
                MenuItem2: TMenuItem;
                menuToolsISO: TMenuItem;
                menuToolISOChecksums: TMenuItem;
                menuTools: TMenuItem;
                menuVer: TMenuItem;
                menuVerInfo: TMenuItem;
                menuQuit: TMenuItem;
                lblVersion: TStaticText;
                timerElapsed: TTimer;
                procedure btnIsoOpenClick(Sender: TObject);
                procedure btnStartClick(Sender: TObject);
                procedure btnStopClick(Sender: TObject);
                procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
                procedure FormCreate(Sender: TObject);
                procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
                procedure lstBoxDevicesDblClick(Sender: TObject);
                procedure lstBoxDevicesSelectionChange(Sender: TObject; User: boolean);
                procedure menuFileClick(Sender: TObject);
                procedure menuFileOpenISOClick(Sender: TObject);
                procedure menuToolDeviceCloneClick(Sender: TObject);
                procedure menuToolDeviceFormatClick(Sender: TObject);
                procedure menuToolISOChecksumsClick(Sender: TObject);
                procedure menuToolISOMountClick(Sender: TObject);
                procedure menuToolsClick(Sender: TObject);
                procedure menuVerClick(Sender: TObject);
                procedure menuQuitClick(Sender: TObject);
                function AskCopyContinue: boolean;
                procedure menuVerInfoClick(Sender: TObject);
                procedure DeviceEvent(AData: PtrInt; AFlags: dword);
                procedure AddItemToDevicesList(const dev: PDeviceProperty);
                procedure FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
                procedure RemoveItemFromDevicesList(const devicePath : PChar);
                procedure FreeAllHash(A : pointer; B : pointer);
                procedure FillListDevices(A : pointer; B : pointer);
                procedure NewListItem(const dev : PDeviceProperty);
                procedure TimerElapsedOn(Sender: TObject);
                procedure StartTimer;
                procedure ValidateActionButtons;
                function GetIsoSize: cuint64;
                function GetDeviceSize: cuint64;
                function ValidateSizesOfFiles: boolean;
                procedure RunCopyIsoToDevice;
                procedure SetValuesOfIsoFile;
                function TryOpenIsoFile(const tryIsoFileName: string) : boolean;

        private
                hashDevices : TFPHashList;
                DeviceEventHandler : PEventHandler;
                isoFileName : string;
                oldIsoFileName : string;
                isoInitialDir : string;
                timerStart: TDateTime;

        public
                copyPause   : boolean;
                copyRunning : boolean;
        end;


var
        MainForm: TMainForm;



implementation

{$R *.lfm}



{ TMainForm }

procedure TMainForm.RunCopyIsoToDevice;
var
        eid : cint;
begin
        while copyRunning  do
        begin
                Application.ProcessMessages;
                if copyPause then break;

                eid := Device_ContinueCopyIsoToDevice;
                if  eid <> E_NO then
                begin
                        timerElapsed.enabled := False;

                        MessageDlg(rsErrorDlgTitle, Device_ProcessError(eid), mtError, [mbOk], 0);

                        Device_StopCopy;
                        btnStop.Enabled := False;
                        btnClose.Enabled := True;
                        lstBoxDevices.Enabled := True;
                        btnIsoOpen.Enabled := True;
                        btnStart.ImageIndex := 0;
                        btnStart.Enabled := False;
                        lblCopyPercent.Caption := '0 %';
                        lblStatus.Caption := rsStatusError;
                        lblUsbName.Caption := rsUsbUnity;
                        lblUsbSize.Caption := '0 KiB';
                        copyRunning := False;
                        break;
                end;

                lblStatus.Caption := rsStatusCopying;
                lblCopyPercent.Caption := Device_GetStrPercentCopy;

                Application.ProcessMessages;
                if copyRunning and (Device_GetLenCopy = 0) then
                begin
                       timerElapsed.enabled := False;
                       lblStatus.Caption := rsStatusTerminate;

                       ShowMessage(rsEndCopyOk);

                       Device_StopCopy;
                       btnStop.Enabled := False;
                       btnClose.Enabled := True;
                       btnIsoOpen.Enabled := True;
                       lstBoxDevices.Enabled := True;
                       btnStart.ImageIndex := 0;
                       lblCopyPercent.Caption := '0 %';
                       copyRunning := False;
                       break;
                end;
       end;
end;


procedure TMainForm.SetValuesOfIsoFile;
var
        sizeStr : PChar;
        size : cuint64;
begin
        size := GetIsoSize;
        if size <> 0 then
        begin
               oldIsoFileName := isoFileName;
               edtIsoName.Caption :=  ExtractFileName(isoFileName);
               sizeStr := Device_GetFileSizeStr(size);
               lblIsoSize.Caption:= sizeStr;
               lblIsoName.Caption:= 'ISO: ' + edtIsoName.Caption;
               lblCopyPercent.Caption := '0 %';
               lblStatus.Caption := rsStatusWaiting;
               CFree(Pointer(sizeStr));
               ValidateActionButtons;
        end
        else
        begin
               MessageDlg(rsErrorDlgTitle, rsErrorIsoCero, mtError, [mbOk], 0);
               isoFileName := oldIsoFileName;
               edtIsoName.Caption := ExtractFileName(oldIsoFileName);
        end;
end;

procedure TMainForm.btnIsoOpenClick(Sender: TObject);
var

        dlgOpenIso : TOpenDialog;
begin
        dlgOpenIso := TOpenDialog.Create(Self);
        dlgOpenIso.Filter:= 'ISO|*.iso|IMG|*.img';
        dlgOpenIso.FilterIndex := 1;
        dlgOpenIso.Title := rsDlgOpenIsoTitle;
        dlgOpenIso.InitialDir := isoInitialDir;

        oldIsoFileName := isoFileName;

        if dlgOpenIso.Execute then
        begin
                isoFileName := dlgOpenIso.FileName;
                isoInitialDir := ExtractFileDir(isoFileName);
                SetValuesOfIsoFile;
        end;

        dlgOpenIso.Free;
end;


procedure TMainForm.btnStartClick(Sender: TObject);
var
        dev : PDeviceProperty;
        eid : cint;
begin
        if copyRunning then
        begin
                copyPause := not copyPause;
		if copyPause then
                begin
                        btnStart.ImageIndex := 0;
                        lblStatus.Caption := rsStatusPause;
                end
                else
                begin
                        btnStart.ImageIndex := 1;
                end;
                timerElapsed.enabled := not copyPause;
                RunCopyIsoToDevice;
                Exit;
        end;

        if AskCopyContinue = False then
                Exit;

        StartTimer;

        dev := hashDevices.Find(hashDevices.NameOfIndex(lstBoxDevices.ItemIndex));

        assert(Assigned(dev));

        eid := Device_CopyIsoToDevice(PChar(isoFileName), GetIsoSize, dev);

        if eid <> E_NO  then
        begin
                timerElapsed.enabled := false;

                MessageDlg(rsErrorDlgTitle, Device_ProcessError(eid), mtError, [mbOk], 0);
                btnStart.ImageIndex:= 0;
                btnStart.Enabled := False;
                btnStop.Enabled := False;
                btnClose.Enabled := True;
                lstBoxDevices.Enabled := True;
                btnIsoOpen.Enabled := True;
                menuQuit.Enabled := True;
                copyRunning := False;
                lblStatus.Caption := rsStatusError;
                lblUsbName.Caption := rsUsbUnity;
                lblUsbSize.Caption := '0 KiB';
                Exit;
        end;

        btnStart.ImageIndex:= 1;
        btnStop.Enabled := True;
        btnClose.Enabled := False;
        btnIsoOpen.Enabled := False;
        lstBoxDevices.Enabled := False;
        copyPause := False;
        copyRunning := True;

        RunCopyIsoToDevice;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
        copyPause := False;
        copyRunning := False;
	btnStop.Enabled := False;
        btnClose.Enabled := True;
        lstBoxDevices.Enabled := True;
        btnIsoOpen.Enabled := True;
        btnStart.ImageIndex := 0;
        lblStatus.Caption := rsStatusCancel;
        timerElapsed.Enabled := False;

        Device_StopCopy;
end;

procedure TMainForm.ValidateActionButtons;
begin
        if lstBoxDevices.Count > 0 then
        begin
                if (edtIsoName.Text <> '') and ValidateSizesOfFiles then
                        btnStart.Enabled := True
                else
                        btnStart.Enabled := False;
        end
        else
        begin
             btnStart.Enabled := False;
             btnStop.Enabled := False;
        end;
end;

function TMainForm.ValidateSizesOfFiles: boolean;
var
        iso : cuint64;
        dev : cuint64;
begin
        iso := GetIsoSize;

        if iso = 0 then
                Exit(False);

        dev := GetDeviceSize;

        if dev < iso then
        begin
                MessageDlg(rsErrorDlgTitle, rsErrorSizeDevice, mtError, [mbOk], 0);
                Exit(False);
        end;

        Exit(True);
end;

function TMainForm.GetIsoSize: cuint64;
var
        size: cuint64 = 0;
begin
        if isoFileName <> '' then
                size := Device_GetFileSize(PChar(isoFileName));

        Exit(size);
end;

function TMainForm.GetDeviceSize: cuint64;
var
        dev : PDeviceProperty;
begin
        if hashDevices.count > 0 then
        begin
                dev := hashDevices.Find(hashDevices.NameOfIndex(lstBoxDevices.ItemIndex));
                if dev <> nil then
                        exit(dev^.totalbytes);
        end;
        exit(0);
end;


procedure TMainForm.FreeAllHash (A : pointer; B : pointer);
begin
        Dispose(PDeviceProperty(A));
end;


procedure TMainForm.FillListDevices (A : pointer; B : pointer);
begin
        NewListItem(PDeviceProperty(A));
end;


{
Pointer function callback.
External Call.
}
procedure ListDevicesCallback; cdecl;
begin
        MainForm.AddItemToDevicesList(Device_GetProperty);
end;


procedure TMainForm.NewListItem(const dev : PDeviceProperty);
var
        attr : string;
begin
        attr := dev^.manufacturer + ' ' + dev^.product;
        lstBoxDevices.Items.Add(attr);

        if not copyRunning then
        begin
                lstBoxDevices.ItemIndex := lstBoxDevices.Count - 1;
                lblUsbName.Caption := rsUsbUnity + dev^.node_path;
                lblUsbSize.Caption := dev^.size;
        end;
end;

procedure TMainForm.StartTimer;
begin
     timerStart := 0;
     timerElapsed.enabled := true;
     lblElapsed.Caption := '00:00:00';
end;


procedure TMainForm.TimerElapsedOn(Sender: TObject);
begin
      timerStart := IncSecond(timerStart);
      lblElapsed.Caption := formatdatetime('hh:nn:ss', timerStart);
end;



procedure TMainForm.AddItemToDevicesList(const dev: PDeviceProperty);
var
        dev_copy : PDeviceProperty;
begin
        NewListItem(dev);

        New(dev_copy);
        Move(dev^, dev_copy^, SizeOf(TDeviceProperty));

        hashDevices.Add(dev_copy^.node_path, dev_copy);

        if not copyRunning then
                ValidateActionButtons;

        if DeviceInfoForm.IsVisible then
                DeviceInfoForm.FillInformation(@hashDevices, @lstBoxDevices);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
        ret : TModalResult;
begin
        if copyRunning then
        begin
                ret := MessageDlg(rsConfirmDlgTitle, rsAskExit, mtConfirmation, mbYesNo, 0);

                if ret = mrYes then
                begin
                   copyRunning := False;
                   Device_StopCopy;
                end
                else
                   CanClose := False;
        end;
end;


procedure TMainForm.FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
        RemoveEventhandler(DeviceEventHandler);
        Device_Unitialize;
        hashDevices.ForEachCall(@FreeAllHash, nil);
        hashDevices.Free;
        DeviceInfoForm.Free;
end;


procedure TMainForm.RemoveItemFromDevicesList(const devicePath : PChar);
var
        dev : PDeviceProperty;
begin
        dev := hashDevices.Find(devicePath);

        if dev <> nil then
        begin
                hashDevices.Delete(hashDevices.IndexOf(dev));
                Dispose(dev);
        end;

        lstBoxDevices.Items.Clear;
        hashDevices.ForEachCall(@FillListDevices, nil);

        if hashDevices.count > 0 then
        begin
                if DeviceInfoForm.IsVisible then
                        DeviceInfoForm.FillInformation(@hashDevices, @lstBoxDevices);
        end
        else
        begin
                if DeviceInfoForm.IsVisible then
                        DeviceInfoForm.Close;

                lblUsbName.Caption := rsUsbUnity;
                lblUsbSize.Caption := '0 KiB';
        end;

        if not copyRunning then
                ValidateActionButtons;
 end;


{
 Event Handle Callback
}
procedure TMainForm.DeviceEvent (AData: PtrInt; AFlags: dword);
const
        ADD = 1;      //ref. device.h
        REMOVE = 2;   //ref. device.h
var
        ret : cint32;
begin
        ret := Device_ReceiveEvents;

        if ret = ADD then
                AddItemToDevicesList(Device_GetProperty)

        else if ret = REMOVE then
                RemoveItemFromDevicesList(Device_GetDevicePath);
end;


procedure TMainForm.FormCreate (Sender: TObject);
begin
        timerElapsed.enabled := false;
        copyPause := False;
        copyRunning := False;
        Caption := APP_TITLE;
        lblStatus.Caption := rsStatusWaiting;
        lblVersion.Caption := rsVersion + ' ' + APP_VER;
        DeviceInfoForm := TDeviceInfoForm.Create(Application);
        hashDevices := TFPHashList.Create;
        Device_Initialize;
        Device_ListOfDevices(@ListDevicesCallback);
        DeviceEventHandler := AddEventHandler(Device_GetMonitorEvent, 3, @DeviceEvent,0);

        isoFileName := Application.GetOptionValue('f', 'file');

        if TryOpenIsoFile(isoFileName) then
        begin
                isoInitialDir := ExtractFileDir(isoFileName);
                SetValuesOfIsoFile;
        end;
end;

function TMainForm.TryOpenIsoFile(const tryIsoFileName: string): boolean;
var
        ext : string;
        fa  : LongInt;
begin
        if tryIsoFileName = '' then
                Exit(False);

        fa := FileGetAttr(tryIsoFileName);

        if (fa <> -1) and ((fa and faDirectory) <> 0) then
                Exit(False);

        if not FileExists(tryIsoFileName) then
        begin
                MessageDlg(rsErrorDlgTitle, rsErrorFileNotExist + LineEnding + tryIsoFileName, mtError, [mbOk], 0);
                Exit(False);
        end;

        ext := ExtractFileExt(tryIsoFileName);

        if (ext <> '.iso') and (ext <> '.img') then
        begin
                 MessageDlg(rsErrorDlgTitle, rsErrorDropFile, mtError, [mbOk], 0);
                 Exit(False);
        end;

        Exit(True);
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
        const FileNames: array of String);
var
        index : integer;
begin
        if copyRunning then Exit;

        index := Low(FileNames);

        if TryOpenIsoFile(FileNames[index]) then
        begin
             isoFileName := FileNames[index];
             isoInitialDir := ExtractFileDir(isoFileName);
             SetValuesOfIsoFile;
        end;
end;


procedure TMainForm.lstBoxDevicesDblClick(Sender: TObject);
begin
        if (hashDevices.Count > 0) and not copyRunning then
        begin
                DeviceInfoForm.FillInformation(@hashDevices, @lstBoxDevices);
                DeviceInfoForm.ShowModal;
        end;
end;

procedure TMainForm.lstBoxDevicesSelectionChange(Sender: TObject; User: boolean);
var
        dev : PDeviceProperty;
begin
        if User then
        begin
                dev := hashDevices.Find(hashDevices.NameOfIndex(lstBoxDevices.ItemIndex));
                if dev <> nil then
                begin
                        lblUsbName.Caption := rsUsbUnity + dev^.node_path;
                        lblUsbSize.Caption := dev^.size;
                        ValidateActionButtons;
                end;
        end;
end;

procedure TMainForm.menuFileClick(Sender: TObject);
begin
        if copyRunning = False then
        begin
                menuFileOpenISO.Enabled := True;
                menuQuit.Enabled := True;
        end
        else
        begin
                menuFileOpenISO.Enabled := False;
                menuQuit.Enabled := False;
        end;
end;

procedure TMainForm.menuFileOpenISOClick(Sender: TObject);
begin
        btnIsoOpenClick(nil);
end;

procedure TMainForm.menuToolDeviceCloneClick(Sender: TObject);
var
        dev : PDeviceProperty;
        dlgSaveIso : TSaveDialog;
begin
        dev := hashDevices.Find(hashDevices.NameOfIndex(lstBoxDevices.ItemIndex));
        Assert(Assigned(dev));

        dlgSaveIso := TSaveDialog.Create(Self);
        dlgSaveIso.Title := rsSaveIsoFile;
        dlgSaveIso.Filename := dev^.manufacturer + '__' + dev^.size + '.iso';
        dlgSaveIso.Options:= [ofOverwritePrompt, ofNoNetworkButton];

        if dlgSaveIso.Execute then
        begin
            CloneDeviceForm := TCloneDeviceForm.Create(Self);
            CloneDeviceForm.isoFileName := dlgSaveIso.FileName;
            CloneDeviceForm.dev := dev;
            CloneDeviceForm.ShowModal;
            CloneDeviceForm.Free;
        end;
        dlgSaveIso.Free;
end;

procedure TMainForm.menuToolDeviceFormatClick(Sender: TObject);
var
   dev : PDeviceProperty;
begin
     dev := hashDevices.Find(hashDevices.NameOfIndex(lstBoxDevices.ItemIndex));
     Assert(Assigned(dev));

     FormatForm := TFormatForm.Create(Self);
     FormatForm.dev := dev;
     FormatForm.ShowModal;
     FreeAndNil(FormatForm);
end;

procedure TMainForm.menuToolISOChecksumsClick(Sender: TObject);
var
        checksumsForm : TChecksumsForm;
begin
        checksumsForm := TChecksumsForm.Create(Self);
        checksumsForm.ISOFileName := isoFileName;
        checksumsForm.ShowModal;
        checksumsForm.Free;
end;

procedure TMainForm.menuToolISOMountClick(Sender: TObject);
var
        mountForm : TMountForm;
        npartitions : cint;
begin
             mnt_iso_init(PChar(isoFileName));
             npartitions := mnt_iso_find_partitions;
             if  npartitions <= 0 then
             begin
                     MessageDlg(rsErrorDlgTitle, rsMountErrorNoPartitions, mtError, [mbOk], 0);
                     mnt_iso_free;
                     Exit;
             end;
             mountForm := TMountForm.Create(Self);
             mountForm.nPartitions := npartitions;
             mountForm.targetName := PChar(isoFileName);
             mountForm.ShowModal;
             mnt_iso_free;
end;

procedure TMainForm.menuToolsClick(Sender: TObject);
begin
        if (copyRunning = False) and (edtIsoName.Text <> '') then
        begin
                menuToolISOChecksums.Enabled := True;
                menuToolISOMount.Enabled := True;
        end
        else
        begin
                menuToolISOChecksums.Enabled := False;
                menuToolISOMount.Enabled := False;
        end;

        if (copyRunning = False) and (lstBoxDevices.Count > 0) then
        begin
                menuToolDeviceFormat.Enabled := True;
                menuToolDeviceClone.Enabled := True;
        end
        else
        begin
                menuToolDeviceFormat.Enabled := False;
                menuToolDeviceClone.Enabled := False;
        end;
end;


procedure TMainForm.menuVerClick(Sender: TObject);
begin
        if (copyRunning = False) and (lstBoxDevices.SelCount > 0) then
                menuVerInfo.Enabled := True
        else
                menuVerInfo.Enabled := False;
end;


procedure TMainForm.menuQuitClick(Sender: TObject);
begin
        Close;
end;


function TMainForm.AskCopyContinue: boolean;
var
        ret : TModalResult;
begin
        ret := MessageDlg(rsConfirmDlgTitle, rsCopyMessage, mtConfirmation, [mbYes, mbNo], 0);

        if ret = mrYes then
                Exit(True);

        Exit(False);
end;


procedure TMainForm.menuVerInfoClick (Sender: TObject);
begin
        lstBoxDevicesDblClick(nil);
end;



end.


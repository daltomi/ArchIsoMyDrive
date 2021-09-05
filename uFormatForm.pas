{
        Copyright (c) 2021 Daniel T. Borelli <danieltborelli@gmail.com>

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
unit uFormatForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, uDevice, uResources,LCLType, CTypes, Process;

type TFormatItem = class
     binPath  : string;
end;

  { TFormatForm }

type  TFormatForm = class(TForm)
    btnFormat: TBitBtn;
    btnClose: TBitBtn;
    cmbTablePartition: TComboBox;
    cmbFilesystem: TComboBox;
    Image1: TImage;
    imgList: TImageList;
    Label1: TLabel;
    lblDescription: TStaticText;
    lblStatus: TStaticText;
    procedure btnFormatClick(Sender: TObject);
    procedure cmbFilesystemChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    function  FindAllPrograms: Boolean;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddToFormatList(binName: string);
    function FormatDevice: Boolean;
  private
    FormatItems : TList;
    SfdiskBin : string;
    ShellBin : string;
  public
    dev : PDeviceProperty;
  end;

var
  FormatForm: TFormatForm;

implementation

{$R *.lfm}

{ TFormatForm }

const
     kBinPath = '/bin:/sbin/:/usr/sbin/:/usr/bin';
     kSfdisk  = 'sfdisk';
     kShell   = 'bash';
     kFat     = 'mkfs.vfat';
     kExFat   = 'mkfs.exfat';
     kExt2    = 'mkfs.ext2';
     kExt3    = 'mkfs.ext3';
     kExt4    = 'mkfs.ext4';
     kNtfs    = 'mkfs.ntfs';
     kMsdos   = 'dos';
     kGpt     = 'gpt';



procedure TFormatForm.FormActivate(Sender: TObject);
begin
   Caption := rsFormatTitle;
   btnClose.Caption := rsClose;
   btnFormat.Caption := rsFormat;

   lblDescription.Caption := Format(rsFormatDescription,
        [dev^.node_path, dev^.product, dev^.manufacturer, dev^.size]);

   cmbTablePartition.Items.Add(kMsdos);
   cmbTablePartition.Items.Add(kGpt);
   cmbTablePartition.ItemIndex := 0;

   if FindAllPrograms() = False then
      Close;
end;

procedure TFormatForm.AddToFormatList(binName: string);
var
   binPath : string;
   formatItem : TFormatItem;
begin
     binPath := ExeSearch(binName,kBinPath);
     if binPath <> '' then
     begin
        formatItem := TFormatItem.Create;
        formatItem.binPath:= binPath;
        FormatItems.Add(formatItem);
        cmbFilesystem.Items.Add(binName.Remove(0,5));
     end;
end;

function TFormatForm.FindAllPrograms: Boolean;
var
  msg : string;
begin
      SfdiskBin := ExeSearch(kSfdisk, kBinPath);
      if SfdiskBin = '' then
      begin
            msg := rsFormatErrorNoProgramFound +  SfdiskBin;
            Application.MessageBox(PChar(msg), APP_TITLE,  MB_OK + MB_ICONERROR);
            Exit(False);
      end;

      ShellBin := ExeSearch(kShell, kBinPath);
      if ShellBin = '' then
      begin
           msg := rsFormatErrorNoProgramFound +  kShell;
           Application.MessageBox(PChar(msg), APP_TITLE,  MB_OK + MB_ICONERROR);
           Exit(False)
      end;

      FormatItems := TList.Create;

      AddToFormatList(kFat);
      AddToFormatList(kExFat);
      AddToFormatList(kExt2);
      AddToFormatList(kExt3);
      AddToFormatList(kExt4);
      AddToFormatList(kNtfs);

      cmbFilesystem.ItemIndex := 0;

      if cmbFilesystem.Items.Count = 0 then
      begin
           Application.MessageBox(PChar(rsFormatErrorNoMkfs), APP_TITLE,  MB_OK + MB_ICONERROR);
           Exit(False);
      end;

      Exit(True);
end;

procedure TFormatForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i : integer;
  item : TFormatItem;
begin
     if Assigned(FormatItems) then
     begin
        for i:=FormatItems.Count-1 downto 0 do
        begin
           item := TFormatItem(FormatItems.Items[i]);
           {$ifopt D+}
           writeln(item.binPath);
           FreeAndNil(item);
           {$else}
           FreeAndNil(item);
           {$endif}
        end;
        FormatItems.Clear;
        FreeAndNil(FormatItems);
     end;
end;

procedure TFormatForm.btnFormatClick(Sender: TObject);
var
   ret : TModalResult;
begin
     ret := MessageDlg(rsConfirmDlgTitle, rsCopyMessage, mtConfirmation, [mbYes, mbNo], 0);

     if ret = mrNo then
        Exit;

     btnFormat.Enabled := False;
     btnClose.Enabled := False;
     lblStatus.Font.Color:= clDefault;
     lblStatus.Caption := rsStatusFormatting;

     if FormatDevice() = False then
     begin
        MessageDlg(rsErrorDlgTitle, rsFormatError, mtError, [mbOk], 0);
        lblStatus.Font.Color:= clRed;
        lblStatus.Caption := rsStatusError;
     end
     else
     begin
        lblStatus.Font.Color:= clGreen;
        lblStatus.Caption := rsStatusTerminate;
     end;
     btnFormat.Enabled := True;
     btnClose.Enabled := True;

end;

procedure TFormatForm.cmbFilesystemChange(Sender: TObject);
begin
    if TFormatItem(FormatItems[cmbFilesystem.ItemIndex]).binPath.Contains('ntfs') = True then
    begin
         MessageDlg(rsConfirmDlgTitle, rsFormatWarningSlow, mtWarning, [mbOk], 0);
    end;
end;


function TFormatForm.FormatDevice: boolean;
var
     Cmd : string;
     sout : string;
 begin
     if cmbTablePartition.Items[cmbTablePartition.ItemIndex] = kMsdos then
        Cmd := 'echo label:dos | ' + SfdiskBin + ' ' + dev^.node_path
     else
        Cmd := 'echo label:gpt | ' + SfdiskBin + ' ' + dev^.node_path;

     Application.ProcessMessages;
     if RunCommand(ShellBin, ['-c', cmd], sout, [poWaitOnExit]) = False then
     begin
        WriteLn('ERROR: ' + cmd);
        Exit(False);
     end;

     cmd :=  'echo ";" | ' + SfdiskBin + ' --wipe always ' + dev^.node_path;

     Application.ProcessMessages;
     if RunCommand(ShellBin, ['-c', cmd], sout, [poWaitOnExit]) = False then
     begin
        WriteLn('ERROR: ' + cmd);;
        Exit(False);
     end;

     cmd := TFormatItem(FormatItems[cmbFilesystem.ItemIndex]).binPath + ' ' + dev^.node_path + '1';

     Application.ProcessMessages;
     if RunCommand(ShellBin, ['-c', cmd], sout, [poWaitOnExit]) = False then
     begin
        WriteLn('ERROR: ' + cmd);
        Exit(False);
     end;

     Exit(True);
end;

end.


{
        Copyright (c) 2019 daltomi <daltomi@disroot.org>

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
unit uCloneDeviceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, ExtCtrls,LCLType, CTypes, uDevice, uResources;

type

        { TCloneDeviceForm }

        TCloneDeviceForm = class(TForm)
              btnCancel: TBitBtn;
              Image1: TImage;
              Label1: TLabel;
              lblDescription: TStaticText;
              lblProgress: TStaticText;
              procedure btnCancelClick(Sender: TObject);
              procedure FormActivate(Sender: TObject);
        private
               copyRunning : boolean;
               procedure RunCopyDeviceToIso;
               procedure StartCopyDeviceToIso;
        public
              isoFileName : string;
              dev : PDeviceProperty;

        end;

var
        CloneDeviceForm: TCloneDeviceForm;

implementation

{$R *.lfm}


procedure TCloneDeviceForm.StartCopyDeviceToIso;
var
        eid : cint;
begin
     eid := Device_CopyDeviceToIso(dev, PChar(isoFileName));
     if eid <> E_NO  then
     begin
          Application.MessageBox(PChar(Device_ProcessError(eid)), PChar(rsErrorDlgTitle),  MB_OK + MB_ICONERROR);
          copyRunning := False;
          Close;
          Exit;
     end;
     copyRunning := True;
     RunCopyDeviceToIso;
end;

procedure TCloneDeviceForm.btnCancelClick(Sender: TObject);
begin
     Application.ProcessMessages;
     copyRunning := False;
     Application.MessageBox(PChar(rsCloneErrorCancel), PChar(rsErrorDlgTitle), MB_OK + MB_ICONERROR);
     Close;
end;

procedure TCloneDeviceForm.FormActivate(Sender: TObject);
begin
     Caption := rsCloneTitle;
     lblDescription.Caption := Format(rsCloneDescription, [dev^.node_path, isoFilename]);
     lblProgress.Caption := rsCloneProgress + '0 %';
     if not copyRunning then
        StartCopyDeviceToIso;
end;


procedure TCloneDeviceForm.RunCopyDeviceToIso;
var
        eid : cint;
begin
     Application.ProcessMessages;
     while copyRunning  do
     begin
                eid := Device_ContinueCopyDeviceToIso;
                if  eid <> E_NO then
                begin
                     Application.MessageBox(PChar(Device_ProcessError(eid)), APP_TITLE,  MB_OK + MB_ICONERROR);
                     Device_StopCopy;
                     Close;
                     break;
                end;

                Application.ProcessMessages;
                lblProgress.Caption := rsCloneProgress + Device_GetStrPercentCopy;

                if copyRunning and (Device_GetLenCopy = 0) then
                begin
                     Hide;
                     ShowMessage(rsEndCopyOk);
                     Device_StopCopy;
                     Close;
                     break;
                end;
     end;
end;

end.



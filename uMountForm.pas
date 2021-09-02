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
unit uMountForm;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
        Buttons, StdCtrls, ExtCtrls, uMntIso,CTypes, uResources,
        FileUtil;

type

        { TMountForm }

        TMountForm = class(TForm)
                BitBtn2: TBitBtn;
                btnSave: TButton;
                Image1: TImage;
                ImageList1: TImageList;
                Label1: TLabel;
                Label2: TLabel;
                lsvPartList: TListView;
                dlgSelectDir: TSelectDirectoryDialog;
                dlgSave: TSaveDialog;
                procedure btnSaveClick(Sender: TObject);
                procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
                procedure FormCreate(Sender: TObject);
                procedure FormShow(Sender: TObject);
                procedure lsvPartListDblClick(Sender: TObject);
                function IsPartMounted: boolean;
        private
                partInfo : PPartInfo;
                mountInitialDir : string;
        public
                nPartitions  : cint;
                targetName : PChar;
        end;

var
        MountForm: TMountForm;

implementation

{$R *.lfm}

{ TMountForm }

procedure TMountForm.FormCreate(Sender: TObject);
begin
        Caption := rsMountTitle;
end;


procedure TMountForm.btnSaveClick(Sender: TObject);
var
     report : TextFile;
     i : cint;
     mid : integer;
     str : string;
begin
        if dlgSave.Execute then
        begin
                if FileExists(dlgSave.FileName) then
                begin
                        mid := MessageDlg(rsConfirmDlgTitle,
                                rsOverwriteFile, mtConfirmation, [mbYes, mbNo], 0);

                        if mid = mrNo then
                                Exit;
                end;

                try
                        AssignFile(report, dlgSave.FileName);
                        ReWrite(report);
                except
                        MessageDlg(rsErrorDlgTitle, rsMountErrorSaveReport, mtError, [mbOk], 0);
                        Exit;
                end;


                WriteLn(report, 'ISO:' + targetName + LineEnding);
                WriteLn(report, 'Part N.'#9''#9'Start'#9''#9'Size'#9''#9'UUID');

                for i := 0 to nPartitions - 1 do
                begin
                        str := Format('%d'#9''#9'%d'#9''#9'%d'#9''#9'%s' + LineEnding,
                               [partInfo[i].partno, partInfo[i].start,
                               partInfo[i].size,partInfo[i].uuid]);

                        WriteLn(report, str);
                end;
                CloseFile(report);
        end;
end;

procedure TMountForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
        if IsPartMounted then
        begin
                MessageDlg(rsErrorDlgTitle, rsMountErrorNoClose, mtError, [mbOk], 0);
                CanClose := False;
                Exit;
        end;
end;



function TMountForm.IsPartMounted: boolean;
var
     i:  cint;
begin
       for i := 0 to nPartitions - 1 do
       begin
               if partInfo[i].mounted = 1 then
                       Exit(True);
       end;
       Exit(False);
end;

procedure TMountForm.FormShow(Sender: TObject);
var
        i : cint;
        item : TListItem;
 begin
        partInfo := mnt_iso_get_list_of_partitions;

        for i := 0 to nPartitions - 1 do
        begin
                item := lsvPartList.Items.Add;
                item.Caption := IntToStr(partInfo[i].partno);
                item.ImageIndex := -1;
                item.SubItems.Add(IntToStr(partInfo[i].start));
                item.SubItems.Add(IntToStr(partInfo[i].size));
                item.SubItems.Add(partInfo[i].uuid);
         end;

        lsvPartList.Items[0].Selected := True;
end;

procedure TMountForm.lsvPartListDblClick(Sender: TObject);
var
        select : integer;
        mounted : cint;
        strerr : string;
        item : TListItem;
begin
        select := lsvPartList.Selected.Index;

        if partInfo[select].mounted = 0 then
        begin
		if IsPartMounted then
		begin
			MessageDlg(rsErrorDlgTitle, rsMountErrorMountOnlyOne, mtError, [mbOk], 0);
			Exit;
		end;

                dlgSelectDir.InitialDir := mountInitialDir;

                if dlgSelectDir.Execute then
                begin
                        mountInitialDir := dlgSelectDir.FileName;
                        mounted := mnt_iso_mount(select, partInfo, PChar(dlgSelectDir.FileName));

                        if mounted = 0 then
                        begin
                                strerr := rsMountErrorNoMount + LineEnding + mnt_iso_get_error;
                                MessageDlg(rsErrorDlgTitle, strerr, mtError, [mbOk], 0);
                        end
                        else
                        begin
                                lsvPartList.Selected.ImageIndex := 0;
                                item := lsvPartList.Items[lsvPartList.Selected.Index];
                                item.SubItems.AddText(dlgSelectDir.FileName);
                        end;
                end;
        end
        else
        begin
                mounted := mnt_iso_umount(select, partInfo);
                if mounted = 1 then
                begin
                        strerr := rsMountErrorNoUmount + LineEnding + mnt_iso_get_error;
                        MessageDlg(rsErrorDlgTitle, strerr, mtError, [mbOk], 0);
                end
                else
                begin
                        lsvPartList.Selected.ImageIndex := -1;
                        item := lsvPartList.Items[lsvPartList.Selected.Index];
                        item.SubItems.Delete(3);
                end;
        end;
end;

begin

end.


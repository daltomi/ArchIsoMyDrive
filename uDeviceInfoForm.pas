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

unit uDeviceInfoForm;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
        ExtCtrls, StdCtrls, Buttons, uDevice, Contnrs, uResources;

type

        PHashListDevices = ^TFPHashList;
        PListBox         = ^TListBox;

  { TDeviceInfoForm }

        TDeviceInfoForm = class(TForm)
                btnClose: TBitBtn;
                btnSave: TButton;
                editReadAHead: TEdit;
                editSize: TEdit;
                editPath: TEdit;
                editBus: TEdit;
                editVendor: TEdit;
                editProduct: TEdit;
                editIdProduct: TEdit;
                editManufacturer: TEdit;
                editSerial: TEdit;
                editVersion: TEdit;
                editPower: TEdit;
                Image1: TImage;
                Label1: TLabel;
                Label10: TLabel;
                Label11: TLabel;
                Label12: TLabel;
                Label2: TLabel;
                Label3: TLabel;
                Label4: TLabel;
                Label5: TLabel;
                Label6: TLabel;
                Label7: TLabel;
                Label8: TLabel;
                Label9: TLabel;
                saveDlg: TSaveDialog;
                // Obsoleto, TODO: procedure btnNavClick(Sender: TObject);
                procedure btnSaveClick(Sender: TObject);
        private
                m_device   : PDeviceProperty;
                m_hashList : PHashListDevices;
                m_listbox  : PListBox;
                procedure WriteFile (namefile: string);
                // Obsoleto, TODO: procedure ValidateNavButtons ();
        public
                procedure FillInformation(hashList : PHashListdevices; listBox : PListBox);
        end;


var
        DeviceInfoForm: TDeviceInfoForm;

implementation

{$R *.lfm}

{ TDeviceInfoForm }

procedure TDeviceInfoForm.btnSaveClick(Sender: TObject);
var
        msg : Integer;
begin
        if saveDlg.Execute then
        begin
                if FileExists(saveDlg.FileName) then
                begin
                        msg := MessageDlg(rsConfirmDlgTitle,
                                rsOverwriteFile, mtConfirmation, [mbYes, mbNo], 0);

                        if msg = mrNo then
                                Exit;
                end;
                WriteFile (saveDlg.FileName);
        end;
end;

// Obsoleto, TODO
// // Bug: cambia el index de lstBoxDevices.
{
procedure TDeviceInfoForm.btnNavClick(Sender: TObject);
var
        index : Word; //unsigned
begin
        index := m_listbox^.ItemIndex;

        if Sender = btnPrev then
                index := index - 1
        else
                index := index + 1;

        m_listbox^.ItemIndex := index;
        FillInformation (nil, nil);
end;
}

{sobreescribe el archivo}
procedure TDeviceInfoForm.WriteFile (namefile: string);
var
        tfile : TextFile;

begin
        AssignFile(tfile, namefile);
        rewrite(tfile);

        with m_device^ do
        begin
                writeln(tfile, 'Node       : ' + node_path);
                writeln(tfile, 'Vendor     : ' + id_vendor);
                writeln(tfile, 'Product    : ' + product);
                writeln(tfile, 'ID Product : ' + id_product);
                writeln(tfile, 'Manufacturer: ' + manufacturer);
                writeln(tfile, 'Serial     : ' + serial);
                writeln(tfile, 'Version    : ' + version);
                writeln(tfile, 'Power max. : ' + max_power);
                writeln(tfile, 'Bus        : ' + bus);
                writeln(tfile, 'ReadAHead   : ' + IntToStr(readahead));
                writeln(tfile, 'Size       : ' + size);
        end;
        CloseFile(tfile);
end;


// Obsoleto, TODO:
{
procedure TDeviceInfoForm.ValidateNavButtons ();
begin
        if m_listbox^.Count = 1 then
        begin
                btnNext.Enabled := false;
                btnPrev.Enabled := false;
                exit;
        end;

        if m_listbox^.ItemIndex < m_listbox^.Count -1 then
                btnNext.Enabled := true
        else
                btnNext.Enabled := false;

        if m_listbox^.ItemIndex > Int(0) then
                btnPrev.Enabled := true
        else
                btnPrev.Enabled := false;
end;
}


procedure TDeviceInfoForm.FillInformation(hashList : PHashListdevices; listBox : PListBox);
var
        dev : PDeviceProperty;
begin
        if m_listbox = nil then m_listbox := listBox;
        if m_hashList = nil then m_hashList := hashList;

        //ValidateNavButtons ();

        { supone que el indice del hash coincide al mostrado en el editbox }
        dev := m_hashList^.Find(m_hashList^.NameOfIndex(m_listbox^.ItemIndex));

        if dev = nil then exit;

        with dev^ do
        begin
                editPath.Text := node_path;
                editVendor.Text := id_vendor;
                editProduct.Text := product;
                editIdProduct.Text := id_product;
                editManufacturer.Text := manufacturer;
                editSerial.Text := serial;
                editVersion.Text := version;
                editPower.Text := max_power;
                editBus.Text := bus;
                editReadAHead.Text := IntToStr(readahead);
                editSize.Text := size;
        end;
        m_device := dev;
end;


end.


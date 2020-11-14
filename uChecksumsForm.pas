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

unit uChecksumsForm;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
        Buttons, ExtCtrls, uResources, uRHash, CTypes;

type

        { TChecksumsForm }

        TChecksumsForm = class(TForm)
                btnClose: TBitBtn;
                btnCalculate: TButton;
                cbxChecksums: TComboBox;
                edtCompare: TEdit;
                edtHash: TEdit;
                Image1: TImage;
                Label1: TLabel;
                Label2: TLabel;
                Label3: TLabel;
                Label4: TLabel;
                Label5: TLabel;
                lblMatches: TLabel;
                lblIsoName: TLabel;
                procedure btnCalculateClick(Sender: TObject);
                procedure edtCompareChange(Sender: TObject);
                procedure FormCreate(Sender: TObject);
        private
                fileName : string;
                procedure SetFileName(const aFileName : string);
        public
                property ISOFileName : string write SetFileName;
        end;

var
        ChecksumsForm: TChecksumsForm;

implementation

{$R *.lfm}

{ TChecksumsForm }
procedure TChecksumsForm.SetFileName(const aFileName : string);
begin
        fileName := aFileName;
        lblIsoName.Caption := 'ISO :' + ExtractFileName(aFileName);
end;

procedure TChecksumsForm.btnCalculateClick(Sender: TObject);
const
        MD5     = 0;
        SHA1    = 1;
        SHA256  = 2;
        SHA384  = 3;
        SHA512  = 4;
var
        id      : cuint;
        hex     : string;
        i       : integer;
        digest  : array[0..127] of cuchar;
        ndigest : integer;
        ret     : cint;
begin
        Enabled := False;
        Application.ProcessMessages();

        case cbxChecksums.ItemIndex of
                MD5    : begin
                         id := RHASH_MD5;
                         ndigest := 16; // 128 bits
                end;
                SHA1   : begin
                         id := RHASH_SHA1;
                         ndigest := 20; // 160 bits
                end;
                SHA256 : begin
                         id := RHASH_SHA256;
                         ndigest := 32; // 256 bits
                end;
                SHA384 : begin
                         id := RHASH_SHA384;
                         ndigest := 48; // 384 bits
                end;
                SHA512 : begin
                         id := RHASH_SHA512;
                         ndigest := 64; // 512 bits
                end;
        end;

        ret := rhash_file(id, PChar(fileName), @digest);

        assert(ret = 0);

        hex := '';
        for i:= 0 to ndigest - 1 do
                hex := hex + LowerCase(IntToHex(digest[i], 2));

        edtHash.Text := hex.Substring(0, ndigest * 2);
        Enabled := True;
        edtCompareChange(nil);
end;

procedure TChecksumsForm.edtCompareChange(Sender: TObject);
begin
        if (edtCompare.Text <> '') and (edtHash.Text <> '') then
        begin
              edtHash.Text := Trim(edtHash.Text);
              if edtCompare.Text = edtHash.Text then
              begin
                      lblMatches.Caption := rsChecksumMatch;
                      lblMatches.Font.Color := clGreen;
              end
              else
              begin
                      lblMatches.Caption := rsChecksumNotMatch;
                      lblMatches.Font.Color := clRed;
              end;
        end
end;

procedure TChecksumsForm.FormCreate(Sender: TObject);
begin
        cbxChecksums.ItemIndex := 2;
end;


end.


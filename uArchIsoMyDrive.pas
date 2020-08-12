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
program ArchIsoMyDrive;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, Forms, uMainForm, Process, LCLType, BaseUnix,
  uResources, SysUtils, LCLTranslator, uMntIso;

{$R *.res}


// Unix.
// Si bien existen paquetes de Lazarus para detectar única instancia,
// èste proyecto no es multiplataforma.
function IsAppRunning: boolean;
const
        CMD = 'ps -C ' + APP_BIN + ' | grep ' + APP_BIN + ' | wc -l';
        BASH1 = '/usr/bin/bash';
        BASH2 = '/bin/bash';
var
        str : String;
begin
        if RunCommand(BASH1, ['-c', CMD], str) = False then
        begin
                RunCommand(BASH2, ['-c', CMD], str);
        end;

        exit(str[1] <> '1');
end;

// Unix.
function IsUserRoot: boolean;
begin
        exit(fpGetEUid = 0);
end;


begin
        RequireDerivedFormResource := True;

        SetDefaultLang('', '/usr/share/ArchIsoMyDrive/locale', False);

        if Application.HasOption('v', 'version') then
        begin
                WriteLn(APP_TITLE_VER);
                Exit;
        end;

        if IsAppRunning then
        begin
                Application.MessageBox(PChar(rsErrUniqueInstance), PChar(rsErrorDlgTitle), MB_OK + MB_ICONWARNING);
                Exit;
        end;

        if not IsUserRoot then
        begin
                Application.MessageBox(PChar(rsErrNotRoot), PChar(rsErrorDlgTitle), MB_OK + MB_ICONERROR);
                Exit;
        end;

        Application.Title := APP_TITLE;
        Application.Initialize;
        Application.CreateForm(TMainForm, MainForm);
        Application.Run;
end.


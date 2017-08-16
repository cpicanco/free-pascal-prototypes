{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit report_abstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTabDelimitedReport }

  TTabDelimitedReport = class(TComponent)
  private
    FFilename : string;
    FTextFile : TextFile;
    procedure SetFilename(AFilename: string);
  protected
    procedure CloseFile;
    procedure WriteLine(Args : array of string);
  public
    procedure WriteFooter; virtual; abstract;
    procedure WriteHeader; virtual; abstract;
    procedure NextFile;
    property Filename : string read FFilename write SetFilename;
  end;

resourcestring
  RSBegin = 'Início:';
  RSEnd = 'Fim:';

implementation

uses character;

const
  TAB = #9;

procedure TTabDelimitedReport.NextFile;
begin
  SetFilename(FFilename);
end;

procedure TTabDelimitedReport.CloseFile;
begin
  System.Close(FTextFile);
end;

// About the 'array of const' argument:
// https://www.freepascal.org/docs-html/ref/refsu69.html
procedure TTabDelimitedReport.WriteLine(Args: array of string);
var
  i : LongInt;
  LLastArg : LongInt;
begin
  LLastArg := High(Args);
  for i := 0 to LLastArg do
    begin
      // write an item of the Args array
      Write(FTextFile, Args[i]);

      // then write a tab charater or LineEnding
      if i < LLastArg then Write(FTextFile, TAB) else Write(FTextFile, LineEnding);
    end;
end;

procedure TTabDelimitedReport.SetFilename(AFilename: string);
var
  LFilePath, LExtension, LBasename: string;
  LChar : char;
  i : Integer;
begin
  LFilePath := ExtractFilePath(AFilename);
  LExtension := ExtractFileExt(AFilename);
  LBasename := ExtractFileName(AFilename);
  LBasename:= LBasename[1..High(LBasename)-4];

  // remove numbers from filename
  for LChar in LBasename do
    if IsNumber(LChar) then Delete(LBasename, Pos(LChar, LBasename), 1);

  // never override an existing file
  i := 0;
  while FileExists(AFilename) do
    begin
      Inc(i);
      AFilename := LFilePath + LBasename + Format('%.3d', [i]) + LExtension;
    end;

  // assign a name to the file
  AssignFile(FTextFile, AFilename);

  // create a file for writing
  Rewrite(FTextFile);

  // store the filename in memory for further use
  FFilename:=AFilename;
end;

end.


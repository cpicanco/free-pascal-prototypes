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
    procedure SetFilename(AFilename: string);
  protected
    FTextFile : TextFile;
    procedure WriteLine(Args : array of const);
  public
    procedure AddFooter; virtual;
    procedure AddHeader; virtual;
    procedure AddLine; virtual;
    procedure NextFile;
    property Filename : string read FFilename write SetFilename;
  end;

resourcestring
  RSBegin = 'Início:';
  RSEnd = 'Fim:';

implementation

const
  TAB = #9;

procedure TTabDelimitedReport.AddFooter;
begin
  WriteLine([RSEnd, TimeToStr(Now)]);
end;

procedure TTabDelimitedReport.AddHeader;
begin
  WriteLine([RSBegin, TimeToStr(Now)]);
end;

procedure TTabDelimitedReport.AddLine;
begin
  AbstractError;
end;

procedure TTabDelimitedReport.NextFile;
begin
  Filename:= FFilename;
end;

// About the 'array of const' argument:
// https://www.freepascal.org/docs-html/ref/refsu69.html
procedure TTabDelimitedReport.WriteLine(Args: array of const);
var
  i : LongInt;
  LLastArg : LongInt;
begin
  LLastArg := High(Args);
  for i := 0 to LLastArg do
    begin
      // write an item of the Args array
      case Args[i].vtype of
        vtinteger    : Write(FTextFile, Args[i].vinteger);
        vtboolean    : Write(FTextFile, Args[i].vboolean);
        vtchar       : Write(FTextFile, Args[i].vchar);
        vtextended   : Write(FTextFile, Args[i].VExtended^);
        vtString     : Write(FTextFile, Args[i].VString^);
        vtPointer    : Write(FTextFile, LongInt(Args[i].VPointer));
        vtPChar      : Write(FTextFile, Args[i].VPChar);
        vtObject     : Write(FTextFile, Args[i].VObject.Classname);
        vtClass      : Write(FTextFile, Args[i].VClass.Classname);
        vtAnsiString : Write(FTextFile, AnsiString(Args[I].VAnsiString));
      else
          Write(FTextFile, 'Unknown Type:', args[i].vtype);
      end;

      // then write a tab charater or LineEnding
      if i < LLastArg then
        Write(FTextFile, TAB)
      else
        Write(FTextFile, LineEnding);
    end;
end;

procedure TTabDelimitedReport.SetFilename(AFilename: string);
var
  LFilePath, LExtension: RawByteString;
  i : Integer;
begin
  // never override an exinting file
  LFilePath := ExtractFilePath(AFilename);
  LExtension := ExtractFileExt(AFilename);
  i := 0;
  while FileExists(AFilename) do
    begin
      Inc(i);
      AFilename := LFilePath + StringOfChar(#48, 3 - Length(IntToStr(i))) + IntToStr(i) + LExtension;
    end;

  // assign a name to the file
  AssignFile(FTextFile, AFilename);

  // create a file for writing
  Rewrite(FTextFile);

  // store the filename in memory for further use
  FFilename:=AFilename;
end;

end.


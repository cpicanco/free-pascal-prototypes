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
    procedure WriteLine(Args : array of const); overload;
  public
    procedure WriteFooter; virtual;
    procedure WriteHeader; virtual;
    procedure WriteLine; overload; virtual; abstract;
    procedure NextFile;
    property Filename : string read FFilename write SetFilename;
  end;

resourcestring
  RSBegin = 'Início:';
  RSEnd = 'Fim:';

implementation

uses Variants;

const
  TAB = #9;

procedure TTabDelimitedReport.WriteFooter;
begin
  WriteLine([RSEnd, TimeToStr(Now)]);
  CloseFile(FTextFile);
end;

procedure TTabDelimitedReport.WriteHeader;
begin
  WriteLine([RSBegin, TimeToStr(Now)]);
end;

procedure TTabDelimitedReport.NextFile;
begin
  SetFilename(FFilename);
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
        vtinteger       : Write(FTextFile, Args[i].VInteger);
        vtboolean       : Write(FTextFile, Args[i].VBoolean);
        vtchar          : Write(FTextFile, Args[i].VChar);
        vtextended      : Write(FTextFile, Args[i].VExtended^);
        vtString        : Write(FTextFile, Args[i].VString^);
        vtPointer       : Write(FTextFile, Format('[0x%p]',[Args[i].VPointer]));
        vtPChar         : Write(FTextFile, Args[i].VPChar);
        vtObject        : Write(FTextFile, Args[i].VObject.Classname);
        vtClass         : Write(FTextFile, Args[i].VClass.Classname);
        vtWideChar      : Write(FTextFile, Args[i].VWideChar);
        vtPWideChar     : Write(FTextFile, Args[i].VPWideChar^);
        vtAnsiString    : Write(FTextFile, AnsiString(Args[I].VAnsiString));
        vtCurrency      : Write(FTextFile, CurrToStr(Args[i].VCurrency^));
        vtVariant       : Write(FTextFile, VarToStr(Args[i].VVariant^));
        vtInterface     : Write(FTextFile, Format('[0x%p]',[Args[i].VInterface]));
        vtWideString    : Write(FTextFile, WideString(Args[i].VWideString));
        vtInt64         : Write(FTextFile, Args[i].VInt64^);
        vtQWord         : Write(FTextFile, Args[i].VQWord^);
        vtUnicodeString : Write(FTextFile, UnicodeString(Args[i].VUnicodeString));
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


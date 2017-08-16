{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit report_custom;   // unit name. See: http://wiki.freepascal.org/Unit

{$mode objfpc}        // compiler directive. See: https://www.freepascal.org/docs-html/prog/progse75.html#x290-306000D.4
{$H+}                 // compiler directive. See: http://wiki.freepascal.org/String

interface             // public to units that specify this unit in their uses clause

uses                  // other units used by this unit
  Classes, SysUtils, report_abstract;

type                  // Lets define some custom types

  {
    TCounters - a class type implicitly inherited from TObject
    See: http://wiki.freepascal.org/Class
  }
  TCounters = class
    Trials, Hits, Misses : LongInt;      // implicit public fields of the class
  end;

  {
    TReport - a class type inherited from TTabDelimitedReport
  }
  TReport = class sealed (TTabDelimitedReport)
  private             // private fields
    FCounters: TCounters;
    FStartTime: LongWord;
  public              // explicit public methods
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure WriteFooter; override;
    procedure WriteHeader; override;
    procedure WriteLine(AEvent : string; AX: integer = -1; AY : integer = -1); overload;
    property Counters : TCounters read FCounters write FCounters;
    property StartTime : LongWord read FStartTime write FStartTime;
  end;

{ resource strings are used to centralize text
  to be displayed and that may be translated }
resourcestring
  // values of the report
  RSHit = 'ACERTO';
  RSMiss = 'ERRO';

  // header, column names of the report
  RSTrial = 'Tentativa';
  RSEvent = 'Evento';
  RSMouseX = 'MouseX';
  RSMouseY = 'MouseY';
  RSTime = 'Tempo(ms)';

  // footer
  RSHits = 'Acertos:';
  RSMisses = 'Erros:';

implementation

{ TReport }

constructor TReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCounters := TCounters.Create;
end;

destructor TReport.Destroy;
begin
  FCounters.Free;
  inherited Destroy;
end;

procedure TReport.WriteFooter;
begin
  WriteLine([RSEnd, TimeToStr(Now)]);
  WriteLine([RSHits, IntToStr(FCounters.Hits)]);
  WriteLine([RSMisses, IntToStr(FCounters.Misses)]);
  CloseFile;
end;

procedure TReport.WriteHeader;
begin
  WriteLine([RSBegin, TimeToStr(Now)]);
  WriteLine([RSTime, RSTrial, RSEvent, RSMouseX, RSMouseY]);
end;

procedure TReport.WriteLine(AEvent: string; AX: integer; AY: integer);
var
  LX : string = 'NA';
  LY : string = 'NA';
  LTime : string = '';
  LTrial : string = '';
begin
  if AX = -1 then { do nothing } else WriteStr(LX, AX);
  if AY = -1 then { do nothing } else WriteStr(LX, AY);
  WriteStr(LTime, GetTickCount64-FStartTime);
  WriteStr(LTrial, FCounters.Trials+1);
  WriteLine([LTime, LTrial, AEvent, LX, LY]);
end;

end.


{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit TabDelimitedReport.Custom;   // unit name. See: http://wiki.freepascal.org/Unit

{$mode objfpc}        // compiler directive. See: https://www.freepascal.org/docs-html/prog/progse75.html#x290-306000D.4
{$H+}                 // compiler directive. See: http://wiki.freepascal.org/String

interface             // public to units that specify this unit in their uses clause

uses                  // other units used by this unit
  Classes, SysUtils, TabDelimitedReport;

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
  TCustomReport = class sealed (TTabDelimitedReport)
  private             // private fields
    FCounters: TCounters;
    FStartTime: LongWord;
  public              // explicit public methods
    constructor Create(AOwner : TComponent); reintroduce;
    destructor Destroy; override;
    procedure WriteFooter;
    procedure WriteHeader;
    procedure WriteRow(AEvent : string; AX: integer = -1; AY : integer = -1); overload;
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
  RSBegin = 'Início:';
  RSEnd = 'Final:';

implementation

{ TReport }

constructor TCustomReport.Create(AOwner: TComponent);
begin
  FCounters := TCounters.Create;
end;

destructor TCustomReport.Destroy;
begin
  FCounters.Free;
  inherited Destroy;
end;

procedure TCustomReport.WriteFooter;
begin
  WriteRow([RSEnd, TimeToStr(Now)]);
  WriteRow([RSHits, IntToStr(FCounters.Hits)]);
  WriteRow([RSMisses, IntToStr(FCounters.Misses)]);
  CloseFile;
end;

procedure TCustomReport.WriteHeader;
begin
  WriteRow([RSBegin, TimeToStr(Now)]);
  WriteRow([RSTime, RSTrial, RSEvent, RSMouseX, RSMouseY]);
end;

procedure TCustomReport.WriteRow(AEvent: string; AX: integer; AY: integer);
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
  WriteRow([LTime, LTrial, AEvent, LX, LY]);
end;

end.


{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit report_custom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , report_abstract
  ;

type

  { TCounters }

  TCounters = class
    Trials, Hits, Misses : LongInt;
  end;

  { TReport }

  TReport = class(TTabDelimitedReport)
  private
    FCounters: TCounters;
    FStartTime: LongWord;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddHeader; override;
    procedure AddFooter; override;
    procedure AddLine(AEvent : string; AX: integer = -1; AY : integer = -1); reintroduce;
    property Counters : TCounters read FCounters write FCounters;
    property StartTime : LongWord read FStartTime write FStartTime;
  end;

resourcestring
  // Target Values
  RSHit = 'ACERTO';
  RSMiss = 'ERRO';

  // header
  RSTrial = 'Tentativa';
  RSEvent = 'Evento';
  RSMouseX = 'MouseX';
  RSMouseY = 'MouseY';
  RSTime = 'Tempo(ms)';

  // footer
  RSHits = 'Acertos:';
  RSMisses = 'Erros:';

implementation

const
  TAB = #9;

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

procedure TReport.AddHeader;
begin
  inherited AddHeader;
  WriteLine([RSTime, RSTrial, RSEvent, RSMouseX, RSMouseY]);
end;

procedure TReport.AddFooter;
begin
  inherited AddFooter;
  WriteLine([RSHits, FCounters.Hits]);
  WriteLine([RSMisses, FCounters.Misses]);
  CloseFile(FTextFile);
end;

procedure TReport.AddLine(AEvent: string; AX: integer; AY: integer);
var
  LTime: LongInt;
  LTrial : LongInt;
begin
  LTime := GetTickCount64-FStartTime;
  LTrial := FCounters.Trials+1;
  if (AX = -1) and (AY = -1) then
    WriteLine([LTime, LTrial, AEvent, 'NA', 'NA'])
  else
    WriteLine([LTime, LTrial, AEvent, AX, AY]);
end;

end.


{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit simple_session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls
  , stimuli_grid
  , report_custom
  ;

type

  { TSession }

  TSession = class(TComponent)
  private
    FInterTrialInterval : TTimer;
    FCounters : TCounters;
    FReport : TReport;
    FGrid : TKeyGrid;
    procedure InterTrialIntervalStartTimer(Sender: TObject);
    procedure InterTrialIntervalStopTimer(Sender: TObject);
    procedure InterTrialIntervalOnTimer(Sender: TObject);
    procedure KeyGridConsequence(Sender: TObject);
    procedure KeyGridResponse(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Stop;
    procedure EndTrial;
  private
    FOnEndTrial: TNotifyEvent;
    FOnGridConsequence: TNotifyEvent;
    FOnGridResponse: TMouseEvent;
    FOnStop: TNotifyEvent;
    procedure SetOnEndTrial(AValue: TNotifyEvent);
    procedure SetOnGridConsequence(AValue: TNotifyEvent);
    procedure SetOnGridResponse(AValue: TMouseEvent);
    procedure SetOnStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Start(ABackGround:TWinControl; AFilename : string; AMatrixValue : integer);
  public
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write SetOnEndTrial;
    property OnGridResponse : TMouseEvent read FOnGridResponse write SetOnGridResponse;
    property OnGridConsequence : TNotifyEvent read FOnGridConsequence write SetOnGridConsequence;
  end;


resourcestring
  RSSessionEnd = 'A sessão terminou. O relatório foi salvo em:';
  RSHitMessage = 'Este é um estímulo correto, parabéns.';
  RSMissMessage = 'Este é um estímulo incorreto, tente novamente.';
  RSBackgroundEvent = 'Fundo';

implementation

uses Dialogs, stimulus_key;

{ TSession }

procedure TSession.InterTrialIntervalStartTimer(Sender: TObject);
begin
  FGrid.Hide;
end;

procedure TSession.InterTrialIntervalStopTimer(Sender: TObject);
begin
  EndTrial;
end;

procedure TSession.InterTrialIntervalOnTimer(Sender: TObject);
begin
  FInterTrialInterval.Enabled:=False;
end;

procedure TSession.KeyGridConsequence(Sender: TObject);
begin
  case TKey(Sender).Caption of
    '1', '2' :
      begin
        ShowMessage(RSHitMessage);
        FReport.WriteLine(RSHit);
        Inc(FReport.Counters.Hits);
      end
    else
      begin
        ShowMessage(RSMissMessage);
        FReport.WriteLine(RSMiss);
        Inc(FReport.Counters.Misses);
      end;
  end;
  FInterTrialInterval.Enabled := True;
end;

procedure TSession.KeyGridResponse(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TKey then
    FReport.WriteLine(TKey(Sender).Caption, X, Y);

  if Sender is TKeyGrid then
    FReport.WriteLine(RSBackgroundEvent, X, Y);

  WriteLn(x, ' ',y);
end;

procedure TSession.Stop;
begin
  FReport.WriteFooter;
  FGrid.DestroyExampleKeys;
  ShowMessage(RSSessionEnd+FReport.Filename);
  if Assigned(OnStop) then OnStop(Self);
end;

procedure TSession.EndTrial;
begin
  if FCounters.Trials < 9 then
    begin
      Inc(FCounters.Trials);
      FGrid.RandomizeKeyPositions;
      FGrid.Show;
      FReport.WriteLine(FGrid.AsString);
      if Assigned(OnEndTrial) then OnEndTrial(Self);
    end
  else
    Stop;
end;

procedure TSession.SetOnEndTrial(AValue: TNotifyEvent);
begin
  if FOnEndTrial=AValue then Exit;
  FOnEndTrial:=AValue;
end;

procedure TSession.SetOnGridConsequence(AValue: TNotifyEvent);
begin
  if FOnGridConsequence=AValue then Exit;
  FOnGridConsequence:=AValue;
end;

procedure TSession.SetOnGridResponse(AValue: TMouseEvent);
begin
  if FOnGridResponse=AValue then Exit;
  FOnGridResponse:=AValue;
end;

procedure TSession.SetOnStop(AValue: TNotifyEvent);
begin
  if FOnStop=AValue then Exit;
  FOnStop:=AValue;
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGrid := TKeyGrid.Create(Self);
  FReport := TReport.Create(Self);
  FCounters := FReport.Counters;
  FInterTrialInterval := TTimer.Create(Self);
  FInterTrialInterval.Enabled := False;
  FInterTrialInterval.Interval := 1000;
  FInterTrialInterval.OnStartTimer := @InterTrialIntervalStartTimer;
  FInterTrialInterval.OnStopTimer := @InterTrialIntervalStopTimer;
  FInterTrialInterval.OnTimer := @InterTrialIntervalOnTimer;
end;

procedure TSession.Start(ABackGround: TWinControl; AFilename: string; AMatrixValue : integer);
begin
  FCounters.Trials := 0;
  FGrid.Parent := ABackGround;
  FGrid.OnConsequence:=@KeyGridConsequence;
  FGrid.OnResponse:=@KeyGridResponse;
  FGrid.CreateExampleKeys(AMatrixValue,AMatrixValue);
  FGrid.RandomizeKeyPositions;
  FGrid.Show;

  FReport.Filename := AFilename;
  FReport.StartTime:=GetTickCount64;
  FReport.WriteHeader;
  FReport.WriteLine(FGrid.AsString);
end;

end.


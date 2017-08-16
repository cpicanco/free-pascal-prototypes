unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, SerialTimer;

type

  { TForm1 }

  TStimulusKind = (stmA, stmB, stmC);

  TStimulus = record
    Data : string;
    SType : TStimulusKind;
  end;

  TStimuliList = array of TStimulus;

  TForm1 = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FNextOffset : Cardinal;
    FStart : Cardinal;
    FCycles : Cardinal;
    FSerialTimer : TSerialTimer;
    FStimulusList : TStimuliList;
    function LogTime : Cardinal;
    procedure TimerOnTimerOffset(Sender: TObject);
    procedure TimerOnTimerStimulusShow(Sender: TObject);
    procedure TimerOnTimerStimulusHideReinforcementShow(Sender: TObject);
    procedure TimerOnTimerReinforcementHide(Sender: TObject);
    procedure TimerOnTimerStimulusHide(Sender: TObject);
    procedure EndTimeSerie(Sender: TObject);
    procedure GenerateStimuliList;
    procedure SetTimeSerie;
    procedure SetTimeSerie1;
    procedure SetTimeSerie2;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math;

const
  LMAX_CYCLE_TIME = 60;
  LMIN_OFFSET = 15;
  LMAX_STIMULUS_ONSET = 30;
  LSTIMULUS_DURATION = 5;
  LREINFORCEMENT_DURATION = 10;

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
begin
  FSerialTimer.Start;
  FStart := GetTickCount64;
  WriteLn('FSerialTimer.Start');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSerialTimer := TSerialTimer.Create(Self);
  GenerateStimuliList;
  FSerialTimer.OnEndTimeSerie := @EndTimeSerie;
end;

function TForm1.LogTime: Cardinal;
begin
  Result := Round((GetTickCount64 - FStart)/1000); // miliseconds to seconds
end;

procedure TForm1.TimerOnTimerOffset(Sender: TObject);
begin
  WriteLn('TimerOnTimerOffset:', LogTime,' seconds');
end;

procedure TForm1.TimerOnTimerStimulusShow(Sender: TObject);
begin
  WriteLn('Show:'+FStimulusList[FCycles].Data,#32, LogTime,' seconds');
end;

procedure TForm1.TimerOnTimerStimulusHideReinforcementShow(Sender: TObject);
begin
  WriteLn('Hide:'+FStimulusList[FCycles].Data,#32, LogTime,' seconds');
  WriteLn('Show:'+'*',#32, LogTime,' seconds');
end;

procedure TForm1.TimerOnTimerReinforcementHide(Sender: TObject);
begin
  WriteLn('Hide:'+'*',#32, LogTime,' seconds');
end;

procedure TForm1.TimerOnTimerStimulusHide(Sender: TObject);
begin
  WriteLn('Hide:'+FStimulusList[FCycles].Data,#32, LogTime,' seconds');
end;

procedure TForm1.EndTimeSerie(Sender: TObject);
begin
  WriteLn('EndTimeSerie',#32, LogTime,' seconds');
  if FCycles < High(FStimulusList) then
    begin
      Inc(FCycles);
      SetTimeSerie;
      FSerialTimer.Start;
    end
  else
    WriteLn('End Session.',#32, LogTime,' seconds');
end;

procedure TForm1.GenerateStimuliList;
const
  LHighList = 4;
var
  LListSize: Integer;
  LStimulus : TStimulus;
  LStrings : array [Ord(Low(TStimulusKind))..Ord(High(TStimulusKind)), 0..LHighList] of string =
    (('ba', 'be', 'bi', 'bo', 'bu'),
     ('da', 'de', 'di', 'do', 'du'),
     ('ba', 'be', 'ca', 'ce', 'da'));
  i , j, l: integer;
begin
  // create list based on some constants
  LListSize := (Ord(High(TStimulusKind))+1)*(LHighList+1);
  SetLength(FStimulusList, LListSize);
  l := 0;
  for i := Low(LStrings) to High(LStrings) do
    for j := Low(LStrings[i]) to High(LStrings[i]) do
      begin
        LStimulus.Data := LStrings[i][j];
        case i of
          Ord(stmA): LStimulus.SType := stmA;
          Ord(stmB): LStimulus.SType := stmB;
          Ord(stmC): LStimulus.SType := stmC;
        end;
        FStimulusList[l] := LStimulus;
        Inc(l);
      end;

  // randomize list
  for i := Low(FStimulusList) to High(FStimulusList) do
    begin
      j := Random(Length(FStimulusList));
      LStimulus := FStimulusList[i];
      FStimulusList[i] := FStimulusList[j];
      FStimulusList[j] := LStimulus;
    end;

  SetTimeSerie;
end;

procedure TForm1.SetTimeSerie;
begin
  case FStimulusList[FCycles].SType of
    stmA, stmB : SetTimeSerie1;
    stmC : SetTimeSerie2;
  end;
end;

function SecondsToMilliseconds(AValue: Cardinal) : Cardinal;
var
  LBaseTime : Cardinal = 1000;
begin
  Result := AValue*LBaseTime;
end;

function RandomStimulusOnset : Cardinal;
begin
  Result := RandomRange(0, LMAX_STIMULUS_ONSET+1) + LMIN_OFFSET;
  //Result := LMIN_OFFSET;
end;

function NextOffset(AMaxCycleTime, AStimulusOnset, AStimulusDuration : Cardinal;
  AReinforcementDuration : Cardinal = 0) : Cardinal;
var
  LTime : Cardinal;
begin
  LTime := AMaxCycleTime-(AStimulusOnset+AStimulusDuration+AReinforcementDuration);
  Assert(0 <= LTime);
  Assert(LTime <= AMaxCycleTime);
  Result := LTime;
end;

procedure TForm1.SetTimeSerie1;
var
  LTimerItems : TTimerItems;
  LStimulusOnset : Cardinal;
begin
  LStimulusOnset := RandomStimulusOnset;
  SetLength(LTimerItems, 4);

  LTimerItems[0].Interval := SecondsToMilliseconds(LStimulusOnset);
  LTimerItems[0].OnTimerEvent := @TimerOnTimerStimulusShow;

  LTimerItems[1].Interval := SecondsToMilliseconds(LSTIMULUS_DURATION);
  LTimerItems[1].OnTimerEvent := @TimerOnTimerStimulusHideReinforcementShow;

  LTimerItems[2].Interval := SecondsToMilliseconds(LREINFORCEMENT_DURATION);
  LTimerItems[2].OnTimerEvent := @TimerOnTimerReinforcementHide;

  LTimerItems[3].Interval := SecondsToMilliseconds(
    NextOffset(LMAX_CYCLE_TIME, LStimulusOnset, LSTIMULUS_DURATION, LREINFORCEMENT_DURATION)
  );
  LTimerItems[3].OnTimerEvent := @TimerOnTimerOffset;

  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

procedure TForm1.SetTimeSerie2;
var
  LTimerItems : TTimerItems;
  LStimulusOnset : Cardinal;
  LOffset : Cardinal;
begin
  LStimulusOnset := RandomStimulusOnset;
  SetLength(LTimerItems, 3);

  LTimerItems[0].Interval := SecondsToMilliseconds(LStimulusOnset);
  LTimerItems[0].OnTimerEvent := @TimerOnTimerStimulusShow;

  LTimerItems[1].Interval := SecondsToMilliseconds(LSTIMULUS_DURATION);
  LTimerItems[1].OnTimerEvent := @TimerOnTimerStimulusHide;

  LTimerItems[2].Interval := SecondsToMilliseconds(
    NextOffset(LMAX_CYCLE_TIME, LStimulusOnset, LSTIMULUS_DURATION)
  );
  LTimerItems[2].OnTimerEvent := @TimerOnTimerOffset;

  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

initialization
  Randomize;

end.


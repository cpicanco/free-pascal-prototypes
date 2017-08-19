{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.StimulusGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls
  , Controls.StimulusKey
  ;

type

  TKeyMatrix = array of array of TStimulusKey;

  { TKeyGrid }

  TKeyGrid = class(TWinControl)
    private
      FKeys: TKeyMatrix;
      FCols : integer;
      FRows : integer;
      procedure GridResponse(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
    private
      FOnKeyConsequence: TNotifyEvent;
      FOnKeyResponse: TMouseEvent;
      procedure SetOnKeyConsequence(AValue: TNotifyEvent);
      procedure SetOnKeyResponse(AValue: TMouseEvent);
    protected
      procedure Resize; override;
    public
      constructor Create(AOwner:TComponent); override;
      function AsString: string;
      procedure CreateExampleKeys(AColCount, ARowCount: integer);
      procedure DestroyExampleKeys;
      procedure RandomizeKeyPositions;
    public
      property OnConsequence : TNotifyEvent read FOnKeyConsequence write SetOnKeyConsequence;
      property OnResponse : TMouseEvent read FOnKeyResponse write SetOnKeyResponse;
  end;

implementation

uses math;

{ TKeyGrid }

procedure TKeyGrid.GridResponse(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnResponse) then
    OnResponse(Sender,Button, Shift, X + Left, Y + Top);
end;

procedure TKeyGrid.SetOnKeyConsequence(AValue: TNotifyEvent);
begin
  if FOnKeyConsequence=AValue then Exit;
  FOnKeyConsequence:=AValue;
end;

procedure TKeyGrid.SetOnKeyResponse(AValue: TMouseEvent);
begin
  if FOnKeyResponse=AValue then Exit;
  FOnKeyResponse:=AValue;
end;

constructor TKeyGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseDown := @GridResponse;
  Align := alClient;
end;

procedure TKeyGrid.CreateExampleKeys(AColCount, ARowCount: integer);
var
  i, j: Integer;
  LCount : Integer = 1;
begin
  FCols := AColCount;
  FRows := ARowCount;
  SetLength(FKeys, FCols, FRows);
  for j := Low(FKeys) to High(FKeys) do
    for i:= Low(FKeys[j]) to High(FKeys[j]) do
      begin
        FKeys[j][i] := TStimulusKey.Create(Self);
        FKeys[j][i].Caption := IntToStr(LCount);
        FKeys[j][i].Parent := Self;
        FKeys[j][i].OnResponse := FOnKeyResponse;
        FKeys[j][i].OnConsequence := FOnKeyConsequence;
        Inc(LCount)
      end;
end;

procedure TKeyGrid.DestroyExampleKeys;
begin
  while ComponentCount > 0 do
    Components[0].Free;
  SetLength(FKeys,0,0);
end;

function TKeyGrid.AsString: string;
var
  j, i: Integer;
begin
  Result := '';
  for j := Low(FKeys) to High(FKeys) do
    for i:= Low(FKeys[j]) to High(FKeys[j]) do
      Result := Result +
        FKeys[j][i].Caption + '-(' +
        IntToStr(FKeys[j][i].Left)+','+
        IntToStr(FKeys[j][i].Top)+'),';
end;

procedure TKeyGrid.Resize;
var
  LLeft : Integer = 0;
  LTop : Integer = 0;
  i, j: Integer;

  {
    GetPositionFromSegment returns Left or Top position based on:
      ASegment = Width or Height from which get the Left or Top position.
      ASteps = Desired number os columns or rows.
      AStep = Target column or row.
      AStimulusSize = Width or height of the target stimulus.
      AInterStimulusSpace = Desired horizontal or vertical space from one stimulus to another.
  }
  function GetPositionFromSegment(ASegment, AStep, ASteps,
    AStimulusSize, AInterStimulusSpace : integer):integer;
  var
    LSize : integer;
  begin
    LSize := AStimulusSize + AInterStimulusSpace;
    Result := Round((LSize*AStep)-((LSize*ASteps)/2)+((ASegment+AInterStimulusSpace)/2));
  end;
begin
  inherited Resize;
  if Length(FKeys) > 0 then
    for j := Low(FKeys) to High(FKeys) do
      begin
        LTop := GetPositionFromSegment(ClientRect.Bottom, j, FCols, 150, 50);
        for i:= Low(FKeys[j]) to High(FKeys[j]) do
          if Assigned(FKeys[j][i]) then
            begin
              LLeft := GetPositionFromSegment(ClientRect.Right, i, FRows, 150, 50);
              FKeys[j][i].Left := LLeft;
              FKeys[j][i].Top := LTop;
            end;
      end;
end;

procedure TKeyGrid.RandomizeKeyPositions;
var
  i, j, ri, rj,
  AColCount, ARowCount : integer;
  LKey : TStimulusKey;
begin
  AColCount := Length(FKeys);
  for j := Low(FKeys) to High(FKeys) do
    for i:= Low(FKeys[j]) to High(FKeys[j]) do
      begin
        ARowCount := Length(FKeys[i]);
        rj := RandomRange(0, AColCount);
        ri := RandomRange(0, ARowCount);
        LKey := FKeys[j][i];
        FKeys[j][i] := FKeys[rj][ri];
        FKeys[rj][ri] := LKey;
      end;
  Resize;
end;

end.


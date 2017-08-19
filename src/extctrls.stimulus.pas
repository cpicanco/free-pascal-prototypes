{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit ExtCtrls.Stimulus;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, Behavior.Events;

type

  { TStimulus }

  TStimulus = class(TImage)
  private
    FOnVisibilityChange: TBehavioralEvent;
    procedure SetOnVisibilityChange(AValue: TBehavioralEvent);
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    property OnVisibilityChange : TBehavioralEvent read FOnVisibilityChange write SetOnVisibilityChange;
  end;


implementation


{ TStimulus }


procedure TStimulus.SetOnVisibilityChange(AValue: TBehavioralEvent);
begin
  if FOnVisibilityChange=AValue then Exit;
  FOnVisibilityChange:=AValue;
end;

procedure TStimulus.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Assigned(OnVisibilityChange) then
    if Value then
      OnVisibilityChange(Self, EnviromentEvent, 'Show')
    else
      OnVisibilityChange(Self, EnviromentEvent, 'Hide');
end;

end.


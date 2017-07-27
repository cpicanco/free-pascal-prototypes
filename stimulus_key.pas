{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit stimulus_key;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls;

type

  { TKey }

  TKey = class(TGraphicControl)
  private
    FBitmap: TBitmap;
    procedure KeyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FOnConsequence: TNotifyEvent;
    FOnResponse: TMouseEvent;
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TMouseEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string);
    procedure Paint; override;
  public
    property Caption;
    property OnConsequence: TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnResponse: TMouseEvent read FOnResponse write SetOnResponse;
    property OnMouseDown;
  end;

implementation

constructor TKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseDown := @KeyMouseDown;
  Caption := 'SD';
  Left := 0;
  Top := 0;
  Width := 150;
  Height := 150;
  FBitmap := TBitmap.Create;
  with FBitmap do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := Self.Color;
      Canvas.Rectangle(ClientRect);
    end;

  with Canvas do
    begin
      Font.Size:= 48;
      Pen.Width := 3;
    end;
end;

destructor TKey.Destroy;
begin
  if Assigned(FBitmap) then
    FBitmap.Free;
  inherited Destroy;
end;

procedure TKey.Paint;
begin
  Canvas.StretchDraw(ClientRect, FBitmap);
  if Caption <> '' then
    Canvas.TextRect(ClientRect,
      (((ClientRect.Right-ClientRect.Left) div 2) - (Canvas.TextWidth(Caption) div 2)),
      (((ClientRect.Bottom-ClientRect.Top) div 2) - (Canvas.TextHeight(Caption) div 2)),
      Caption);
end;

procedure TKey.LoadFromFile(AFilename: string);
  procedure Load_PNG;
  var
    LPNG : TPortableNetworkGraphic;
  begin
    LPNG := TPortableNetworkGraphic.Create;
    LPNG.LoadFromFile(AFilename);
    FBitmap.Assign(LPNG);
    FBitmap.Transparent:=True;
    FBitmap.TransparentColor:=clFuchsia;
    LPNG.Free;
  end;

  procedure Load_JPG;
  var
    LJPG : TJPEGImage;
  begin
    LJPG := TJPEGImage.Create;
    LJPG.LoadFromFile(AFilename);
    FBitmap.Assign(LJPG);
    LJPG.Free;
  end;

begin
  if FileExists(AFilename) then
    begin
      case UpperCase(ExtractFileExt(AFilename)) of
        // images
        '.BMP' : FBitmap.LoadFromFile(AFilename);
        '.JPG' : Load_JPG;
        '.PNG' : Load_PNG;
        else
          Exit;
      end;
      Invalidate;
    end;
end;

procedure TKey.KeyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnResponse) then
    OnResponse(Sender,Button, Shift, X + Left, Y + Top);

  if Button = mbLeft then
    if Assigned(OnConsequence) then OnConsequence(Self);
end;

procedure TKey.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence=AValue then Exit;
  FOnConsequence:=AValue;
end;

procedure TKey.SetOnResponse(AValue: TMouseEvent);
begin
  if FOnResponse=AValue then Exit;
  FOnResponse:=AValue;
end;


end.


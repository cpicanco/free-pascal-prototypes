unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TShapeDirection = (sdTop, sdRight, sdBottom, sdLeft);

  { TBall }

  TBall = class sealed(TShape)
  private
    FDirection: TShapeDirection;
    FMovementSize: integer;
    procedure SetDirection(AValue: TShapeDirection);
    procedure CustomClick(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Move;
    property Direction : TShapeDirection read FDirection write SetDirection;
    property MovementSize : integer read FMovementSize write FMovementSize;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    TheForce: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TheForceTimer(Sender: TObject);
  private
    FBall : TBall;
  public

  end;



var
  Form1: TForm1;

var
  Granularity : Cardinal = 80;

implementation

{$R *.lfm}

uses
  math
  //, typinfo
  ;

{ TBall }

procedure TBall.SetDirection(AValue: TShapeDirection);
begin
  if FDirection=AValue then Exit;
  FDirection:=AValue;
end;

procedure TBall.CustomClick(Sender: TObject);
begin
  Brush.Color := RGBToColor(RandomRange(0,256),RandomRange(0,256),RandomRange(0,256));
  case Direction of
    sdLeft: Direction := sdTop;
    sdTop: Direction := sdRight;
    sdRight: Direction := sdBottom;
    sdBottom: Direction := sdLeft;
  end;
end;

constructor TBall.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClick:=@CustomClick;
  Shape:=stCircle;
  MovementSize:=10;
  Direction:=sdBottom;
end;

procedure TBall.Move;
begin
  case Direction of
    sdLeft: if Left <= 0 then Direction := sdRight;
    sdTop: if Top <= 0 then Direction := sdBottom;
    sdRight: if Left >= (Parent.Width-Width) then Direction := sdLeft;
    sdBottom: if Top >= (Parent.Height-Height) then Direction := sdTop;
  end;
  //WriteLn(GetEnumName(TypeInfo(TShapeDirection),Ord(Direction)));
  case Direction of
    sdLeft: Left := Left-MovementSize;
    sdTop: Top := Top-MovementSize;
    sdRight: Left := Left+MovementSize;
    sdBottom: Top := Top+MovementSize;
  end;
end;

{ TForm1 }

procedure TForm1.TheForceTimer(Sender: TObject);
begin
  FBall.Move;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TheForce.Interval:=Granularity;

  FBall := TBall.Create(Self);
  FBall.Parent := Self;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    37 : FBall.Direction:=sdLeft;
    38 : FBall.Direction:=sdTop;
    39 : FBall.Direction:=sdRight;
    40 : FBall.Direction:=sdBottom;
  end;
end;



end.


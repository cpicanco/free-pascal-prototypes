unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Shape1: TShape;
    Timer1: TTimer;
    procedure Image1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Image1Click(Sender: TObject);
  procedure ResetTimer;
  begin
    Timer1.Enabled := False;
    Timer1.Enabled := True;
  end;
begin
  Shape1.Show;
  ResetTimer;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Shape1.Hide;
end;

end.


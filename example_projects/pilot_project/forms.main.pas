unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls.Stimulus;

type

  { TForm1 }

  TForm1 = class(TForm)
    StimulusAntecedent : TStimulus;
    StimulusConsequent : TStimulus;
    procedure ComponentClick(Sender: TObject);
    procedure ComponentKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RecordBehavior(Sender: TObject; const Category:string;
      const EventSufix: string); inline;
  private
    FFirstTickcount : Cardinal;
    procedure CreateStimulus(out AStimulus : TStimulus; AColor : TColor;
      ASize : integer; ALeft : integer; ATop: integer);
  public

  end;

var
  Form1: TForm1;

implementation

uses TabDelimitedReport, Timestamps, Behavior.Events;

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateStimulus(StimulusAntecedent, clBlack,150,0,0);
  StimulusAntecedent.Name:='Preto';
  CreateStimulus(StimulusConsequent, clBlue,100,200,0);
  StimulusConsequent.Name:='Azul';

  Report.Filename := Application.ExeName;
  Report.WriteRow(['Tempo', 'Categoria', 'Evento']);
  FFirstTickcount := GetTickCount64;
  RecordBehavior(Sender, SystemEvent, 'inicio:'+DateTimeToStr(Now));
  StimulusAntecedent.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RecordBehavior(Sender, SystemEvent, 'final:'+DateTimeToStr(Now));
  Report.CloseFile;
end;

procedure TForm1.RecordBehavior(Sender: TObject; const Category: string;
  const EventSufix: string);
var
  SenderName: string;
begin
  if Sender is TComponent then
    SenderName := TComponent(Sender).Name
  else
    SenderName := Sender.ClassName;

  Report.WriteRow([
    Miliseconds(FFirstTickcount),
    Category,
    SenderName+#32+EventSufix
  ]);
end;

procedure TForm1.CreateStimulus(out AStimulus: TStimulus; AColor: TColor;
  ASize: integer; ALeft: integer; ATop: integer);
begin
  AStimulus := TStimulus.Create(Self);
  AStimulus.SetBounds(ALeft, ATop, ASize, ASize);
  AStimulus.Picture.Bitmap.SetSize(ASize, ASize);
  AStimulus.Picture.Bitmap.Canvas.Brush.Color := AColor;
  AStimulus.Picture.Bitmap.Canvas.Rectangle(0,0, ASize, ASize);
  AStimulus.Parent := Self;
  AStimulus.Hide;
  AStimulus.OnVisibilityChange:=@RecordBehavior;
  AStimulus.OnClick:=@ComponentClick;

  // AStimulus.Picture.LoadFromFile();
  // AStimulus.Stretch := True;
end;

procedure TForm1.ComponentClick(Sender: TObject);
begin
  RecordBehavior(Sender, BehavioralEvent, 'Click');
  if Sender = StimulusAntecedent then
    begin
      StimulusAntecedent.Hide;
      StimulusConsequent.Show;
    end;
  if Sender = StimulusConsequent then
    begin
      StimulusConsequent.Hide;
      StimulusAntecedent.Show;
    end;
end;

procedure TForm1.ComponentKeyPress(Sender: TObject; var Key: char);
const
  SpaceKey = #32;
  DeleteKey = #127;
var
  Event : string = '';
begin
  case Key of
    SpaceKey : Event := '<32>';
    DeleteKey: Event := '<127>';
    #0..#31  : Event := '<NA>';
  end;
  RecordBehavior(Sender, BehavioralEvent, Event);
end;


end.


unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Schedules;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    FSchedule : TSchedule;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure Error(const msg : string);
begin
  raise Exception.Create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  FSchedule := TSchedule.Create(Self);
  WriteLn(FSchedule.Loaded);

  FSchedule.AsString:='CRF';
  WriteLn(FSchedule.AsString);
  WriteLn(FSchedule.ScheduleNameAsString);
  WriteLn(FSchedule.ParametersAsString);
  WriteLn(FSchedule.Loaded);
  WriteLn('');

  FSchedule.AsString:='FR 2';
  WriteLn(FSchedule.AsString);
  WriteLn(FSchedule.ScheduleNameAsString);
  WriteLn(FSchedule.ParametersAsString);
  WriteLn('');

  FSchedule.Load(FR, 2);
  WriteLn(FSchedule.AsString);
  WriteLn(FSchedule.ScheduleNameAsString);
  WriteLn(FSchedule.ParametersAsString);
  WriteLn('');

  FSchedule.Load('DRL 1 4000');
  WriteLn(FSchedule.AsString);
  WriteLn(FSchedule.ScheduleNameAsString);
  WriteLn(FSchedule.ParametersAsString);
  WriteLn(FSchedule.Parameter[0]);
  WriteLn(FSchedule.Parameter[1]);
  FSchedule.Parameter[0] := 2;
  FSchedule.Parameter[1] := 5000;
  WriteLn(FSchedule.Parameter[0]);
  WriteLn(FSchedule.Parameter[1]);
  WriteLn('');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

end.


{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin,
  Sessions;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonStartSession: TButton;
    ButtonClose: TButton;
    LabelMatrix: TLabel;
    Panel1: TPanel;
    SpinEditMatrix: TSpinEdit;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonStartSessionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    FSession : TSession;
    FReportFilename : string;
    procedure SessionStop(Sender: TObject);
  public

  end;

var
  FormMain: TFormMain;

resourcestring
  RSSessionStart = 'A sessão irá começar.';

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  WindowState:=wsFullScreen;
  FSession := TSession.Create(Self);
  FSession.OnStop:=@SessionStop;
end;

procedure TFormMain.Panel1Resize(Sender: TObject);
begin
  Panel1.Left:= (Width div 2)-(Panel1.Width div 2);
  Panel1.Top:= (Height div 2)-(Panel1.Height div 2);
end;

procedure TFormMain.SessionStop(Sender: TObject);
begin
  Panel1.Show;
end;

procedure TFormMain.ButtonStartSessionClick(Sender: TObject);
begin
  Panel1.Hide;
  ShowMessage(RSSessionStart);
  FSession.Start(Self, Application.ExeName, SpinEditMatrix.Value);
end;

procedure TFormMain.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.


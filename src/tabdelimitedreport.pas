{
  Simple Discrimination
  Copyright (C) 2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit TabDelimitedReport;

{$mode objfpc}{$H+}

interface

type

  { TTabDelimitedReport }

  TTabDelimitedReport = class
  private
    FFilename : string;
    FTextFile : TextFile;
    procedure SetFilename(AFilename: string);
  public
    procedure CloseFile;
    procedure NextFile;
    procedure WriteRow(Cols : array of string);
    property Filename : string read FFilename write SetFilename;
  end;

var
  Report : TTabDelimitedReport;       // variável pública

implementation

uses SysUtils, LazFileUtils; // torna visível funções para o manuseio de arquivos

procedure TTabDelimitedReport.WriteRow(Cols: array of string);
const
  TAB = #9;
var
  i : Integer;
  LastColumn : Integer;
begin
  LastColumn := High(Cols);
  for i := 0 to LastColumn do          // percorre todos os itens
    if i < LastColumn then             // se antes do último item
      Write(FTextFile, Cols[i]+TAB)    // escreve item e TAB
    else               // se último escreve item e final de linha
      WriteLn(FTextFile, Cols[i]);
  Flush(FTextFile);                    // salva as mudanças no disco rígido
end;

procedure TTabDelimitedReport.SetFilename(AFilename: string);
var
  LFilePath, LExtension, LBasename: string;
  i : Integer;
begin
                     // retorna o caminho raiz do nome de arquivo
  LFilePath := ExtractFilePath(AFilename);

                     // retorna apenas o nome base do arquivo
                     // sem extenção e sem caminho
  LBasename := ExtractFileNameOnly(AFilename);

                     // retorna a extenção do nome do arquivo
  LExtension := ExtractFileExt(AFilename);

                     // caso a extenção seja vazia ou .exe
                     // a extenção torna-se '.txt'
  case LExtension of
  '', '.exe' : LExtension:='.txt';
  end;

                      // nunca subscreva um arquivo já existente
                      // se o arquivo existir, incremente seu nome
  i := 0;
  while FileExists(AFilename) do
    begin
      Inc(i);
      AFilename := LFilePath+LBasename+'_data_'+Format('%.3d', [i])+LExtension;
    end;

                         // atribui um nome ao arquivo de texto
  AssignFile(FTextFile, AFilename);
  Rewrite(FTextFile);    // abre o arquivo de texto para escrita
  FFilename:=AFilename;  // salva o nome do arquivo para uso posterior
end;

procedure TTabDelimitedReport.NextFile;
begin
  SetFilename(FFilename);             // abre um novo arquivo
end;

procedure TTabDelimitedReport.CloseFile;
begin
  System.Close(FTextFile);            // fecha o arquivo de texto
end;

initialization // antes de executar o programa, crie (a memória do) objeto
  Report := TTabDelimitedReport.Create;

finalization   // após finalizar o programa, libere (a memória do) objeto
  Report.Free;

end.

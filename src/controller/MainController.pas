unit MainController;

interface

uses System.IOUtils, System.SysUtils, Vcl.Dialogs;

type TMainController = class
  public
    procedure SetupDirectory;
end;

implementation

{ TMainController }

procedure TMainController.SetupDirectory;
var
  path: String;
begin
  path := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MTTools');
  if not TDirectory.Exists(path) then begin
    try
      TDirectory.CreateDirectory(path);
    except
    on e: Exception do
      ShowMessage('Erro ao criar diretório do sistema em "' + path + '": ' + e.message);
    end;
  end;
end;

end.

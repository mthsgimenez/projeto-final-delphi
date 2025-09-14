unit ConfigController;

interface

uses DBConfigDTO, DBConfigModel, DBConnection,
      DBConfigRepositoryInterface, DBConfigRepositoryJSON,
      System.IOUtils, System.SysUtils, Vcl.Dialogs, FireDAC.Phys.PGWrapper;

type TConfigController = class
  private
    repository: IDBConfigRepository;
  public
    constructor Create;
    procedure PrepareDirectory;
    procedure ConfigureConnection(aDBConfigModel: TDBConfigModel);
end;

implementation

{ TDBConfigController }

constructor TConfigController.Create;
begin
  Self.repository := TDBConfigRepositoryJSON.Create;
end;

procedure TConfigController.PrepareDirectory;
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

procedure TConfigController.ConfigureConnection(aDBConfigModel: TDBConfigModel);
begin
  try
    Connection.FDConnection.Connected := False;
    with Connection.FDConnection.Params do begin
      Clear;
      Add('Server=' + aDBConfigModel.server);
      Add('Port=' + aDBConfigModel.port.ToString);
      Add('Database=' + aDBConfigModel.database);
      Add('User_Name=' + aDBConfigModel.user);
      Add('Password=' + aDBConfigModel.password);
      Add('DriverID=PG');
    end;
    Connection.FDConnection.Connected := True;
  except
  on e: EPgNativeException do begin
    ShowMessage('Erro na conexão: ' + e.Message);
  end;
  on e: Exception do begin
    ShowMessage('Erro: ' + e.Message);
  end;
  end;
end;

end.

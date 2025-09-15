unit ConfigController;

interface

uses DBConfigDTO, DBConfigModel, DBConnection,
      DBConfigRepositoryInterface, DBConfigRepositoryJSON,
      System.IOUtils, System.SysUtils, Vcl.Dialogs, FireDAC.Phys.PGWrapper;

type TConfigController = class
  private
    repository: IDBConfigRepository;
    function DtoToModel(aConfigDTO: TDBConfigDTO): TDBConfigModel;
  public
    constructor Create;
    procedure PrepareDirectory;
    procedure ConfigureConnection(aDBConfig: TDBConfigDTO);
    procedure LoadDBConfig;
    function IsDBConnected: Boolean;
end;

implementation

{ TDBConfigController }

constructor TConfigController.Create;
begin
  Self.repository := TDBConfigRepositoryJSON.Create;
end;

function TConfigController.DtoToModel(aConfigDTO: TDBConfigDTO): TDBConfigModel;
begin
  Result := TDBConfigModel.Create;
  Result.server := aConfigDTO.server;
  Result.port := aConfigDTO.port;
  Result.database := aConfigDTO.database;
  Result.user := aConfigDTO.user;
  Result.password := aConfigDTO.password;
end;

function TConfigController.IsDBConnected: Boolean;
begin
  Result := Connection.FDConnection.Connected;
end;

procedure TConfigController.LoadDBConfig;
var
  config: TDBConfigModel;
begin
  try
    config := repository.Get;
    Connection.Configure(config);
    config.Free;
  except
    raise;
  end;
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

procedure TConfigController.ConfigureConnection(aDBConfig: TDBConfigDTO);
var
  DBConfigModel: TDBConfigModel;
begin
  DBConfigModel := Self.DtoToModel(aDBConfig);
  try
    Connection.Configure(DBConfigModel);
    repository.Save(DBConfigModel);
  finally
    DBConfigModel.Free;
  end;
end;

end.

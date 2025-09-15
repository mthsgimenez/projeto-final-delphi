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
    destructor Destroy;
    function IsConfigValid: Boolean;
    procedure PrepareDirectory;
    procedure ConfigureConnection(aDBConfig: TDBConfigDTO);
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

function TConfigController.IsConfigValid: Boolean;
begin
  try
    repository.Get;
    Result := True;
  except
    Result := False;
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

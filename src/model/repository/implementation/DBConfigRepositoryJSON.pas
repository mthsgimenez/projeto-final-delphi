unit DBConfigRepositoryJSON;

interface

  uses DBConfigModel, DBConfigRepository, DBConfigDTO,
  System.JSON, System.IOUtils, System.SysUtils, Vcl.Dialogs;

  type TDBConfigRepositoryJSON = class(TInterfacedObject, IDBConfigRepository)
    private
      path: String;
    public
      function ReadFromFile: TDBConfigModel;
      procedure SaveToFile(aDBConfigModel: TDBConfigModel);
      constructor Create(aPath: String);
      function PathExists(aPath: String): Boolean;
  end;

implementation

{ TDBConfigRepositoryJSON }

constructor TDBConfigRepositoryJSON.Create(aPath: String);
begin
  inherited Create;
  if not PathExists(aPath) then raise Exception.Create('Caminho para arquivo de configuração inválido: ' + aPath);
  Self.path := aPath;
end;

function TDBConfigRepositoryJSON.PathExists(aPath: String): Boolean;
var
  path: String;
begin
  path := ExtractFileDir(aPath);
  
  if DirectoryExists(path) then begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

function TDBConfigRepositoryJSON.ReadFromFile: TDBConfigModel;
var
  jsonFile: TJSONValue;
  config: TDBConfigModel;
begin
  jsonFile := TJSONObject.ParseJSONValue(TFile.ReadAllBytes(Self.path), 0);
  if jsonFile <> nil then
    config := TDBConfigModel.Create;
    //  TODO: Tratamento de erro
    try
      config.server := TJSONObject(jsonFile).GetValue<String>('server');
      config.port := TJSONObject(jsonFile).GetValue<Integer>('port');
      config.database := TJSONObject(jsonFile).GetValue<String>('database');
      config.user := TJSONObject(jsonFile).GetValue<String>('user');
      config.password := TJSONObject(jsonFile).GetValue<String>('password');

      Result := config;
    except
    on e: Exception do begin
      config.Free;
      ShowMessage(e.Message);
    end;
    end;
    jsonFile.Free;
end;

procedure TDBConfigRepositoryJSON.SaveToFile(aDBConfigModel: TDBConfigModel);
var
  json: TJSONObject;
begin
  json := TJSONObject.Create;
  try
    json.AddPair('server', aDBConfigModel.server);
    json.AddPair('port', TJSONNumber.Create(aDBConfigModel.port));
    json.AddPair('database', aDBConfigModel.database);
    json.AddPair('user', aDBConfigModel.user);
    json.AddPair('password', aDBConfigModel.password);

    TFile.WriteAllText(Self.path, json.ToString);
  finally
    json.Free;
  end;
end;

end.

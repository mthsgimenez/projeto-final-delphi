unit DBConfigRepositoryJSON;

interface

  uses DBConfigRepositoryInterface, DBConfigModel, DBConfigDTO,
  System.JSON, System.IOUtils, System.SysUtils, Vcl.Dialogs;

  type TDBConfigRepositoryJSON = class(TInterfacedObject, IDBConfigRepository)
    private
      path: String;
    public
      function Get: TDBConfigModel;
      procedure Save(aDBConfigModel: TDBConfigModel);
      constructor Create;
  end;

implementation

{ TDBConfigRepositoryJSON }

constructor TDBConfigRepositoryJSON.Create;
begin
  inherited Create;
  Self.path := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MTTools', 'config.json');
end;

function TDBConfigRepositoryJSON.Get: TDBConfigModel;
var
  jsonFile: TJSONValue;
  config: TDBConfigModel;
  currentField: String;
begin
  try
    jsonFile := TJSONObject.ParseJSONValue(TFile.ReadAllBytes(Self.path), 0);

    if jsonFile = nil then
    raise Exception.Create('Erro ao ler arquivo config.json localizado em "' + Self.path + '". Verifique se o arquivo é um arquivo json válido.');

    config := TDBConfigModel.Create;

    try
      currentField := 'server';
      config.server := TJSONObject(jsonFile).GetValue<String>('server');

      currentField := 'port';
      config.port := TJSONObject(jsonFile).GetValue<Integer>('port');

      currentField := 'database';
      config.database := TJSONObject(jsonFile).GetValue<String>('database');

      currentField := 'user';
      config.user := TJSONObject(jsonFile).GetValue<String>('user');

      currentField := 'password';
      config.password := TJSONObject(jsonFile).GetValue<String>('password');

      Result := config;
    except
      on e: Exception do begin
        config.Free;
        ShowMessage('Erro ao ler campo "' + currentField + '" do arquivo config.json localizado em "' + Self.path + '". Verifique se o campo está presente e nomeado corretamente.');
      end;
    end;
  finally
    jsonFile.Free;
  end;
end;

procedure TDBConfigRepositoryJSON.Save(aDBConfigModel: TDBConfigModel);
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

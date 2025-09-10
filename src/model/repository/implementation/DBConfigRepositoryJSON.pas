unit DBConfigRepositoryJSON;

interface

  uses DBConfigRepositoryInterface, DBConfigModel, DBConfigDTO,
  System.JSON, System.IOUtils, System.SysUtils, Vcl.Dialogs;

  type TDBConfigRepositoryJSON = class(TInterfacedObject, IDBConfigRepository)
    private
      path: String;
    public
      function ReadFromFile: TDBConfigModel;
      procedure SaveToFile(aDBConfigModel: TDBConfigModel);
      constructor Create;
  end;

implementation

{ TDBConfigRepositoryJSON }

constructor TDBConfigRepositoryJSON.Create;
begin
  inherited Create;
  Self.path := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MTTools');
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

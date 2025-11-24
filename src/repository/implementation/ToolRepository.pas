unit ToolRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param,
  ToolRepositoryInterface, StorageRepositoryInterface, ToolTypeRepositoryInterface,
  ToolModel, StorageModel, ToolTypeModel;

type TToolRepository = class(TRepositoryBase, IToolRepository)
  private
    storageRepository: IStorageRepository;
    toolTypeRepository: IToolTypeRepository;
    helper: TDBHelper;
  public
    function Insert(aTool: TTool): TTool;
    function Update(aTool: TTool): TTool;
    function FindById(aToolId: Integer): TTool;
    function FindAll(): TObjectList<TTool>;
    function FindAvailable(): TObjectList<TTool>;
    function DeleteById(aToolId: Integer): Boolean;
    function ExistsById(aToolId: Integer): Boolean;

    constructor Create(aHelper: TDBHelper; aToolTypeRepository: IToolTypeRepository;
      aStorageRepository: IStorageRepository);
end;

implementation

{ TToolRepository }

constructor TToolRepository.Create(aHelper: TDBHelper;
  aToolTypeRepository: IToolTypeRepository;
  aStorageRepository: IStorageRepository);
begin
  inherited Create;
  Self.helper := aHelper;
  Self.storageRepository := aStorageRepository;
  Self.toolTypeRepository := aToolTypeRepository;
end;

function TToolRepository.DeleteById(aToolId: Integer): Boolean;
begin
  Self.Query.SQL.Text := 'DELETE FROM tools WHERE id = :toolId';
  Self.Query.ParamByName('toolId').AsInteger := aToolId;

  try
    Self.Query.ExecSQL;
    Result := Self.Query.RowsAffected > 0;
  except
    Result := False;
  end;
end;

function TToolRepository.ExistsById(aToolId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('tools', 'id', aToolId);
end;

function TToolRepository.FindAll: TObjectList<TTool>;
var
  tools: TObjectList<TTool>;
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM tools';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tools := TObjectList<TTool>.Create;
      Result := tools;

      while not Self.Query.Eof do begin
        tool := TTool.Create;
        tool.code := Self.Query.FieldByName('code').AsString;
        tool.id := Self.Query.FieldByName('id').AsInteger;
        tool.model := Self.toolTypeRepository.FindById(
          Self.Query.FieldByName('id_tool_model').AsInteger
        );
        tool.state := StringToState(Self.Query.FieldByName('state').AsString);
        tool.honingNum := Self.Query.FieldByName('honing_num').AsInteger;
        tool.storage := Self.storageRepository.FindById(
          Self.Query.FieldByName('id_storage').AsInteger
        );
        tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

        tools.Add(tool);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolRepository.FindAvailable: TObjectList<TTool>;
var
  tools: TObjectList<TTool>;
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM tools WHERE "status" = ''AVAILABLE''';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tools := TObjectList<TTool>.Create;
      Result := tools;

      while not Self.Query.Eof do begin
        tool := TTool.Create;
        tool.code := Self.Query.FieldByName('code').AsString;
        tool.id := Self.Query.FieldByName('id').AsInteger;
        tool.model := Self.toolTypeRepository.FindById(
          Self.Query.FieldByName('id_tool_model').AsInteger
        );
        tool.state := StringToState(Self.Query.FieldByName('state').AsString);
        tool.honingNum := Self.Query.FieldByName('honing_num').AsInteger;
        tool.storage := Self.storageRepository.FindById(
          Self.Query.FieldByName('id_storage').AsInteger
        );
        tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

        tools.Add(tool);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolRepository.FindById(aToolId: Integer): TTool;
var
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM tools WHERE id = :toolId';
  Self.Query.ParamByName('toolId').AsInteger := aToolId;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tool := TTool.Create;
      tool.id := Self.Query.FieldByName('id').AsInteger;
      tool.code := Self.Query.FieldByName('code').AsString;
      tool.model := Self.toolTypeRepository.FindById(
        Self.Query.FieldByName('id_tool_model').AsInteger
      );
      tool.state := StringToState(Self.Query.FieldByName('state').AsString);
      tool.honingNum := Self.Query.FieldByName('honing_num').AsInteger;
      tool.storage := Self.storageRepository.FindById(
        Self.Query.FieldByName('id_storage').AsInteger
      );
      tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

      Result := tool;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolRepository.Insert(aTool: TTool): TTool;
var
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'INSERT INTO tools (code, id_tool_model, id_storage) ' +
    'VALUES (:code, :toolTypeId, :storageId) ' +
    'RETURNING *';

  Self.Query.ParamByName('code').AsString := aTool.model.code + '_temp';
  Self.Query.ParamByName('toolTypeId').AsInteger := aTool.model.id;
  Self.Query.ParamByName('storageId').AsInteger := aTool.storage.id;

  Self.Query.Connection.StartTransaction;
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      tool := TTool.Create;

      tool.id := Self.Query.FieldByName('id').AsInteger;
      tool.code := Self.Query.FieldByName('code').AsString;
      tool.model := Self.toolTypeRepository.FindById(
        Self.Query.FieldByName('id_tool_model').AsInteger
      );
      tool.code := Format('%s_%d', [tool.model.code, tool.id]);
      tool.state := StringToState(Self.Query.FieldByName('state').AsString);
      tool.honingNum := Self.Query.FieldByName('honing_num').AsInteger;
      tool.storage := Self.storageRepository.FindById(
        Self.Query.FieldByName('id_storage').AsInteger
      );
      tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

      Self.Query.Close;

      Self.Query.SQL.Text :=
        'UPDATE tools SET code = :code ' +
        'WHERE id = :toolId';

      Self.Query.ParamByName('code').AsString := tool.code;
      Self.Query.ParamByName('toolId').AsInteger := tool.id;

      Self.Query.ExecSQL;

      Self.Query.Connection.Commit;
      Result := tool;
    end;
  except
    Self.Query.Connection.Rollback;
  end;
end;

function TToolRepository.Update(aTool: TTool): TTool;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'UPDATE tools SET state = :state, honing_num = :honingNum, id_storage = :storageId, status = :status ' +
    'WHERE id = :toolId RETURNING *';

  Self.Query.ParamByName('state').AsString := StateToString(aTool.state);
  Self.Query.ParamByName('honingNum').AsInteger := aTool.honingNum;
  Self.Query.ParamByName('storageId').AsInteger := aTool.storage.id;
  Self.Query.ParamByName('status').AsString := StatusToString(aTool.status);
  Self.Query.ParamByName('toolId').AsInteger := aTool.id;

  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      Result := TTool.Create;
      Result.id := Self.Query.FieldByName('id').AsInteger;
      Result.code := Self.Query.FieldByName('code').AsString;
      Result.model := Self.toolTypeRepository.FindById(
        Self.Query.FieldByName('id_tool_model').AsInteger
      );
      Result.state := StringToState(Self.Query.FieldByName('state').AsString);
      Result.honingNum := Self.Query.FieldByName('honing_num').AsInteger;
      Result.storage := Self.storageRepository.FindById(
        Self.Query.FieldByName('id_storage').AsInteger
      );
      Result.status := StringToStatus(Self.Query.FieldByName('status').AsString);
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

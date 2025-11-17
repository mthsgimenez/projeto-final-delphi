unit StorageRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase,
  StorageRepositoryInterface, StorageModel, ToolTypeModel, ToolModel, ToolTypeRepositoryInterface;

type TStorageRepository = class(TRepositoryBase, IStorageRepository)
  private
    helper: TDBHelper;
    toolTypeRepository: IToolTypeRepository;
  public
    function Insert(aStorage: TStorage): TStorage;
    function Update(aStorage: TStorage): TStorage;
    function FindById(aStorageId: Integer): TStorage;
    function FindAll(): TObjectList<TStorage>;
    function DeleteById(aStorageId: Integer): Boolean;
    function ExistsById(aStorageId: Integer): Boolean;
    function GetToolTypesInStorage(aStorageId: Integer): TObjectList<TToolType>;
    function GetToolsInStorage(aStorageId: Integer; aToolTypeId: Integer): TObjectList<TTool>;
    constructor Create(aHelper: TDBHelper; aToolTypeRepository: IToolTypeRepository);
end;

implementation

{ TStorageRepository }

constructor TStorageRepository.Create(aHelper: TDBHelper;
  aToolTypeRepository: IToolTypeRepository);
begin
  Self.helper := aHelper;
  Self.toolTypeRepository := aToolTypeRepository;
end;

function TStorageRepository.DeleteById(aStorageId: Integer): Boolean;
begin
  Self.Query.SQL.Text := 'DELETE FROM storages WHERE id = :storageId';
  Self.Query.ParamByName('storageId').AsInteger := aStorageId;

  try
    try
      Self.Query.ExecSQL;
      Result := Self.Query.RowsAffected > 0;
    except
      Result := False;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.ExistsById(aStorageId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('storages', 'id', aStorageId);
end;

function TStorageRepository.FindAll: TObjectList<TStorage>;
var
  storage: TStorage;
  storages: TObjectList<TStorage>;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT s.*, COUNT(t.id) AS "quantity_total", ' +
    'COUNT (t.id) FILTER (WHERE t.status = ''IN_USE'') AS "quantity_in_use" ' +
    'FROM storages s LEFT JOIN tools t ON t.id_storage = s.id GROUP BY s.id';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storages := TObjectList<TStorage>.Create;

      while not Self.Query.Eof do begin
        storage := TStorage.Create;
        storage.id := Self.Query.FieldByName('id').AsInteger;
        storage.name := Self.Query.FieldByName('name').AsString;
        storage.quantityTotal := Self.Query.FieldByName('quantity_total').AsInteger;
        storage.quantityInUse := Self.Query.FieldByName('quantity_in_use').AsInteger;

        storages.Add(storage);

        Self.Query.Next;
      end;

      Result := storages;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.FindById(aStorageId: Integer): TStorage;
var
  storage: TStorage;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'SELECT s.*, COUNT(t.id) AS "quantity_total", ' +
    'COUNT (t.id) FILTER (WHERE t.status = ''IN_USE'') AS "quantity_in_use" ' +
    'FROM storages s LEFT JOIN tools t ON t.id_storage = s.id ' +
    'WHERE s.id = :storageId GROUP BY s.id';
  Self.Query.ParamByName('storageId').AsInteger := aStorageId;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storage := TStorage.Create;
      storage.id := Self.Query.FieldByName('id').AsInteger;
      storage.name := Self.Query.FieldByName('name').AsString;
      storage.quantityTotal := Self.Query.FieldByName('quantity_total').AsInteger;
      storage.quantityInUse := Self.Query.FieldByName('quantity_in_use').AsInteger;

      Result := storage;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.GetToolsInStorage(aStorageId,
  aToolTypeId: Integer): TObjectList<TTool>;
var
  toolsList: TObjectList<TTool>;
  tool: TTool;
  storage: TStorage;
  toolType: TToolType;
begin
  Result := nil;

  storage := Self.FindById(aStorageId);
  toolType := Self.toolTypeRepository.FindById(aToolTypeId);

  Self.Query.SQL.Text :=
    'SELECT t.* FROM tools t ' +
    'JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'JOIN storages s ON t.id_storage = s.id ' +
    'WHERE tm.id = :toolTypeId AND s.id = :storageId';

  Self.Query.ParamByName('toolTypeId').AsInteger := aToolTypeId;
  Self.Query.ParamByName('storageId').AsInteger := aStorageId;

  try
    try
      Self.Query.Open;
      if not Self.Query.IsEmpty then begin
        toolsList := TObjectList<TTool>.Create;

        while not Self.Query.Eof do begin
          tool := TTool.Create;

          tool.id := Self.Query.FieldByName('id').AsInteger;
          tool.code := Self.Query.FieldByName('code').AsString;
          tool.state := StringToState(Self.Query.FieldByName('state').AsString);
          tool.honingNum := Self.Query.FieldByName('id').AsInteger;
          tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);
          tool.model := toolType;
          tool.storage := storage;

          toolsList.Add(tool);

          Self.Query.Next;
        end;

        Result := toolsList;
      end;
    except
      storage.Free;
      toolType.Free;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.GetToolTypesInStorage(
  aStorageId: Integer): TObjectList<TToolType>;
var
  list: TObjectList<TToolType>;
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'SELECT DISTINCT tm.id AS id ' +
    'FROM tools t ' +
    'JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'JOIN storages s ON t.id_storage = s.id ' +
    'WHERE s.id = :storageId;';

  Self.Query.ParamByName('storageId').AsInteger := aStorageId;

  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      list := TObjectList<TToolType>.Create;

      while not Self.Query.Eof do begin
        toolType := Self.toolTypeRepository.FindById(
          Self.Query.FieldByName('id').AsInteger
        );

        list.Add(toolType);

        Self.Query.Next;
      end;

      Result := list;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.Insert(aStorage: TStorage): TStorage;
var
  storage: TStorage;
begin
  Result := nil;

  Self.Query.SQL.Text := 'INSERT INTO storages("name") VALUES (:name) RETURNING *';
  Self.Query.ParamByName('name').AsString := aStorage.name;

  try
    if Self.helper.CheckIfAlreadyExists('storages', 'name', aStorage.name) then
      raise Exception.Create('Já existe um estoque chamado ' + aStorage.name);

    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storage := TStorage.Create;
      storage.id := Self.Query.FieldByName('id').AsInteger;
      storage.name := Self.Query.FieldByName('name').AsString;

      Result := storage;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageRepository.Update(aStorage: TStorage): TStorage;
var
  updatedStorage: TStorage;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'UPDATE storages SET "name" = :name WHERE id = :storageId RETURNING *';
  Self.Query.ParamByName('name').AsString := aStorage.name;
  Self.Query.ParamByName('storageId').AsInteger := aStorage.id;

  try
    if Self.helper.CheckIfAlreadyExistsExcludingId('storages', 'name', aStorage.name, aStorage.id) then
      raise Exception.Create('Já existe um estoque chamado ' + aStorage.name);

    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      updatedStorage := TStorage.Create;
      updatedStorage.id := Self.Query.FieldByName('id').AsInteger;
      updatedStorage.name := Self.Query.FieldByName('name').AsString;

      Result := updatedStorage;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

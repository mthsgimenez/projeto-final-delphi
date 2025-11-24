unit StorageRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param, FireDAC.Comp.Client, FireDAC.DApt,
  StorageRepositoryInterface, StorageModel, ToolTypeModel, ToolModel, ToolTypeRepositoryInterface;

type TStorageRepository = class(TRepositoryBase, IStorageRepository)
  private
    helper: TDBHelper;
    toolTypeRepository: IToolTypeRepository;
    procedure GetToolTypeCountInsideStorage(aStorageId: Integer; aToolType: TToolType);
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
  inherited Create;
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

  Self.Query.SQL.Text := 'SELECT s.*, ' +
      'COUNT(t.id) AS "quantity_total", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''AVAILABLE'') AS "quantity_available", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''IN_USE'') AS "quantity_in_use", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''HONING'') AS "quantity_honing" ' +
      'FROM storages s ' +
      'LEFT JOIN tools t ON t.id_storage = s.id ' +
      'GROUP BY s.id';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storages := TObjectList<TStorage>.Create;

      while not Self.Query.Eof do begin
        storage := TStorage.Create;
        storage.id := Self.Query.FieldByName('id').AsInteger;
        storage.name := Self.Query.FieldByName('name').AsString;
        storage.quantityTotal := Self.Query.FieldByName('quantity_total').AsInteger;
        storage.quantityAvailable := Self.Query.FieldByName('quantity_available').AsInteger;
        storage.quantityInUse := Self.Query.FieldByName('quantity_in_use').AsInteger;
        storage.quantityHoning := Self.Query.FieldByName('quantity_honing').AsInteger;

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

  Self.Query.SQL.Text := 'SELECT s.*, ' +
      'COUNT(t.id) AS "quantity_total", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''AVAILABLE'') AS "quantity_available", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''IN_USE'') AS "quantity_in_use", ' +
      'COUNT(t.id) FILTER (WHERE t.status = ''HONING'') AS "quantity_honing" ' +
      'FROM storages s ' +
      'LEFT JOIN tools t ON t.id_storage = s.id ' +
      'WHERE s.id = :storageId ' +
      'GROUP BY s.id';
  Self.Query.ParamByName('storageId').AsInteger := aStorageId;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storage := TStorage.Create;
      storage.id := Self.Query.FieldByName('id').AsInteger;
      storage.name := Self.Query.FieldByName('name').AsString;
      storage.quantityTotal := Self.Query.FieldByName('quantity_total').AsInteger;
      storage.quantityAvailable := Self.Query.FieldByName('quantity_available').AsInteger;
      storage.quantityInUse := Self.Query.FieldByName('quantity_in_use').AsInteger;
      storage.quantityHoning := Self.Query.FieldByName('quantity_honing').AsInteger;

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
  toolsQuery: TFDQuery;
begin
  Result := nil;

  toolsQuery := TFDQuery.Create(Self.Query.Connection);
  toolsQuery.Connection := Self.Query.Connection;

  toolsQuery.SQL.Text :=
    'SELECT t.* FROM tools t ' +
    'JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'JOIN storages s ON t.id_storage = s.id ' +
    'WHERE tm.id = :toolTypeId AND s.id = :storageId';

  toolsQuery.ParamByName('toolTypeId').AsInteger := aToolTypeId;
  toolsQuery.ParamByName('storageId').AsInteger := aStorageId;

  try
    toolsQuery.Open;
    if not toolsQuery.IsEmpty then begin
      toolsList := TObjectList<TTool>.Create;

      while not toolsQuery.Eof do begin
        tool := TTool.Create;

        tool.id := toolsQuery.FieldByName('id').AsInteger;
        tool.code := toolsQuery.FieldByName('code').AsString;
        tool.state := StringToState(toolsQuery.FieldByName('state').AsString);
        tool.honingNum := toolsQuery.FieldByName('honing_num').AsInteger;
        tool.status := StringToStatus(toolsQuery.FieldByName('status').AsString);
        tool.model := Self.toolTypeRepository.FindById(
          toolsQuery.FieldByName('id_tool_model').AsInteger
        );
        tool.storage := Self.FindById(
          toolsQuery.FieldByName('id_storage').AsInteger
        );

        toolsList.Add(tool);

        toolsQuery.Next;
      end;

      Result := toolsList;
    end;
  finally
    toolsQuery.Close;
    toolsQuery.Free;
  end;
end;

procedure TStorageRepository.GetToolTypeCountInsideStorage(aStorageId: Integer;
  aToolType: TToolType);
var
  countQuery: TFDQuery;
begin
  countQuery := TFDQuery.Create(Self.Query.Connection);
  countQuery.Connection := Self.Query.Connection;

  countQuery.SQL.Text := 'SELECT s.*, ' +
    'COUNT(t.id) AS "quantity_total", ' +
    'COUNT(t.id) FILTER (WHERE t.status = ''AVAILABLE'') AS "quantity_available", ' +
    'COUNT(t.id) FILTER (WHERE t.status = ''IN_USE'') AS "quantity_in_use", ' +
    'COUNT(t.id) FILTER (WHERE t.status = ''HONING'') AS "quantity_honing" ' +
    'FROM storages s ' +
    'LEFT JOIN tools t ON t.id_storage = s.id ' +
    'WHERE s.id = :storageId ' +
    'AND t.id_tool_model = :toolTypeId ' +
    'GROUP BY s.id';
  countQuery.ParamByName('toolTypeId').AsInteger := aToolType.id;
  countQuery.ParamByName('storageId').AsInteger := aStorageId;

  try
    countQuery.Open;

    if not countQuery.IsEmpty then begin
      aToolType.quantityTotal := countQuery.FieldByName('quantity_total').AsInteger;
      aToolType.quantityAvailable := countQuery.FieldByName('quantity_available').AsInteger;
      aToolType.quantityInUse := countQuery.FieldByName('quantity_in_use').AsInteger;
      aToolType.quantityHoning := countQuery.FieldByName('quantity_honing').AsInteger;
    end;
  finally
    countQuery.Close;
    countQuery.Free;
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

        Self.GetToolTypeCountInsideStorage(aStorageId, toolType);

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

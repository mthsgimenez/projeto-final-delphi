unit StorageDAO;

interface

uses DAOBase, StorageDAOInterface, StorageModel, DBHelper,
  System.Generics.Collections, System.SysUtils, Data.DB, ToolTypeModel;

type TStorageDAO = class(TDAOBase, IStorageDAO)
  function Insert(aStorage: TStorage): TStorage;
  function SelectById(aStorageId: Integer): TStorage;
  function SelectAll(): TObjectList<TStorage>;
  function Update(aStorage: TStorage): TStorage;
  function DeleteById(aStorageId: Integer): Boolean;
  function GetToolTypes(aStorage: TStorage): TObjectList<TToolType>;
end;

implementation

{ TStorageDAO }

function TStorageDAO.DeleteById(aStorageId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format(
    'DELETE FROM storages WHERE id = %d',
    [aStorageId]
  );

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

function TStorageDAO.GetToolTypes(aStorage: TStorage): TObjectList<TToolType>;
var
  list: TObjectList<TToolType>;
  tool: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT ' +
	  'tm.id AS "id", ' +
    'tm.code AS "code", ' +
    'tm.description AS "description", ' +
    'tm."family" AS "family", ' +
    'tm."usage" AS "usage", ' +
    'tm.price AS "price", ' +
    'COUNT(t.id) AS "quantity_total", ' +
    'COUNT(t.id) FILTER (WHERE t.in_use = TRUE) AS "quantity_in_use"' +
    'FROM tools t ' +
    'JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'JOIN storages s ON t.id_storage = s.id ' +
    'WHERE s.id = %d ' +
    'GROUP BY tm.id, s.id',
    [aStorage.id]
  );

  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      list := TObjectList<TToolType>.Create;

      while not Self.Query.Eof do begin
        tool := TToolType.Create;

        tool.id := Self.Query.FieldByName('id').AsInteger;
        tool.code := Self.Query.FieldByName('code').AsString;
        tool.description := Self.Query.FieldByName('description').AsString;
        tool.family := Self.Query.FieldByName('family').AsString;
        tool.usage := Self.Query.FieldByName('usage').AsString;
        tool.price := Self.Query.FieldByName('price').AsCurrency;
        tool.quantityTotal := Self.Query.FieldByName('quantity_total').AsInteger;
        tool.quantityInUse := Self.Query.FieldByName('quantity_in_use').AsInteger;

        list.Add(tool);

        Self.Query.Next;
      end;

      Result := list;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TStorageDAO.Insert(aStorage: TStorage): TStorage;
var
  storage: TStorage;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO storages("name") VALUES (%s) RETURNING *',
    [QuotedStr(aStorage.name)]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExists('storages', 'name', aStorage.name) then
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
    helper.Free;
  end;
end;

function TStorageDAO.SelectAll: TObjectList<TStorage>;
var
  storage: TStorage;
  storages: TObjectList<TStorage>;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT s.*, COUNT(t.id) AS "quantity_total", ' +
    'COUNT (t.id) FILTER (WHERE t.in_use = TRUE) AS "quantity_in_use" ' +
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

function TStorageDAO.SelectById(aStorageId: Integer): TStorage;
var
  storage: TStorage;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT s.*, COUNT(t.id) AS "quantity_total", ' +
    'COUNT (t.id) FILTER (WHERE t.in_use = TRUE) AS "quantity_in_use" ' +
    'FROM storages s LEFT JOIN tools t ON t.id_storage = s.id ' +
    'WHERE s.id = %d GROUP BY s.id',
    [aStorageId]
  );

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

function TStorageDAO.Update(aStorage: TStorage): TStorage;
var
  helper: TDBHelper;
begin
  Self.Query.SQL.Text := Format(
    'UPDATE storages SET "name" = %s WHERE id = %d',
    [QuotedStr(aStorage.name), aStorage.id]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('storages', 'name', aStorage.name, aStorage.id) then
      raise Exception.Create('Já existe um estoque chamado ' + aStorage.name);

    Self.Query.ExecSQL;

    Result := Self.SelectById(aStorage.id);
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

end.

unit StorageDAO;

interface

uses DAOBase, StorageDAOInterface, StorageModel, DBHelper,
  System.Generics.Collections, System.SysUtils, Data.DB;

type TStorageDAO = class(TDAOBase, IStorageDAO)
  function Insert(aStorage: TStorage): TStorage;
  function SelectById(aStorageId: Integer): TStorage;
  function SelectAll(): TObjectList<TStorage>;
  function Update(aStorage: TStorage): TStorage;
  function DeleteById(aStorageId: Integer): Boolean;
  function GetToolCount(aStorage: TStorage): Integer;
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

function TStorageDAO.GetToolCount(aStorage: TStorage): Integer;
begin
  Result := 0;

  Self.Query.SQL.Text := Format(
    'SELECT COUNT(*) FROM tools WHERE id_storage = %d',
    [aStorage.id]
  );

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      Result := Self.Query.FieldByName('count').AsInteger;
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

  Self.Query.SQL.Text := 'SELECT * FROM storages';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      storages := TObjectList<TStorage>.Create;

      while not Self.Query.Eof do begin
        storage := TStorage.Create;
        storage.id := Self.Query.FieldByName('id').AsInteger;
        storage.name := Self.Query.FieldByName('name').AsString;

        storages.Add(storage);

        Self.Query.Next;
      end;
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
    'SELECT * FROM storages WHERE id = %d',
    [aStorageId]
  );

  try
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

function TStorageDAO.Update(aStorage: TStorage): TStorage;
var
  updatedStorage: TStorage;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'UPDATE storages SET "name" = %s WHERE id = %d RETURNING *',
    [QuotedStr(aStorage.name), aStorage.id]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('storages', 'name', aStorage.name, aStorage.id) then
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
    helper.Free;
  end;
end;

end.

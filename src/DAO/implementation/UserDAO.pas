unit UserDAO;

interface

uses DAOBase, UserDAOInterface, UserModel, DBHelper,
  System.Generics.Collections, System.SysUtils, Data.DB;

type TUserDAO = class(TDAOBase, IUserDAO)
  public
    function Insert(aUser: TUserModel): TUserModel;
    function SelectById(aUserId: Integer): TUserModel;
    function SelectAll(): TObjectList<TUserModel>;
    function Update(aUser: TUserModel): TUserModel;
    function DeleteById(aUserId: Integer): Boolean;
    function SelectByLogin(aUserLogin: String): TUserModel;
end;

implementation

{ TUserDAO }

function TUserDAO.DeleteById(aUserId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('DELETE FROM users WHERE id = %d', [aUserId]);

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

function TUserDAO.SelectByLogin(aUserLogin: String): TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT * FROM users WHERE login = %s',
    [QuotedStr(aUserLogin)]);

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      Result := TUserModel.Create;
      Result.id := Self.Query.FieldByName('id').AsInteger;
      Result.name := Self.Query.FieldByName('name').AsString;
      Result.login := Self.Query.FieldByName('login').AsString;
      Result.SetHash(Self.Query.FieldByName('hash').AsString);

      Result.permissionGroup := nil;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserDAO.Insert(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO users(id, name, login, hash) VALUES (%s, %s, %s, %s) RETURNING *',
    [IntToStr(aUser.id), QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(aUser.GetHash)]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExists('users', 'login', aUser.login) then
      raise Exception.Create('Já existe um usuário utilizando o login "' + aUser.login + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      user.permissionGroup := nil;

      Result := user;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

function TUserDAO.SelectAll: TObjectList<TUserModel>;
var
  users: TObjectList<TUserModel>;
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM users';
  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      users := TObjectList<TUserModel>.Create;
      Result := users;

      while not Self.Query.Eof do begin
        user := TUserModel.Create;
        user.id := Self.Query.FieldByName('id').AsInteger;
        user.name := Self.Query.FieldByName('name').AsString;
        user.login := Self.Query.FieldByName('login').AsString;
        user.SetHash(Self.Query.FieldByName('hash').AsString);
        user.permissionGroup := nil;

        users.Add(user);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserDAO.SelectById(aUserId: Integer): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT * FROM users WHERE id = %d', [aUserId]);
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);
      user.permissionGroup := nil;

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserDAO.Update(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
  helper: TDBHelper;
  permGroup: String;
begin
  Result := nil;

  if Assigned(aUser.permissionGroup) then begin
    permGroup := IntToStr(aUser.permissionGroup.id);
  end else begin
    permGroup := 'NULL';
  end;

  Self.Query.SQL.Text := Format(
    'UPDATE users SET name = %s, login = %s, hash = %s, id_pgroup = %s ' +
    'WHERE id = %d RETURNING *',
    [QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(aUser.GetHash), permGroup, aUser.id]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('users', 'login', aUser.login, aUser.id) then
      raise Exception.Create('Já existe um usuário utilizando o login "' + aUser.login + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      user.permissionGroup := nil;

      Result := user;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

end.

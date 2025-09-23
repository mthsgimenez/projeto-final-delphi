unit UserRepository;

interface

uses CrudRepositoryInterface, UserModel, RepositoryBase, Bcrypt, DBHelper, System.SysUtils, System.Generics.Collections, Data.DB, UserDTO;

type TUserRepository = class(TRepositoryBase, ICrudRepository<TUserModel>)
  public
    function Save(aUser: TUserModel): TUserModel;
    function FindById(aId: Integer): TUserModel;
    function FindAll(): TObjectList<TUserModel>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
end;

implementation

{ TUserRepository }

function TUserRepository.DeleteById(aId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('DELETE FROM users WHERE id = %d', [aId]);

  try
    Self.Query.ExecSQL;
    Result := Self.Query.RowsAffected > 0;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.ExistsById(aId: Integer): Boolean;
var
  helper: TDBHelper;
begin
  helper := TDBHelper.Create;
  Result := helper.CheckIfAlreadyExists('users', 'id', aId);
  helper.Free;
end;

function TUserRepository.FindAll: TObjectList<TUserModel>;
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

        users.Add(user);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.FindById(aId: Integer): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT * FROM users WHERE id = %d', [aId]);
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.Save(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
  helper: TDBHelper;
begin
  Result := nil;

  if aUser.id = 0 then begin
    Self.Query.SQL.Text := Format(
      'INSERT INTO users VALUES (DEFAULT, %s, %s, %s) RETURNING *',
      [aUser.name, aUser.login, aUser.GetHash]);
  end else begin
    Self.Query.SQL.Text := Format(
      'UPDATE users SET "name" = %s, login = %s, hash = %s WHERE id = %d RETURNING *',
      [aUser.name, aUser.login, aUser.GetHash, aUser.id]);
  end;

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('users', 'login', aUser.login, aUser.id) then
      raise Exception.Create(Format('Já existe um usuário com o LOGIN "%s"', [aUser.login]));

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      Result := user;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

end.

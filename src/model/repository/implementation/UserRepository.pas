unit UserRepository;

interface

uses
  RepositoryBase, CrudRepositoryInterface, UserModel, DBHelper, PermissionGroupRepository,
  System.SysUtils, System.Generics.Collections, Data.DB, System.StrUtils, System.Classes;

type TUserRepository = class(TRepositoryBase, ICrudRepository<TUserModel>)
  public
    function Save(aUser: TUserModel): TUserModel;
    function FindById(aId: Integer): TUserModel;
    function FindAll(): TObjectList<TUserModel>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
    function FindByLogin(aLogin: String): TUserModel;
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

        user.permissionGroup := nil;
        if not Self.Query.FieldByName('id_pgroup').IsNull then
          user.permissionGroup := TPermissionGroupRepository.GetInstance.FindById(
            Self.Query.FieldByName('id_pgroup').AsInteger
          );

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

      user.permissionGroup := nil;
      if not Self.Query.FieldByName('id_pgroup').IsNull then
        user.permissionGroup := TPermissionGroupRepository.GetInstance.FindById(
          Self.Query.FieldByName('id_pgroup').AsInteger
        );

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.FindByLogin(aLogin: String): TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT * FROM users WHERE login = %s',
    [QuotedStr(aLogin)]);

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      Result := TUserModel.Create;
      Result.id := Self.Query.FieldByName('id').AsInteger;
      Result.name := Self.Query.FieldByName('name').AsString;
      Result.login := Self.Query.FieldByName('login').AsString;
      Result.SetHash(Self.Query.FieldByName('hash').AsString);

      Result.permissionGroup := nil;
      if not Self.Query.FieldByName('id_pgroup').IsNull then
        Result.permissionGroup := TPermissionGroupRepository.GetInstance.FindById(
          Self.Query.FieldByName('id_pgroup').AsInteger
        );
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

  Self.Query.SQL.Text := Format(
    'INSERT INTO users(id, name, login, hash, id_pgroup) VALUES (%s, %s, %s, %s, %s) ' +
    'ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name, login = EXCLUDED.login, hash = EXCLUDED.hash, id_pgroup = EXCLUDED.id_pgroup ' +
    'RETURNING *',
    [IfThen(aUser.id = 0, 'DEFAULT', IntToStr(aUser.id)), QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(aUser.GetHash), IfThen(not Assigned(aUser.permissionGroup), 'NULL', IntToStr(aUser.permissionGroup.id))]
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
      if not Self.Query.FieldByName('id_pgroup').IsNull then
        user.permissionGroup := TPermissionGroupRepository.GetInstance.FindById(
          Self.Query.FieldByName('id_pgroup').AsInteger
        );

      Result := user;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

end.

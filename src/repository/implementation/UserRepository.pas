unit UserRepository;

interface

uses System.Generics.Collections, System.SysUtils,
  DBHelper, RepositoryBase, UserModel, UserRepositoryInterface,
  PermissionGroupRepositoryInterface;

type TUserRepository = class(TRepositoryBase, IUserRepository)
  private
    helper: TDBHelper;
    permissionGroupRepository: IPermissionGroupRepository;
  public
    function Insert(aUser: TUserModel): TUserModel;
    function Update(aUser: TUserModel): TUserModel;
    function FindById(aUserId: Integer): TUserModel;
    function FindAll(): TObjectList<TUserModel>;
    function ExistsById(aUserId: Integer): Boolean;
    function DeleteById(aUserId: Integer): Boolean;
    function FindByLogin(aLogin: String): TUserModel;
    constructor Create(aHelper: TDBHelper; aPermissionGroupRepository: IPermissionGroupRepository);
end;

implementation

{ TUserRepository }

constructor TUserRepository.Create(aHelper: TDBHelper;
  aPermissionGroupRepository: IPermissionGroupRepository);
begin
  Self.helper := aHelper;
  Self.permissionGroupRepository := aPermissionGroupRepository;
end;

function TUserRepository.DeleteById(aUserId: Integer): Boolean;
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

function TUserRepository.ExistsById(aUserId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('users', 'id', aUserId);
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
        user.permissionGroup := Self.permissionGroupRepository.FindById(
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

function TUserRepository.FindById(aUserId: Integer): TUserModel;
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
      user.permissionGroup := Self.permissionGroupRepository.FindById(
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

      Result.permissionGroup := Self.permissionGroupRepository.FindById(
        Self.Query.FieldByName('id_pgroup').AsInteger
      );
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.Insert(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO users(name, login, hash) VALUES (%s, %s, %s) RETURNING *',
    [QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(aUser.GetHash)]
  );

  try
    if Self.helper.CheckIfAlreadyExists('users', 'login', aUser.login) then
      raise Exception.Create('Já existe um usuário utilizando o login "' + aUser.login + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      user.permissionGroup := Self.permissionGroupRepository.FindById(
        Self.Query.FieldByName('id_pgroup').AsInteger
      );

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.Update(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
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

  try
    if Self.helper.CheckIfAlreadyExistsExcludingId('users', 'login', aUser.login, aUser.id) then
      raise Exception.Create('Já existe um usuário utilizando o login "' + aUser.login + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      user.permissionGroup := Self.permissionGroupRepository.FindById(
        Self.Query.FieldByName('id_pgroup').AsInteger
      );

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

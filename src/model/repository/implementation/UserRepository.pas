unit UserRepository;

interface

uses
  RepositoryBase, CrudRepositoryInterface, UserModel, DBHelper, FireDAC.Comp.Client, FireDAC.DApt,
  System.SysUtils, System.Generics.Collections, Data.DB, System.StrUtils, Permissions, System.Classes;

type TUserRepository = class(TRepositoryBase, ICrudRepository<TUserModel>)
  private
    function GetUserPermissions(aId: Integer): TPermissionsSet;
    procedure UpdatePermissions(aUser: TUserModel);
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

        user.permissions := Self.GetUserPermissions(user.id);

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

      user.permissions := Self.GetUserPermissions(user.id);

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
      Result.permissions := Self.GetUserPermissions(Result.id);
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.GetUserPermissions(aId: Integer): TPermissionsSet;
var
  permQuery: TFDQuery;
  permissions: TPermissionsSet;
  permId: Integer;
begin
  permQuery := TFDQuery.Create(Self.Query.Connection);
  permQuery.Connection := Self.Query.Connection;

  permQuery.SQL.Text := Format(
    'SELECT id_permission AS id FROM permissions_users ' +
    'WHERE id_user = %d',
    [aId]
  );

  try
    permQuery.Open();
    while not permQuery.Eof do begin
      permId := permQuery.FieldByName('id').AsInteger;
      permissions := permissions + [IntToPermission(permId)];

      permQuery.Next;
    end;

    Result := permissions;
  finally
    permQuery.Close;
    permQuery.Free;
  end;
end;

function TUserRepository.Save(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO users VALUES (%s, %s, %s, %s) ' +
    'ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name, login = EXCLUDED.login, hash = EXCLUDED.hash ' +
    'RETURNING *',
    [IfThen(aUser.id = 0, 'DEFAULT', IntToStr(aUser.id)), QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(aUser.GetHash)]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('users', 'login', aUser.login, aUser.id) then
      raise Exception.Create('Já existe um usuário utilizando o login "' + aUser.login + '"');

    Self.Query.Open();

    Self.UpdatePermissions(aUser);

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;
      user.SetHash(Self.Query.FieldByName('hash').AsString);

      user.permissions := Self.GetUserPermissions(user.id);

      Result := user;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

procedure TUserRepository.UpdatePermissions(aUser: TUserModel);
var
  pQuery: TFDQuery;
  perm: TPermissions;
  arguments: TStringList;
begin
  arguments := TStringList.Create;
  arguments.QuoteChar := #0;

  for perm in aUser.permissions do begin
    arguments.Add(Format('(%d, %d)', [aUser.id, Integer(perm)]));
  end;

  pQuery := TFDQuery.Create(Self.Query.Connection);
  pQuery.Connection := Self.Query.Connection;
  pQuery.Connection.StartTransaction;

  try
    try
      pQuery.SQL.Text := Format(
        'DELETE FROM permissions_users WHERE id_user = %d',
        [aUser.id]
      );
      pQuery.ExecSQL;

      if arguments.Count > 0 then begin
        pQuery.SQL.Text := Format(
          'INSERT INTO permissions_users (id_user, id_permission) VALUES %s',
          [arguments.DelimitedText]
        );
        pQuery.ExecSQL;
      end;

      pQuery.Connection.Commit;
    except
      on e: Exception do begin
        pQuery.Connection.Rollback;
        raise Exception.Create('Erro ao atualizar as permissões do usuário ' + aUser.name + ': ' + e.Message);
      end;
    end;
  finally
    arguments.Free;
    pQuery.Free;
  end;
end;

end.

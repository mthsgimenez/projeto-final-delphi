unit UserRepository;

interface

uses CrudRepositoryInterface, UserModel, RepositoryBase, Bcrypt, DBHelper, System.SysUtils;

type TUserRepository = class(TRepositoryBase, TInterfacedObject, ICrudRepository<TUserModel>)
  public
    function Login(aLogin: String; aPassword: String): TUserModel;
    function RegisterUser(aUser: TUserModel; aPassword: String): TUserModel;
    function Save(aUser: TUserModel): TUserModel;
    function FindById(aId: Integer): TUserModel;
end;

implementation

{ TUserRepository }

function TUserRepository.FindById(aId: Integer): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT id, "name", login FROM users WHERE id=%d', [aId]);
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.Login(aLogin, aPassword: String): TUserModel;
var
  hash: String;
  rehashNeeded: Boolean;
begin
  Self.Query.SQL.Text := 'SELECT * FROM users WHERE login = ' + QuotedStr(aLogin);

  try
    Self.Query.Open;
    if Self.Query.RecordCount < 1 then raise Exception.Create('Usuário não encontrado');

    hash := Self.Query.FieldByName('hash').AsString;

    if not TBCrypt.CheckPassword(aPassword, hash, rehashNeeded) then
      raise Exception.Create('Senha incorreta');

    Result := TUserModel.Create;
    Result.id := Self.Query.FieldByName('id').AsInteger;
    Result.name := Self.Query.FieldByName('name').AsString;
    Result.login := Self.Query.FieldByName('login').AsString;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.Save(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('UPDATE users SET "name"=%s WHERE id=%d RETURNING id, "name", login',
                          [QuotedStr(aUser.name), QuotedStr(aUser.login), aUser.id]);

  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TUserRepository.RegisterUser(aUser: TUserModel; aPassword: String): TUserModel;
var
  hash: String;
  user: TUserModel;
begin
  Result := nil;

  hash := TBCrypt.HashPassword(aPassword, 12);

  Self.Query.SQL.Text := Format('INSERT INTO users ("name", login, hash) VALUES (%s, %s, %s) RETURNING id, "name", login',
                          [QuotedStr(aUser.name), QuotedStr(aUser.login), QuotedStr(hash)]);
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      user := TUserModel.Create;
      user.id := Self.Query.FieldByName('id').AsInteger;
      user.name := Self.Query.FieldByName('name').AsString;
      user.login := Self.Query.FieldByName('login').AsString;

      Result := user;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

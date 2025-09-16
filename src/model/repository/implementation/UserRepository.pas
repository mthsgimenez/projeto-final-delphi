unit UserRepository;

interface

uses UserModel, DBConnection, FireDAC.Comp.Client, Bcrypt, System.SysUtils, FireDAC.DApt;

type TUserRepository = class
  private
    Query: TFDQuery;
    Connection: TFDConnection;
  public
    constructor Create;
    destructor Destroy; override;
    function Login(aLogin: String; aPassword: String): TUserModel;
end;

implementation

{ TUserRepository }

constructor TUserRepository.Create;
begin
  inherited Create;
  Self.Connection := DBConnection.Connection.FDConnection;
  Self.Query := TFDQuery.Create(Connection);
  Self.Query.Connection := Self.Connection;
end;

destructor TUserRepository.Destroy;
begin
  Self.Query.Free;
  inherited Destroy;
end;

function TUserRepository.Login(aLogin, aPassword: String): TUserModel;
var
  hash: String;
  rehashNeeded: Boolean;
begin
  Self.Query.SQL.Text := 'SELECT * FROM users WHERE login = ' + QuotedStr(aLogin);
  Self.Query.Open;

  try
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

end.

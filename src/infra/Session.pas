unit Session;

interface

uses UserModel, Logging, System.SysUtils;

type TSession = class
  private
    loggedUser: TUserModel;
    class var instance: TSession;
    constructor Create;
  public
    class function GetInstance: TSession;
    function GetUser: TUserModel;
    procedure setUser(aUser: TUserModel);
    destructor Destroy; override;
end;

implementation

{ TSession }

constructor TSession.Create;
begin

end;

destructor TSession.Destroy;
begin
  Self.loggedUser.Free;
  inherited Destroy;
end;

class function TSession.GetInstance: TSession;
begin
  if instance = nil then
    instance := TSession.Create;

  Result := instance;
end;

function TSession.GetUser: TUserModel;
begin
  Result := Self.loggedUser;
end;

procedure TSession.setUser(aUser: TUserModel);
begin
  TLogger.GetLogger.Debug('Sess�o iniciada com usu�rio: ' + Format('%d %s', [aUser.id, aUser.login]));

  Self.loggedUser := aUser;
end;

end.

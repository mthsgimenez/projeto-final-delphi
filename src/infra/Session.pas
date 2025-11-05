unit Session;

interface

uses UserModel, Logging, System.SysUtils;

type TSession = class
  private
    class var loggedUser: TUserModel;
    constructor Create;
  public
    class procedure setUser(aUser: TUserModel);
    class function GetUser: TUserModel;
end;

implementation

{ TSession }

constructor TSession.Create;
begin
end;

class function TSession.GetUser: TUserModel;
begin
  Result := loggedUser;
end;

class procedure TSession.setUser(aUser: TUserModel);
begin
  TLogger.GetLogger.Debug('Sessão iniciada com usuário: ' + Format('%d %s', [aUser.id, aUser.login]));

  if Assigned(loggedUser) then
    loggedUser.Free;

  loggedUser := aUser;
end;

end.

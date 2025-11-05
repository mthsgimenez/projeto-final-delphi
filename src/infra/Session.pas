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
  if Assigned(loggedUser) then
    loggedUser.Free;

  loggedUser := aUser;

  TLogger.GetLogger.Debug(Format(
    'Usuário salvo em TSession: (ID: %d) %s',
    [aUser.id, aUser.name]
  ));
end;

end.

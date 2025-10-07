unit Session;

interface

uses UserModel;

type TSession = class
  private
    loggedUser: TUserModel;
    class var instance: TSession;
    constructor Create;
  public
    class function GetInstance: TSession;
    function GetUser: TUserModel;
    procedure setUser(aUser: TUserModel);
end;

implementation

{ TSession }

constructor TSession.Create;
begin

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
  Self.loggedUser := aUser;
end;

end.

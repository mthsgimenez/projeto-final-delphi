unit UserController;

interface

uses UserRepository, UserModel, UserDTO;

  type TUserController = class
    private
      repository: TUserRepository;
    public
      constructor Create;
      destructor Destroy; override;
      function Login(aUser: TUserDTO): TUserModel;
  end;

implementation

{ TUserController }

constructor TUserController.Create;
begin
  inherited Create;
  Self.repository := TUserRepository.Create;
end;

destructor TUserController.Destroy;
begin
  Self.repository.Free;
  inherited Destroy;
end;

function TUserController.Login(aUser: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  user := Self.repository.FindByLogin(aUser.login);
  if not (user = nil) then
    if user.CheckPassword(aUser.password) then Result := user;
end;

end.

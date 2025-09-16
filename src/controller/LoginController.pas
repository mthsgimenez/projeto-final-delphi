unit LoginController;

interface

uses UserModel, UserRepository;

type TLoginController = class
  private
    repository: TUserRepository;
  public
    constructor Create;
    destructor Destroy; override;
    function Login(aLogin: String; aPassword: String): TUserModel;
end;

implementation

{ TLoginController }

constructor TLoginController.Create;
begin
  inherited Create;
  Self.repository := TUserRepository.Create;
end;

destructor TLoginController.Destroy;
begin
  Self.repository.Free;
  inherited Destroy;
end;

function TLoginController.Login(aLogin, aPassword: String): TUserModel;
begin
  Result := Self.repository.Login(aLogin, aPassword);
end;

end.

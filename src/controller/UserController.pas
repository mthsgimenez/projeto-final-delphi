unit UserController;

interface

uses UserRepository, UserModel, UserDTO, System.Generics.Collections;

  type TUserController = class
    private
      repository: TUserRepository;
    public
      constructor Create;
      destructor Destroy; override;
      function Login(aUser: TUserDTO): TUserModel;
      function EditUser(aId: Integer; aData: TUserDTO): TUserModel;
      function GetUsers: TObjectList<TUserModel>;
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

function TUserController.EditUser(aId: Integer; aData: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  user := Self.repository.FindById(aId);
  if not (user = nil) then begin
    user.name := aData.name;
    user.login := aData.login;
    user.SetPassword(aData.password);
    Self.repository.Save(user);
  end;
  Result := user;
end;

function TUserController.GetUsers: TObjectList<TUserModel>;
begin
  Result := Self.repository.FindAll;
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

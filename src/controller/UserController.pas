unit UserController;

interface

uses UserRepository, UserModel, UserDTO, System.Generics.Collections, System.SysUtils;

  type TUserController = class
    private
      repository: TUserRepository;
    public
      constructor Create;
      destructor Destroy; override;
      function Login(aUser: TUserDTO): TUserModel;
      function EditUser(aId: Integer; aData: TUserDTO): TUserModel;
      function GetUser(aId: Integer): TUserModel;
      function GetUsers: TObjectList<TUserModel>;
      function DeleteUser(aId: Integer): Boolean;
      function CreateUser(aUser: TUserDTO): TUserModel;
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

function TUserController.CreateUser(aUser: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  user := TUserModel.Create;
  try
    user.name := aUser.name;
    user.login := aUser.login;
    user.SetPassword(Trim(aUser.password));
    user.permissions := aUser.permissions;

    Result := Self.repository.Save(user);
  finally
    user.Free;
  end;
end;

function TUserController.DeleteUser(aId: Integer): Boolean;
begin
  Result := Self.repository.DeleteById(aId);
end;

function TUserController.EditUser(aId: Integer; aData: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  user := Self.repository.FindById(aId);

  try
    user.name := aData.name;
    user.login := aData.login;
    user.permissions := aData.permissions;
    if Trim(aData.password) <> '' then user.SetPassword(Trim(aData.password));

    Result := Self.repository.Save(user);
  finally
    user.Free;
  end;
end;

function TUserController.GetUser(aId: Integer): TUserModel;
begin
  Result := Self.repository.FindById(aId);
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
  if not (user = nil) then begin
    if user.CheckPassword(aUser.password) then begin
      Result := user;
      Exit;
    end;

    user.Free;
  end;
end;

end.

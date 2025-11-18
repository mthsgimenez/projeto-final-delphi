unit UserController;

interface

uses UserRepositoryInterface, UserModel, UserDTO, System.Generics.Collections, System.SysUtils, Logging, Session;

  type TUserController = class
    private
      userRepository: IUserRepository;
    public
      constructor Create(aUserRepository: IUserRepository);
      function Login(aUser: TUserDTO): TUserModel;
      function EditUser(aId: Integer; aData: TUserDTO): TUserModel;
      function GetUser(aId: Integer): TUserModel;
      function GetUsers: TObjectList<TUserModel>;
      function DeleteUser(aId: Integer): Boolean;
      function CreateUser(aUser: TUserDTO): TUserModel;
  end;

implementation

{ TUserController }

constructor TUserController.Create(aUserRepository: IUserRepository);
begin
  inherited Create;
  Self.userRepository := aUserRepository;
end;

function TUserController.CreateUser(aUser: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  user := TUserModel.Create;
  try
    user.name := aUser.name;
    user.login := aUser.login;
    user.SetPassword(Trim(aUser.password));

    Result := Self.userRepository.Insert(user);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Usuário criado: Usuário (ID: %d) %s cadastrou o usuário (ID: %d)',
        [TSession.GetUser.id, TSession.GetUser.name, Result.id]
      ));
  finally
    user.Free;
  end;
end;

function TUserController.DeleteUser(aId: Integer): Boolean;
begin
  Result := Self.userRepository.DeleteById(aId);
  if Result then
    TLogger.GetLogger.Info(Format(
      'Usuário deletado: Usuário (ID: %d) %s deletou o usuário (ID: %d)',
      [TSession.GetUser.id, TSession.GetUser.name, aId]
    ));

end;

function TUserController.EditUser(aId: Integer; aData: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  user := Self.userRepository.FindById(aId);

  try
    user.name := aData.name;
    user.login := aData.login;
    if Trim(aData.password) <> '' then user.SetPassword(Trim(aData.password));

    Result := Self.userRepository.Update(user);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Usuário editado: Usuário (ID: %d) %s editou o usuário (ID: %d)',
        [TSession.GetUser.id, TSession.GetUser.name, aId]
      ));
  finally
    user.Free;
  end;
end;

function TUserController.GetUser(aId: Integer): TUserModel;
begin
  Result := Self.userRepository.FindById(aId);
end;

function TUserController.GetUsers: TObjectList<TUserModel>;
begin
  Result := Self.userRepository.FindAll;
end;

function TUserController.Login(aUser: TUserDTO): TUserModel;
var
  user: TUserModel;
begin
  Result := nil;

  user := Self.userRepository.FindByLogin(aUser.login);
  if not (user = nil) then begin
    if user.CheckPassword(aUser.password) then begin
      Result := user;
      Exit;
    end;

    user.Free;
  end;
end;

end.

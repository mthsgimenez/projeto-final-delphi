unit Dependencies;

interface

uses PermissionGroupDAOInterface, PermissionGroupDAO, PermissionGroupRepository, PermissionsController,
  UserDAOInterface, UserDAO, UserRepository, UserController;

type TDependencies = class
  private
    userDAO: IUserDAO;
    userRepository: TUserRepository;
    userController: TUserController;

    permissionDAO: IPermissionGroupDAO;
    permissionRepository: TPermissionGroupRepository;
    permissionController: TPermissionController;

    class var instance: TDependencies;
    constructor Create;
  public
    function GetUserController: TUserController;
    function GetPermissionController: TPermissionController;
    class function GetInstance: TDependencies;
    destructor Destroy; override;
end;

implementation

{ TDependencies }

constructor TDependencies.Create;
begin
  inherited;
end;

destructor TDependencies.Destroy;
begin
  if Assigned(Self.permissionController) then begin
    Self.permissionController.Free;
    Self.permissionRepository.Free;
  end;

  if Assigned(Self.userController) then begin
    Self.userController.Free;
    Self.userRepository.Free;
  end;

  inherited;
end;

class function TDependencies.GetInstance: TDependencies;
begin
  if not Assigned(instance) then
    instance := TDependencies.Create;

  Result := instance;
end;

function TDependencies.GetPermissionController: TPermissionController;
begin
  if not Assigned(Self.permissionController) then begin
    if not Assigned(Self.permissionDAO) then
      Self.permissionDAO := TPermissionGroupDAO.Create;
    Self.permissionRepository := TPermissionGroupRepository.Create(Self.permissionDAO);
    Self.permissionController := TPermissionController.Create(Self.permissionRepository);
  end;

  Result := Self.permissionController;
end;

function TDependencies.GetUserController: TUserController;
begin
  if not Assigned(Self.userController) then begin
    if not Assigned(Self.permissionDAO) then
      Self.permissionDAO := TPermissionGroupDAO.Create;

    Self.userDAO := TUserDAO.Create;
    Self.userRepository := TUserRepository.Create(Self.userDAO, Self.permissionDAO);
    Self.userController := TUserController.Create(Self.userRepository);
  end;

  Result := Self.userController;
end;

end.

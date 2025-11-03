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

    procedure CreateUserRepository;
    procedure CreatePermissionRepository;

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

procedure TDependencies.CreatePermissionRepository;
begin
  if not Assigned(Self.permissionDAO) then
    Self.permissionDAO := TPermissionGroupDAO.Create;

  if not Assigned(Self.permissionRepository) then
    Self.permissionRepository := TPermissionGroupRepository.Create(Self.permissionDAO);
end;

procedure TDependencies.CreateUserRepository;
begin
  if not Assigned(Self.userDAO) then
    Self.userDAO := TUserDAO.Create;

  if not Assigned(Self.permissionDAO) then
    Self.permissionDAO := TPermissionGroupDAO.Create;

  if not Assigned(Self.userRepository) then
    Self.userRepository := TUserRepository.Create(Self.userDAO, Self.permissionDAO);
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
    if not Assigned(Self.permissionRepository) then
      Self.CreatePermissionRepository;
    if not Assigned(Self.userRepository) then
      Self.CreateUserRepository;

    Self.permissionController := TPermissionController.Create(Self.permissionRepository, Self.userRepository);
  end;

  Result := Self.permissionController;
end;

function TDependencies.GetUserController: TUserController;
begin
  if not Assigned(Self.userController) then begin
    if not Assigned(Self.userRepository) then
      Self.CreateUserRepository;

    Self.userController := TUserController.Create(Self.userRepository);
  end;

  Result := Self.userController;
end;

end.

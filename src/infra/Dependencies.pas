unit Dependencies;

interface

uses PermissionGroupDAOInterface, PermissionGroupDAO, PermissionGroupRepository, PermissionsController,
  UserDAOInterface, UserDAO, UserRepository, UserController,
  SupplierDAOInterface, SupplierDAO, SupplierRepository, SupplierController, CNPJApiInterface, ImplCnpja,
  ToolTypeDAOInterface, ToolTypeDAO, ToolTypeRepository, ToolTypeController;

type TDependencies = class
  private
    userDAO: IUserDAO;
    userRepository: TUserRepository;
    userController: TUserController;

    permissionDAO: IPermissionGroupDAO;
    permissionRepository: TPermissionGroupRepository;
    permissionController: TPermissionController;

    cnpjApi: ICNPJApi;

    supplierDAO: ISupplierDAO;
    supplierRepository: TSupplierRepository;
    supplierController: TSupplierController;

    toolTypeDAO: IToolTypeDAO;
    toolTypeRepository: TToolTypeRepository;
    toolTypeController: TToolTypeController;

    procedure CreateUserRepository;
    procedure CreatePermissionRepository;
    procedure CreateSupplierRepository;
    procedure CreateToolTypeRepository;

    class var instance: TDependencies;
    constructor Create;
  public
    function GetUserController: TUserController;
    function GetPermissionController: TPermissionController;
    function GetSupplierController: TSupplierController;
    function GetToolTypeController: TToolTypeController;

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

procedure TDependencies.CreateSupplierRepository;
begin
  if not Assigned(Self.supplierDAO) then
    Self.supplierDAO := TSupplierDAO.Create;

  if not Assigned(Self.cnpjApi) then
    Self.cnpjApi := TCNPJA.Create;

  if not Assigned(Self.supplierRepository) then
    Self.supplierRepository := TSupplierRepository.Create(Self.supplierDAO, Self.cnpjApi);
end;

procedure TDependencies.CreateToolTypeRepository;
begin
  if not Assigned(Self.supplierDAO) then
    Self.supplierDAO := TSupplierDAO.Create;

  if not Assigned(Self.toolTypeDAO) then
    Self.toolTypeDAO := TToolTypeDAO.Create;

  if not Assigned(Self.toolTypeRepository) then
    Self.toolTypeRepository := TToolTypeRepository.Create(Self.toolTypeDAO, Self.supplierDAO);
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
  if Assigned(Self.userController) then Self.userController.Free;
  if Assigned(Self.permissionController) then Self.permissionController.Free;
  if Assigned(Self.supplierController) then Self.supplierController.Free;
  if Assigned(Self.toolTypeController) then Self.toolTypeController.Free;

  if Assigned(Self.userRepository) then Self.userRepository.Free;
  if Assigned(Self.permissionRepository) then Self.permissionRepository.Free;
  if Assigned(Self.supplierRepository) then Self.supplierRepository.Free;
  if Assigned(Self.toolTypeRepository) then Self.toolTypeRepository.Free;
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

function TDependencies.GetSupplierController: TSupplierController;
begin
  if not Assigned(Self.supplierController) then begin
    if not Assigned(Self.supplierRepository) then
      Self.CreateSupplierRepository;

    Self.supplierController := TSupplierController.Create(Self.supplierRepository);
  end;

  Result := Self.supplierController;
end;

function TDependencies.GetToolTypeController: TToolTypeController;
begin
  if not Assigned(Self.toolTypeController) then begin
    if not Assigned(Self.toolTypeRepository) then
      Self.CreateToolTypeRepository;

    if not Assigned(Self.supplierRepository) then
      Self.CreateSupplierRepository;

    Self.toolTypeController := TToolTypeController.Create(Self.toolTypeRepository, Self.supplierRepository);
  end;

  Result := Self.toolTypeController;
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

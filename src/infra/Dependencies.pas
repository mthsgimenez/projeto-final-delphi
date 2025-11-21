unit Dependencies;

interface

uses DBHelper,
  PermissionGroupRepositoryInterface, PermissionGroupRepository, PermissionsController,
  UserRepositoryInterface, UserRepository, UserController,
  SupplierRepositoryInterface, SupplierRepository, SupplierController,
  CNPJApiInterface, ImplCnpja,
  ToolTypeRepositoryInterface, ToolTypeRepository, ToolTypeController,
  StorageRepositoryInterface, StorageRepository, StorageController,
  ToolRepositoryInterface, ToolRepository,
  PurchaseOrderRepositoryInterface, PurchaseOrderRepository, PurchaseOrderController,
  ReportRepositoryInterface, ReportRepository, ReportController;

type
  TDependencies = class
  private
    helper: TDBHelper;

    userRepository: IUserRepository;
    userController: TUserController;

    permissionRepository: IPermissionGroupRepository;
    permissionController: TPermissionController;

    cnpjApi: ICNPJApi;

    supplierRepository: ISupplierRepository;
    supplierController: TSupplierController;

    toolTypeRepository: IToolTypeRepository;
    toolTypeController: TToolTypeController;

    storageRepository: IStorageRepository;
    storageController: TStorageController;

    toolRepository: IToolRepository;

    purchaseOrderRepository: IPurchaseOrderRepository;
    purchaseOrderController: TPurchaseOrderController;

    reportRepository: IReportRepository;
    reportController: TReportController;

    class var instance: TDependencies;
    constructor Create;
  public
    function GetUserController: TUserController;
    function GetPermissionController: TPermissionController;
    function GetSupplierController: TSupplierController;
    function GetToolTypeController: TToolTypeController;
    function GetStorageController: TStorageController;
    function GetPurchaseOrderController: TPurchaseOrderController;
    function GetReportController: TReportController;

    class function GetInstance: TDependencies;
    destructor Destroy; override;
  end;

implementation

{ TDependencies }

constructor TDependencies.Create;
begin
  Self.helper := TDBHelper.Create;

  Self.permissionRepository := TPermissionGroupRepository.Create(Self.helper);
  Self.userRepository := TUserRepository.Create(Self.helper, Self.permissionRepository);

  Self.cnpjApi := TCNPJA.Create;
  Self.supplierRepository := TSupplierRepository.Create(Self.helper, Self.cnpjApi);

  Self.toolTypeRepository := TToolTypeRepository.Create(Self.helper, Self.supplierRepository);
  Self.storageRepository := TStorageRepository.Create(Self.helper, Self.toolTypeRepository);
  Self.toolRepository := TToolRepository.Create(Self.helper, Self.toolTypeRepository, Self.storageRepository);

  Self.purchaseOrderRepository := TPurchaseOrderRepository.Create(Self.helper, Self.toolTypeRepository, Self.supplierRepository);

  Self.reportRepository := TReportRepository.Create;
end;

destructor TDependencies.Destroy;
begin
  if Assigned(Self.userController) then Self.userController.Free;
  if Assigned(Self.permissionController) then Self.permissionController.Free;
  if Assigned(Self.supplierController) then Self.supplierController.Free;
  if Assigned(Self.toolTypeController) then Self.toolTypeController.Free;
  if Assigned(Self.storageController) then Self.storageController.Free;
  if Assigned(Self.purchaseOrderController) then Self.purchaseOrderController.Free;
  if Assigned(Self.reportController) then Self.reportController.Free;


  Self.helper.Free;
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
  if not Assigned(Self.permissionController) then
    Self.permissionController := TPermissionController.Create(Self.permissionRepository, Self.userRepository);

  Result := Self.permissionController;
end;

function TDependencies.GetPurchaseOrderController: TPurchaseOrderController;
begin
  if not Assigned(Self.purchaseOrderController) then
    Self.purchaseOrderController := TPurchaseOrderController.Create(Self.purchaseOrderRepository, Self.toolRepository);

  Result := Self.purchaseOrderController;
end;

function TDependencies.GetReportController: TReportController;
begin
  if not Assigned(Self.reportController) then
    Self.reportController := TReportController.Create(Self.reportRepository);

  Result := Self.reportController;
end;

function TDependencies.GetStorageController: TStorageController;
begin
  if not Assigned(Self.storageController) then
    Self.storageController := TStorageController.Create(Self.storageRepository);

  Result := Self.storageController;
end;

function TDependencies.GetSupplierController: TSupplierController;
begin
  if not Assigned(Self.supplierController) then
    Self.supplierController := TSupplierController.Create(Self.supplierRepository);

  Result := Self.supplierController;
end;

function TDependencies.GetToolTypeController: TToolTypeController;
begin
  if not Assigned(Self.toolTypeController) then
    Self.toolTypeController := TToolTypeController.Create(Self.toolTypeRepository, Self.supplierRepository);

  Result := Self.toolTypeController;
end;


function TDependencies.GetUserController: TUserController;
begin
  if not Assigned(Self.userController) then
    Self.userController := TUserController.Create(Self.userRepository);

  Result := Self.userController;
end;

end.

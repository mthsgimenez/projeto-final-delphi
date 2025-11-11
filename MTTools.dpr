program MTTools;

uses
  Vcl.Forms,
  DBConfigDTO in 'src\DTO\DBConfigDTO.pas',
  ConfigController in 'src\controller\ConfigController.pas',
  Bcrypt in 'lib\Bcrypt.pas',
  DBHelper in 'src\util\DBHelper.pas',
  DAOBase in 'src\util\DAOBase.pas',
  UserDTO in 'src\DTO\UserDTO.pas',
  UserController in 'src\controller\UserController.pas',
  Session in 'src\infra\Session.pas',
  MessageHelper in 'src\util\MessageHelper.pas',
  Logging in 'src\infra\Logging.pas',
  PermissionsController in 'src\controller\PermissionsController.pas',
  DBConfigModel in 'src\model\DBConfigModel.pas',
  PermissionGroupModel in 'src\model\PermissionGroupModel.pas',
  Permissions in 'src\model\Permissions.pas',
  UserModel in 'src\model\UserModel.pas',
  DBConfigRepositoryJSON in 'src\repository\implementation\DBConfigRepositoryJSON.pas',
  UserRepository in 'src\repository\implementation\UserRepository.pas',
  CrudRepositoryInterface in 'src\repository\interface\CrudRepositoryInterface.pas',
  PermissionGroupDTO in 'src\DTO\PermissionGroupDTO.pas',
  DBConfigRepositoryInterface in 'src\repository\interface\DBConfigRepositoryInterface.pas',
  DAOInterface in 'src\DAO\interface\DAOInterface.pas',
  PermissionGroupDAO in 'src\DAO\implementation\PermissionGroupDAO.pas',
  UserDAO in 'src\DAO\implementation\UserDAO.pas',
  PermissionGroupRepository in 'src\repository\implementation\PermissionGroupRepository.pas',
  UserDAOInterface in 'src\DAO\interface\UserDAOInterface.pas',
  PermissionGroupDAOInterface in 'src\DAO\interface\PermissionGroupDAOInterface.pas',
  Dependencies in 'src\infra\Dependencies.pas',
  StorageDAO in 'src\DAO\implementation\StorageDAO.pas',
  StorageModel in 'src\model\StorageModel.pas',
  StorageRepository in 'src\repository\implementation\StorageRepository.pas',
  StorageController in 'src\controller\StorageController.pas',
  StorageDTO in 'src\DTO\StorageDTO.pas',
  StorageDAOInterface in 'src\DAO\interface\StorageDAOInterface.pas',
  SupplierModel in 'src\model\SupplierModel.pas',
  CNPJ in 'src\model\CNPJ.pas',
  CNPJApiInterface in 'src\DAO\interface\CNPJApiInterface.pas',
  ImplCnpja in 'src\DAO\implementation\ImplCnpja.pas',
  SupplierDAO in 'src\DAO\implementation\SupplierDAO.pas',
  SupplierRepository in 'src\repository\implementation\SupplierRepository.pas',
  SupplierController in 'src\controller\SupplierController.pas',
  SupplierDAOInterface in 'src\DAO\interface\SupplierDAOInterface.pas',
  SupplierDTO in 'src\DTO\SupplierDTO.pas',
  ToolTypeModel in 'src\model\ToolTypeModel.pas',
  ToolTypeDAOInterface in 'src\DAO\interface\ToolTypeDAOInterface.pas',
  ToolTypeDAO in 'src\DAO\implementation\ToolTypeDAO.pas',
  ToolTypeRepository in 'src\repository\implementation\ToolTypeRepository.pas',
  ToolTypeController in 'src\controller\ToolTypeController.pas',
  ToolTypeDTO in 'src\DTO\ToolTypeDTO.pas',
  LoginView in 'src\view\LoginView.pas' {formLogin},
  DBConnection in 'src\infra\DBConnection.pas' {Connection: TDataModule},
  MainView in 'src\view\MainView.pas' {formMain},
  DBConfigView in 'src\view\DBConfigView.pas' {formDBConfig},
  UserView in 'src\view\UserView.pas' {formUser},
  MenuView in 'src\view\MenuView.pas' {formMenu},
  PermissionsView in 'src\view\PermissionsView.pas' {formPermissions},
  SupplierView in 'src\view\SupplierView.pas' {formSupplier},
  ToolTypeView in 'src\view\ToolTypeView.pas' {formToolType};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnection, Connection);
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.

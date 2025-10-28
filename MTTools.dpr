program MTTools;

uses
  Vcl.Forms,
  LoginView in 'src\view\LoginView.pas' {formLogin},
  DBConnection in 'src\infra\DBConnection.pas' {Connection: TDataModule},
  DBConfigDTO in 'src\DTO\DBConfigDTO.pas',
  ConfigController in 'src\controller\ConfigController.pas',
  MainView in 'src\view\MainView.pas' {formMain},
  DBConfigView in 'src\view\DBConfigView.pas' {formDBConfig},
  Bcrypt in 'lib\Bcrypt.pas',
  DBHelper in 'src\util\DBHelper.pas',
  DAOBase in 'src\util\DAOBase.pas',
  UserDTO in 'src\DTO\UserDTO.pas',
  UserController in 'src\controller\UserController.pas',
  UserView in 'src\view\UserView.pas' {formUser},
  MenuView in 'src\view\MenuView.pas' {formMenu},
  Session in 'src\infra\Session.pas',
  MessageHelper in 'src\util\MessageHelper.pas',
  Logging in 'src\infra\Logging.pas',
  PermissionsView in 'src\view\PermissionsView.pas' {formPermissions},
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
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnection, Connection);
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.

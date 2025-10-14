program MTTools;

uses
  Vcl.Forms,
  LoginView in 'src\view\LoginView.pas' {formLogin},
  DBConnection in 'src\infra\DBConnection.pas' {Connection: TDataModule},
  DBConfigDTO in 'src\DTO\DBConfigDTO.pas',
  DBConfigRepositoryInterface in 'src\model\repository\interface\DBConfigRepositoryInterface.pas',
  DBConfigModel in 'src\model\entity\DBConfigModel.pas',
  DBConfigRepositoryJSON in 'src\model\repository\implementation\DBConfigRepositoryJSON.pas',
  ConfigController in 'src\controller\ConfigController.pas',
  MainView in 'src\view\MainView.pas' {formMain},
  DBConfigView in 'src\view\DBConfigView.pas' {formDBConfig},
  Bcrypt in 'lib\Bcrypt.pas',
  UserModel in 'src\model\entity\UserModel.pas',
  UserRepository in 'src\model\repository\implementation\UserRepository.pas',
  DBHelper in 'src\util\DBHelper.pas',
  RepositoryBase in 'src\util\RepositoryBase.pas',
  CrudRepositoryInterface in 'src\model\repository\interface\CrudRepositoryInterface.pas',
  UserDTO in 'src\DTO\UserDTO.pas',
  UserController in 'src\controller\UserController.pas',
  UserView in 'src\view\UserView.pas' {formUser},
  MenuView in 'src\view\MenuView.pas' {formMenu},
  Permissions in 'src\model\entity\Permissions.pas',
  Session in 'src\infra\Session.pas',
  MessageHelper in 'src\util\MessageHelper.pas',
  Logging in 'src\infra\Logging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnection, Connection);
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.

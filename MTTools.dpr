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
  LoginController in 'src\controller\LoginController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnection, Connection);
  Application.CreateForm(TformMain, formMain);
  Application.CreateForm(TformLogin, formLogin);
  Application.CreateForm(TformDBConfig, formDBConfig);
  Application.Run;
end.

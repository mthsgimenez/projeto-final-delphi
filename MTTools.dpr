program MTTools;

uses
  Vcl.Forms,
  LoginView in 'src\view\LoginView.pas' {formLogin},
  DBConnection in 'src\infra\DBConnection.pas' {Connection: TDataModule},
  DBConfigDTO in 'src\DTO\DBConfigDTO.pas',
  DBConfigRepositoryInterface in 'src\model\repository\interface\DBConfigRepositoryInterface.pas',
  DBConfigModel in 'src\model\entity\DBConfigModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnection, Connection);
  Application.CreateForm(TformLogin, formLogin);
  Application.Run;
end.

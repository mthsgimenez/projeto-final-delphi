unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, Vcl.ExtCtrls, ConfigController, DBConfigView, LoginView,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.Imaging.pngimage, UserModel, MenuView, Session, Logging, MessageHelper, PermissionGroupRepository, Dependencies;

type
  TformMain = class(TForm)
    panelMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    configController: TConfigController;
    activeForm: TForm;
    procedure OnLogin(user: TUserModel);
    procedure OpenForm(aForm: TForm);
    procedure OnLogout;
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

procedure TformMain.OpenForm(aForm: TForm);
begin
  Self.activeForm := aForm;
  Self.activeForm.Parent := Self.panelMain;
  Self.activeForm.BorderStyle := bsNone;
  Self.activeForm.Show;
end;

procedure TformMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TLogger.GetLogger.Info('Encerrando o sistema');
  TLogger.GetLogger.Free;
  TMessageHelper.GetInstance.Free;
  if TSession.GetUser <> nil then
    TSession.GetUser.Free;
  TDependencies.GetInstance.Free;
  Self.configController.Free;
  Self.activeForm.Free;
end;

procedure TformMain.FormCreate(Sender: TObject);
var
  configForm: TformDBConfig;
  loginForm: TformLogin;
begin
  ReportMemoryLeaksOnShutdown := True;

  Self.activeForm := nil;

  Self.configController := TConfigController.Create;
  Self.configController.PrepareDirectory;

  TLogger.GetLogger.setLevel(TLogLevels.DEBUG);

  TLogger.GetLogger.Info('Iniciando o sistema');

  try
    TLogger.GetLogger.Info('Carregando arquivo de configuração do banco');
    Self.configController.LoadDBConfig;
  except
    on e: Exception do begin
      ShowMessage(e.Message);

      TLogger.GetLogger.Warn('Não foi possível carregar a configuração, exibindo formulário para configuração manual');
      configForm := TformDBConfig.Create(Self.configController, Self);
      configForm.ShowModal;
      configForm.Free;
    end;
  end;

  if Self.configController.IsDBConnected then begin
    TLogger.GetLogger.Info('Exibindo formulário de login');
    loginForm := TformLogin.Create(Self.panelMain, Self.OnLogin);
    Self.OpenForm(loginForm);
  end else begin
    TLogger.GetLogger.Warn('Encerrando o sistema pois não foi possível se conectar ao banco');
    Application.Terminate;
  end;
end;

procedure TformMain.OnLogin(user: TUserModel);
const
  closedWidth = 50;
  openWidth = 250;
var
  menuForm: TformMenu;
begin
  if not Assigned(user) then begin
    TMessageHelper.GetInstance.Error('Login ou senha inválidos');
    Exit;
  end;

  TSession.SetUser(user);

  if Assigned(Self.activeForm) then
    Self.activeForm.Free;

  TLogger.GetLogger.Info('Login realizado: ' + TSession.GetUser.login);

  menuForm := TformMenu.Create(Self.panelMain, closedWidth, openWidth);
  menuForm.setLogoutCallback(Self.OnLogout);
  Self.OpenForm(menuForm);
end;

procedure TformMain.OnLogout;
var
  loginForm: TformLogin;
begin
  if Assigned(Self.activeForm) then
    Self.activeForm.Free;

  loginForm := TformLogin.Create(Self.panelMain, Self.OnLogin);
  Self.OpenForm(loginForm);
end;

end.

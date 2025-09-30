unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, Vcl.ExtCtrls, ConfigController, DBConfigView, LoginView,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.Imaging.pngimage, UserModel, MenuView;

type
  TformMain = class(TForm)
    panelMain: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    configController: TConfigController;
    activeForm: TForm;
    procedure OnLogin(user: TUserModel);
    procedure OpenForm(aForm: TForm);
  public
    class var loggedUser: TUserModel;
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

procedure TformMain.FormCreate(Sender: TObject);
var
  configForm: TformDBConfig;
  loginForm: TformLogin;
begin
  Self.configController := TConfigController.Create;
  Self.configController.PrepareDirectory;

  try
    Self.configController.LoadDBConfig;
  except
    on e: Exception do begin
      ShowMessage(e.Message);

      configForm := TformDBConfig.Create(Self.configController, nil);
      configForm.ShowModal;
      configForm.Free;
    end;
  end;

  if Self.configController.IsDBConnected then begin
    loginForm := TformLogin.Create(Self.panelMain, Self.OnLogin);
    Self.OpenForm(loginForm);
  end else begin
    Application.Terminate;
  end;
end;

procedure TformMain.OnLogin(user: TUserModel);
var
  menuForm: TformMenu;
begin
  if not Assigned(user) then begin
    raise Exception.Create('Login ou senha inválidos');
  end;

  Self.loggedUser := user;
  Self.activeForm.Free;

  menuForm := TformMenu.Create(Self.panelMain, 50, 250);
  Self.OpenForm(menuForm);
end;

end.

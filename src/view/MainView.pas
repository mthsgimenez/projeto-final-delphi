unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, Vcl.ExtCtrls, ConfigController, DBConfigView, LoginView,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.Imaging.pngimage, UserModel, UserView;

type
  TformMain = class(TForm)
    panelMain: TPanel;
    panelMenu: TPanel;
    imgMenu: TImage;
    Shape1: TShape;
    panelUser: TPanel;
    imgUser: TImage;
    labelUser: TLabel;
    buttonUserMenu: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure imgMenuClick(Sender: TObject);
    procedure buttonUserMenuClick(Sender: TObject);
  private
    openWidth: Integer;
    closedWidth: Integer;
    isMenuOpen: Boolean;
    configController: TConfigController;
    activeForm: TForm;
    procedure OnLogin(user: TUserModel);
    procedure ToggleMenu;
  public
    class var loggedUser: TUserModel;
    procedure ChangeForm(aForm: TFormClass);
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

procedure TformMain.buttonUserMenuClick(Sender: TObject);
begin
  Self.ChangeForm(TformUser);
  if Self.isMenuOpen then Self.ToggleMenu;
end;

procedure TformMain.ChangeForm(aForm: TFormClass);
begin
  Self.activeForm.Free;
  Self.activeForm := aForm.Create(Self.panelMain);
  Self.activeForm.Parent := Self.panelMain;
  Self.activeForm.BorderStyle := bsNone;
  Self.activeForm.Show;
end;

procedure TformMain.FormCreate(Sender: TObject);
var
  configForm: TformDBConfig;
  loginForm: TformLogin;
begin
  Self.panelMenu.Visible := False;
  Self.closedWidth := 50;
  Self.openWidth := 250;
  Self.isMenuOpen := False;
  Self.panelMenu.Width := Self.closedWidth;

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
    Self.activeForm := loginForm;
    Self.activeForm.Parent := Self.panelMain;
    Self.activeForm.BorderStyle := bsNone;
    Self.activeForm.Show;
  end else begin
    Application.Terminate;
  end;
end;

procedure TformMain.imgMenuClick(Sender: TObject);
begin
  Self.ToggleMenu;
end;

procedure TformMain.OnLogin(user: TUserModel);
begin
  if not Assigned(user) then begin
    raise Exception.Create('Login ou senha inválidos');
  end;

  Self.loggedUser := user;
  Self.panelMenu.Visible := True;
  Self.activeForm.Free;
  Self.activeForm := nil;
  Self.panelMain.Margins.Left := 50;
end;

procedure TformMain.ToggleMenu;
begin
  if Self.isMenuOpen then begin
    Self.isMenuOpen := False;
    Self.panelMenu.Width := Self.closedWidth;
    Exit;
  end;

  Self.isMenuOpen := True;
  Self.panelMenu.Width := Self.openWidth;
end;

end.

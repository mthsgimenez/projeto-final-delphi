unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Buttons, Vcl.Imaging.pngimage, LoginController;

type
  TformLogin = class(TForm)
    panelBackground: TPanel;
    imageBackground: TImage;
    gridPanelLayout: TGridPanel;
    panelLoginForm: TPanel;
    shapeLoginForm: TShape;
    gridPanelLoginForm: TGridPanel;
    labelTitle: TLabel;
    panelEdits: TPanel;
    panelEditUser: TPanel;
    shapeEditUser: TShape;
    imageUserIcon: TImage;
    editUser: TEdit;
    panelEditPassword: TPanel;
    shapeEditPassword: TShape;
    imageLockIcon: TImage;
    editPassword: TEdit;
    panelButtonLogin: TPanel;
    shapeButtonLogin: TShape;
    buttonLogin: TSpeedButton;
    procedure buttonLoginClick(Sender: TObject);
  private
    loginController: TLoginController;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formLogin: TformLogin;

implementation

{$R *.dfm}

procedure TformLogin.buttonLoginClick(Sender: TObject);
var
  user, password: String;
  errors: TStringList;
begin
  user := Trim(editUser.Text);
  password := Trim(editPassword.Text);

  errors := TStringList.Create;
  try
    if user = '' then begin
      errors.Add('Preencha o campo usuário');
    end;
    if password = '' then begin
      errors.Add('Preencha o campo senha');
    end;

    if errors.Count > 0 then raise Exception.Create(errors.Text);

    Self.loginController.Login(user, password);
  finally
    errors.Free;
  end;
end;

constructor TformLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.loginController := TLoginController.Create;
end;

destructor TformLogin.Destroy;
begin
  Self.loginController.Free;
  inherited Destroy;
end;

end.

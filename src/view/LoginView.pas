unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Buttons, Vcl.Imaging.pngimage, UserController, UserDTO, UserModel, ViewController, UserView;

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
    userController: TUserController;
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
  userDTO: TUserDTO;
  errors: TStringList;
  user: TUserModel;
begin
  userDTO.login := Trim(editUser.Text);
  userDTO.password := Trim(editPassword.Text);

  errors := TStringList.Create;
  try
    if userDTO.login = '' then begin
      errors.Add('Preencha o campo usuário');
    end;
    if userDTO.password = '' then begin
      errors.Add('Preencha o campo senha');
    end;

    if errors.Count > 0 then raise Exception.Create(errors.Text);

    user := Self.userController.Login(userDTO);
    if user = nil then raise Exception.Create('Login ou senha incorretos');
    user.Free;

    TViewController.GetInstance(nil).ChangeForm(TformUser);
  finally
    errors.Free;
  end;
end;

constructor TformLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.userController := TUserController.Create;
end;

destructor TformLogin.Destroy;
begin
  Self.userController.Free;
  inherited Destroy;
end;

end.

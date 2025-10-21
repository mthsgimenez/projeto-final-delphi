unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Buttons, Vcl.Imaging.pngimage, UserController, UserDTO, UserModel, MessageHelper,
  Vcl.Skia;

type
  TLoginCallback = procedure(user: TUserModel) of object;

  TformLogin = class(TForm)
    panelBackground: TPanel;
    imageBackground: TImage;
    gridpanelLayout: TGridPanel;
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
    messageHelper: TMessageHelper;
  public
    onLoginAttempt: TLoginCallback;
    constructor Create(AOwner: TComponent; aCallback: TLoginCallback); reintroduce;
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

    if errors.Count > 0 then begin
      Self.messageHelper.Error(errors.Text);
      Exit;
    end;

    user := Self.userController.Login(userDTO);

    onLoginAttempt(user);
  finally
    errors.Free;
  end;
end;

constructor TformLogin.Create(AOwner: TComponent; aCallback: TLoginCallback);
begin
  inherited Create(AOwner);
  if not Assigned(aCallback) then raise Exception.Create('Callback de login não específicado');
  Self.onLoginAttempt := aCallback;
  Self.userController := TUserController.Create;
end;

destructor TformLogin.Destroy;
begin
  Self.userController.Free;
  inherited Destroy;
end;

end.

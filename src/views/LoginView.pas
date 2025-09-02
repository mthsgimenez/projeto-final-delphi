unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Buttons, Vcl.Imaging.pngimage;

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
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formLogin: TformLogin;

implementation

{$R *.dfm}

procedure TformLogin.buttonLoginClick(Sender: TObject);
begin
  ShowMessage('fkasdfka');
end;

end.

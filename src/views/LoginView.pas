unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.Buttons;

type
  TformLogin = class(TForm)
    panelBackground: TPanel;
    imageBackground: TImage;
    gridPanelLayout: TGridPanel;
    panelInputs: TPanel;
    editUser: TEdit;
    panelButton: TPanel;
    buttonLogin: TSpeedButton;
    panelEditUser: TPanel;
    panelEditPassword: TPanel;
    editPassword: TEdit;
    labelTitle: TLabel;
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

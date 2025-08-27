unit LoginView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg;

type
  TformLogin = class(TForm)
    panelBackground: TPanel;
    imageBackground: TImage;
    GridPanelLogin: TGridPanel;
    editUser: TEdit;
    editPassword: TEdit;
    buttonLogin: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formLogin: TformLogin;

implementation

{$R *.dfm}

end.

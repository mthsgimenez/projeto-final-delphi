unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, Vcl.ExtCtrls, ConfigController, DBConfigView, LoginView;

type
  TformMain = class(TForm)
    panelMain: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    configController: TConfigController;
    activeForm: TForm;
  public
    { Public declarations }
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

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
    loginForm := TformLogin.Create(Self.panelMain);
    loginForm.Parent := Self.panelMain;
    loginForm.BorderStyle := bsNone;
    loginForm.Show;
    Self.activeForm := loginForm;
  end else begin
    Application.Terminate;
  end;
end;

end.

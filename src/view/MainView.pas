unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, Vcl.ExtCtrls, ConfigController, DBConfigView, LoginView, MainViewInterface, ViewController;

type
  TformMain = class(TForm, IMainView)
    panelMain: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    configController: TConfigController;
    activeForm: TForm;
    procedure OpenForm(aForm: TFormClass);
  public
    procedure ChangeForm(aForm: TFormClass);
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

procedure TformMain.ChangeForm(aForm: TFormClass);
begin
  Self.activeForm.Free;
  Self.activeForm := nil;

  Self.OpenForm(aForm);
end;

procedure TformMain.FormCreate(Sender: TObject);
var
  configForm: TformDBConfig;
begin
  Self.configController := TConfigController.Create;
  Self.configController.PrepareDirectory;

  TViewController.GetInstance(Self);

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
    Self.OpenForm(TformLogin);
  end else begin
    Application.Terminate;
  end;
end;

procedure TformMain.OpenForm(aForm: TFormClass);
var
  form: TForm;
begin
  form := aForm.Create(Self.panelMain);
  form.Parent := Self.panelMain;
  form.BorderStyle := bsNone;
  form.Show;
  Self.activeForm := form;
end;

end.

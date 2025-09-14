unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, ConfigController;

type
  TformMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    configController: TConfigController;
  public
    { Public declarations }
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

procedure TformMain.FormCreate(Sender: TObject);
begin
  Self.configController := TConfigController.Create;
  Self.configController.PrepareDirectory;
end;

end.

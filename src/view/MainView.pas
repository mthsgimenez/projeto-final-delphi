unit MainView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.IOUtils, MainController;

type
  TformMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    controller: TMainController;
  public
    { Public declarations }
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

procedure TformMain.FormCreate(Sender: TObject);
var
  path: String;
begin
  path := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MTTools');

  Self.Controller := TMainController.Create;
  Self.Controller.SetupDirectory;
end;

end.

unit DBConfigView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, ConfigController, DBConfigDTO;

type
  TformDBConfig = class(TForm)
    gridPanelDBConfig: TGridPanel;
    panelServer: TPanel;
    panelPort: TPanel;
    panelDatabase: TPanel;
    panelUser: TPanel;
    panelPassword: TPanel;
    labelServer: TLabel;
    editServer: TEdit;
    labelPort: TLabel;
    editPort: TEdit;
    labelTitle: TLabel;
    labelDatabase: TLabel;
    editDatabase: TEdit;
    labelUser: TLabel;
    editUser: TEdit;
    labelPassword: TLabel;
    editPassword: TEdit;
    buttonSave: TButton;
    procedure buttonSaveClick(Sender: TObject);
  private
    controller: TConfigController;
  public
    constructor Create(aConfigController: TConfigController; AOwner: TComponent); reintroduce;
  end;

var
  formDBConfig: TformDBConfig;

implementation

{$R *.dfm}

{ TformDBConfig }

procedure TformDBConfig.buttonSaveClick(Sender: TObject);
var
  DBConfigDTO: TDBConfigDTO;
  errors: TStringList;
begin
  DBConfigDTO.server := Self.editServer.Text;
  if not TryStrToInt(Self.editPort.Text, DBConfigDTO.port) then raise Exception.Create('Campo "port" deve ser um número inteiro');
  DBConfigDTO.database := Self.editDatabase.Text;
  DBConfigDTO.user := Self.editUser.Text;
  DBConfigDTO.password := Self.editPassword.Text;

  errors := DBConfigDTO.ValidateDTO;
  if errors.Count > 0 then raise Exception.Create(errors.Text);

  Self.controller.ConfigureConnection(DBConfigDTO);
  Self.Close;
end;

constructor TformDBConfig.Create(aConfigController: TConfigController;
  AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.controller := aConfigController;
end;

end.

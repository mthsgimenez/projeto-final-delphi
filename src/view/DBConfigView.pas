unit DBConfigView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, ConfigController, DBConfigDTO,
  Vcl.Buttons;

type
  TformDBConfig = class(TForm)
    panelDBConfig: TPanel;
    panelEditPassword: TPanel;
    shapeEditPassword: TShape;
    editPassword: TEdit;
    panelContainerPassword: TPanel;
    labelPassword: TLabel;
    panelContainerUser: TPanel;
    labelUser: TLabel;
    panelEditUser: TPanel;
    shapeEditUser: TShape;
    editUser: TEdit;
    panelContainerDatabase: TPanel;
    labelDatabase: TLabel;
    panelEditDatabase: TPanel;
    shapeEditDatabase: TShape;
    editDatabase: TEdit;
    panelContainerAddress: TPanel;
    labelServer: TLabel;
    panelEditServer: TPanel;
    shapeEditServer: TShape;
    editServer: TEdit;
    panelEditPort: TPanel;
    shapeEditPort: TShape;
    editPort: TEdit;
    labelPort: TLabel;
    labelTitle: TLabel;
    panelButtonSave: TPanel;
    shapeButtonSave: TShape;
    buttonSave: TSpeedButton;
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
  DBConfigDTO.database := Self.editDatabase.Text;
  DBConfigDTO.user := Self.editUser.Text;
  DBConfigDTO.password := Self.editPassword.Text;

  errors := DBConfigDTO.ValidateDTO;

  try
    if not TryStrToInt(Self.editPort.Text, DBConfigDTO.port) then errors.Add('campo "port" deve ser um número inteiro');

    if errors.Count > 0 then raise Exception.Create(errors.Text);
  finally
    errors.Free;
  end;

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

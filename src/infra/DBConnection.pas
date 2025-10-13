unit DBConnection;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG,
  FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  Vcl.Dialogs, FireDAC.Phys.PGWrapper, DBConfigModel, Logging;

type
  TConnection = class(TDataModule)
    FDConnection: TFDConnection;
    FDPhysPgDriverLink: TFDPhysPgDriverLink;
  private
    { Private declarations }
  public
    procedure Configure(aDBConfigModel: TDBConfigModel);
  end;

var
  Connection: TConnection;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TConnection }

procedure TConnection.Configure(aDBConfigModel: TDBConfigModel);
begin
  try
    Self.FDConnection.Connected := False;
    with Self.FDConnection.Params do begin
      Clear;
      Add('Server=' + aDBConfigModel.server);
      Add('Port=' + aDBConfigModel.port.ToString);
      Add('Database=' + aDBConfigModel.database);
      Add('User_Name=' + aDBConfigModel.user);
      Add('Password=' + aDBConfigModel.password);
      Add('DriverID=PG');
    end;
    Self.FDConnection.Connected := True;
    TLogger.GetLogger.Info('Conexão com banco estabelecida');
  except
  on e: EPgNativeException do begin
    Self.FDConnection.Connected := False;
    TLogger.GetLogger.Error('Erro na conexão com banco de dados: ' + e.Message);
    raise Exception.Create('Erro na conexão: ' + e.Message);
  end;
  on e: Exception do begin
    Self.FDConnection.Connected := False;
    TLogger.GetLogger.Error('Erro na conexão com banco de dados: ' + e.Message);
    raise Exception.Create('Erro: ' + e.Message);
  end;
  end;
end;

end.

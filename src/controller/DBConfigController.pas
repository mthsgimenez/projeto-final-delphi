unit DBConfigController;

interface

uses DBConfigDTO, DBConfigModel, DBConnection,
      System.SysUtils, Vcl.Dialogs, FireDAC.Phys.PGWrapper;

type TDBConfigController = class
  private
  public
    procedure Configure(aConnection: TConnection; aDBConfigModel: TDBConfigModel);
end;

implementation

{ TDBConfigController }

procedure TDBConfigController.Configure(aConnection: TConnection; aDBConfigModel: TDBConfigModel);
begin
  try
    aConnection.FDConnection.Connected := False;
    with aConnection.FDConnection.Params do begin
      Clear;
      Add('Server=' + aDBConfigModel.server);
      Add('Port=' + aDBConfigModel.port.ToString);
      Add('Database=' + aDBConfigModel.database);
      Add('User_Name=' + aDBConfigModel.user);
      Add('Password=' + aDBConfigModel.password);
      Add('DriverID=PG');
    end;
    aConnection.FDConnection.Connected := True;
  except
  on e: EPgNativeException do begin
    ShowMessage('Erro na conexão: ' + e.Message);
  end;
  on e: Exception do begin
    ShowMessage('Erro: ' + e.Message);
  end;
  end;
end;

end.

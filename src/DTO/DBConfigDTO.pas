unit DBConfigDTO;

interface

  uses System.SysUtils, System.Classes;

  type TDBConfigDTO = record
    server: String;
    port: Integer;
    database: String;
    user: String;
    password: String;
    function ValidateDTO: TStringList;
  end;

implementation

function TDBConfigDTO.ValidateDTO: TStringList;
var
  errorFields: TStringList;
begin
  errorFields := TStringList.Create;
  if Trim(Self.server) = '' then errorFields.Add('campo "server" n�o pode estar vazio.');
  if Trim(Self.database) = '' then errorFields.Add('campo "database" n�o pode estar vazio.');
  if Trim(Self.user) = '' then errorFields.Add('campo "user" n�o pode estar vazio.');
  Result := errorFields;
end;

end.

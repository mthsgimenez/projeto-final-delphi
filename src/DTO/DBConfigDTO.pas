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
  if Trim(Self.server) = '' then errorFields.Add('campo "server" não pode estar vazio.');
  if Trim(Self.database) = '' then errorFields.Add('campo "database" não pode estar vazio.');
  if Trim(Self.user) = '' then errorFields.Add('campo "user" não pode estar vazio.');
  Result := errorFields;
end;

end.

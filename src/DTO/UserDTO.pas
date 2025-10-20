unit UserDTO;

interface

  uses System.SysUtils, System.Classes, Permissions;

  type TUserDTO = record
    name: String;
    login: String;
    password: String;
    function ValidateDTO(ensurePassword: Boolean): TStringList;
  end;

implementation

function TUserDTO.ValidateDTO(ensurePassword: Boolean): TStringList;
var
  errorFields: TStringList;
begin
  errorFields := TStringList.Create;
  if Trim(Self.name) = '' then errorFields.Add('campo "nome" não pode estar vazio.');
  if Trim(Self.login) = '' then errorFields.Add('campo "login" não pode estar vazio.');
  if ensurePassword then
    if Trim(Self.password) = '' then errorFields.Add('campo "senha" não pode estar vazio.');

  Result := errorFields;
end;

end.

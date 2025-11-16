unit ToolTypeDTO;

interface

uses System.Classes, System.SysUtils;

type TToolTypeDTO = record
  code: String;
  description: String;
  family: String;
  usage: String;
  id_supplier: Integer;
  price: Currency;
  image: String;

  function Validate(): TStringList;
end;

implementation

{ TToolTypeDTO }

function TToolTypeDTO.Validate: TStringList;
var
  errors: TStringList;
begin
  errors := TStringList.Create;

  if Trim(code) = '' then errors.Add('código não pode estar vazio');
  if Trim(description) = '' then errors.Add('descrição não pode estar vazia');
  if Trim(family) = '' then errors.Add('família não pode estar vazia');
  if price <= 0 then errors.Add('preço deve ser maior que zero');
  if id_supplier <= 0 then errors.Add('fornecedor inválido');
  if Trim(usage) = '' then errors.Add('tipo de consumo não pode estar vazio');

  Result := errors;
end;

end.

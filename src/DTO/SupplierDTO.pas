unit SupplierDTO;

interface

uses System.SysUtils, System.Classes;

type TSupplierDTO = record
  tradeName: String;
  legalName: String;
  CNPJ: String;
  CEP: String;
  email: String;
  phone: String;
  function ValidateDTO(): TStringList;
end;

implementation

{ TSupplierDTO }

function TSupplierDTO.ValidateDTO: TStringList;
var
  errorFields: TStringList;
begin
  errorFields := TStringList.Create;

  if Trim(Self.tradeName) = '' then
    errorFields.Add('Nome fantasia não pode estar vazio');

  if Trim(Self.legalName) = '' then
    errorFields.Add('Razão social não pode estar vazia');

  if Trim(Self.CNPJ) = '' then
    errorFields.Add('CNPJ não pode estar vazio');

  if Trim(Self.CEP) = '' then
    errorFields.Add('CEP não pode estar vazio');

  Result := errorFields;
end;

end.

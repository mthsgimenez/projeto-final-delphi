unit ImplCnpja;

interface

uses CNPJApiInterface, SupplierDTO, CNPJ,
  System.SysUtils, System.Classes, IdHTTP, System.JSON, System.JSON.Types, System.Generics.Collections;

type TCNPJA = class(TInterfacedObject, ICNPJApi)
  private
    httpClient: TIdHTTP;
  public
    constructor Create;
    destructor Destroy; override;
    function SearchCNPJ(aCNPJ: String): TSupplierDTO;
end;

implementation

{ TCNPJA }

constructor TCNPJA.Create;
begin
  Self.httpClient := TIdHTTP.Create(nil);
end;

destructor TCNPJA.Destroy;
begin
  Self.httpClient.Free;
  inherited;
end;

function TCNPJA.SearchCNPJ(aCNPJ: String): TSupplierDTO;
const
  baseUrl = 'https://open.cnpja.com/office/';
var
  url: String;
  JSONResponse: string;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
  companyObj: TJSONObject;
  addressObj: TJSONObject;
  phonesArray: TJSONArray;
  emailsArray: TJSONArray;
  CNPJDTO: TSupplierDTO;
begin
  url := baseUrl + aCNPJ;

  try
    JSONResponse := Self.httpClient.Get(url);
  except
    Exit;
  end;

  JSONValue := TJSONObject.ParseJSONValue(JSONResponse);
  if not Assigned(JSONValue) then
    Exit;

  try
    JSONObj := JSONValue as TJSONObject;

    CNPJDTO.cnpj := JSONObj.GetValue<string>('taxId');
    CNPJDTO.tradeName := JSONObj.GetValue<string>('alias');

    companyObj := JSONObj.GetValue<TJSONObject>('company');
    CNPJDTO.legalName := companyObj.GetValue<string>('name');

    addressObj := JSONObj.GetValue<TJSONObject>('address');
    CNPJDTO.cep := addressObj.GetValue<string>('zip');

    phonesArray := JSONObj.GetValue<TJSONArray>('phones');
    if phonesArray.Count > 0 then begin
      CNPJDTO.phone := phonesArray.Items[0].GetValue<String>('area');
      CNPJDTO.phone := CNPJDTO.phone + phonesArray.Items[0].GetValue<string>('number');
    end;

    emailsArray := JSONObj.GetValue<TJSONArray>('emails');
    if emailsArray.Count > 0 then
      CNPJDTO.email := emailsArray.Items[0].GetValue<string>('address');

    Result := CNPJDTO;
  finally
    JSONValue.Free;
  end;
end;

end.

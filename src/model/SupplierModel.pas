unit SupplierModel;

interface

uses CNPJ;

type TSupplier = class
  public
    id: Integer;
    tradeName: String;
    legalName: String;
    CNPJ: TCNPJ;
    CEP: String;
    email: String;
    phone: String;
end;

implementation

end.


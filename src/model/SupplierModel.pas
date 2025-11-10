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

    destructor Destroy; override;
end;

implementation

{ TSupplier }

destructor TSupplier.Destroy;
begin
  Self.CNPJ.Free;
  inherited;
end;

end.


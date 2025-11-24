unit ToolTypeModel;

interface

uses SupplierModel;

type TToolType = class
  public
    id: Integer;
    code: String;
    description: String;
    family: String;
    usage: String;
    supplier: TSupplier;
    price: Currency;
    image: String;
    quantityTotal: Integer;
    quantityAvailable: Integer;
    quantityInUse: Integer;
    quantityHoning: Integer;

    destructor Destroy; override;
end;

implementation

{ TToolType }

destructor TToolType.Destroy;
begin
  Self.supplier.Free;
  inherited;
end;

end.

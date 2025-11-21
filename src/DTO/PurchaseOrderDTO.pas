unit PurchaseOrderDTO;

interface

uses System.Generics.Collections, PurchaseOrderModel, SupplierModel;

type TPurchaseOrderDTO = class
  public
    supplier: TSupplier;
    items: TObjectList<TPurchaseOrderItem>;

    constructor Create;
    destructor Destroy; override;
end;

implementation

{ TPurchaseOrderDTO }

constructor TPurchaseOrderDTO.Create;
begin
  Self.items := TObjectList<TPurchaseOrderItem>.Create;
end;

destructor TPurchaseOrderDTO.Destroy;
begin
  Self.items.Free;
  inherited;
end;

end.

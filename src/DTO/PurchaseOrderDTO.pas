unit PurchaseOrderDTO;

interface

type TPurchaseOrderItemDTO = record
  modelId: Integer;
  quantity: Integer;
end;

type TPurchaseOrderDTO = record
    supplierId: Integer;
    items: TArray<TPurchaseOrderItemDTO>;

    procedure AddItem(aItem: TPurchaseOrderItemDTO);
end;

implementation

{ TPurchaseOrderDTO }

procedure TPurchaseOrderDTO.AddItem(aItem: TPurchaseOrderItemDTO);
begin
  SetLength(Self.items, Length(Self.items) + 1);
  Self.items[High(Self.items)] := aItem;
end;

end.

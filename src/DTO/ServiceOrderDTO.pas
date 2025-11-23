unit ServiceOrderDTO;

interface

type TServiceOrderDTO = record
  supplierId: Integer;
  price: Currency;
  items: TArray<Integer>;

  procedure AddItem(aToolId: Integer);
end;

implementation

{ TServiceOrderDTO }

procedure TServiceOrderDTO.AddItem(aToolId: Integer);
begin
  SetLength(Self.items, Length(Self.items) + 1);
  Self.items[High(Self.items)] := aToolId;
end;

end.

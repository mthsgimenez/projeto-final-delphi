unit PurchaseOrderModel;

interface

uses System.SysUtils, SupplierModel, ToolTypeModel, System.Generics.Collections;

type TStatus = (
  OPEN = 1,
  CLOSED = 2,
  CANCELLED = 3
);

type TPurchaseOrderItem = class
  model: TToolType;
  quantity: Integer;

  destructor Destroy; override;
end;

type TPurchaseOrder = class
  public
    id: Integer;
    supplier: TSupplier;
    status: TStatus;
    issuedAt: TDateTime;
    statusUpdatedAt: TDateTime;
    items: TObjectList<TPurchaseOrderItem>;

    function GetTotalPrice: Currency;
    function GetTotalQuantity: Integer;
    procedure AddItemToOrder(aToolType: TToolType; aQuantity: Integer);
    constructor Create;
    destructor Destroy; override;
end;

function StatusToString(aStatus: TStatus): String;
function StringToStatus(aString: String): TStatus;
function StatusToViewString(aStatus: TStatus): String;

implementation

function StringToStatus(aString: String): TStatus;
begin
  aString := UpperCase(aString);
  if aString = 'OPEN' then
    Result := OPEN
  else if aString = 'CLOSED' then
    Result := CLOSED
  else if aString = 'CANCELLED' then
    Result := CANCELLED
  else
    raise Exception.Create(Format('String de status inválida: %s', [aString]));
end;

function StatusToString(aStatus: TStatus): String;
begin
  case aStatus of
    OPEN: Result := 'OPEN';
    CLOSED:    Result := 'CLOSED';
    CANCELLED:    Result := 'CANCELLED';
  else
    raise Exception.Create(Format('Valor de status inválido: %d', [Ord(aStatus)]));
  end;
end;

function StatusToViewString(aStatus: TStatus): String;
begin
  case aStatus of
    OPEN: Result := 'EM ABERTO';
    CLOSED: Result := 'FECHADO';
    CANCELLED: Result := 'CANCELADO';
  end;
end;

{ TPurchaseOrder }

procedure TPurchaseOrder.AddItemToOrder(aToolType: TToolType;
  aQuantity: Integer);
var
  item: TPurchaseOrderItem;
begin
  item := TPurchaseOrderItem.Create;
  item.model := aToolType;
  item.quantity := aQuantity;

  Self.items.Add(item);
end;

constructor TPurchaseOrder.Create;
begin
  Self.items := TObjectList<TPurchaseOrderItem>.Create;
end;

destructor TPurchaseOrder.Destroy;
begin
  Self.supplier.Free;
  Self.items.Free;
  inherited;
end;

function TPurchaseOrder.GetTotalPrice: Currency;
var
  item: TPurchaseOrderItem;
  total: Currency;
begin
  total := 0;

  for item in Self.items do begin
    total := total + item.model.price * item.quantity;
  end;

  result := total;
end;

function TPurchaseOrder.GetTotalQuantity: Integer;
var
  item: TPurchaseOrderItem;
  count: Integer;
begin
  count := 0;

  for item in Self.items do begin
    count := count + item.quantity;
  end;

  Result := count;
end;

{ TPurchaseOrderItem }

destructor TPurchaseOrderItem.Destroy;
begin
  Self.model.Free;
  inherited;
end;

end.

unit PurchaseOrderModel;

interface

uses System.SysUtils, SupplierModel, ToolModel, System.Generics.Collections;

type TStatus = (
  OPEN = 1,
  CLOSED = 2,
  CANCELLED = 3
);

type TPurchaseOrder = class
  public
    id: Integer;
    id_supplier: Integer;
    status: TStatus;
    issued_at: TDateTime;
    status_updated_at: TDateTime;
    items: TDictionary<Integer, Integer>; // id_tool_model, quantity

    constructor Create;
    destructor Destroy; override;
end;

function StatusToString(aStatus: TStatus): String;
function StringToStatus(aString: String): TStatus;

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

{ TPurchaseOrder }

constructor TPurchaseOrder.Create;
begin
  Self.items := TDictionary<Integer, Integer>.Create;
end;

destructor TPurchaseOrder.Destroy;
begin
  Self.items.Free;
  inherited;
end;

end.

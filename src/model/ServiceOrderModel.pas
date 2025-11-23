unit ServiceOrderModel;

interface

uses System.SysUtils, SupplierModel, ToolModel, System.Generics.Collections;

type TStatus = (
  OPEN = 1,
  CLOSED = 2,
  CANCELLED = 3
);

type TServiceOrder = class
  public
    id: Integer;
    supplier: TSupplier;
    status: TStatus;
    issuedAt: TDateTime;
    statusUpdatedAt: TDateTime;
    items: TObjectList<TTool>;
    price: Currency;

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

constructor TServiceOrder.Create;
begin
  Self.items := TObjectList<TTool>.Create;
end;

destructor TServiceOrder.Destroy;
begin
  Self.supplier.Free;
  Self.items.Free;
  inherited;
end;

end.

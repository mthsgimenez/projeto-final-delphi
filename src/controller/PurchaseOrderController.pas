unit PurchaseOrderController;

interface

uses Logging, Session, PurchaseOrderRepositoryInterface, PurchaseOrderModel, PurchaseOrderDTO,
  System.Generics.Collections, System.SysUtils, ToolRepositoryInterface, ToolModel, StorageModel;

type TPurchaseOrderController = class
  private
    purchaseOrderRepository: IPurchaseOrderRepository;
    toolRepository: IToolRepository;
    logger: TLogger;
  public
    function GetOrders(): TObjectList<TPurchaseOrder>;
    function CreateOrder(aData: TPurchaseOrderDTO): TPurchaseOrder;
    function CancelOrder(aOrderId: Integer): TPurchaseOrder;
    function CloseOrder(aOrderId: Integer; aStorage: TStorage): TPurchaseOrder;
    constructor Create(aPurchaseOrderRepository: IPurchaseOrderRepository;
      aToolRepository: IToolRepository);
end;

implementation

{ TPurchaseOrderController }

function TPurchaseOrderController.CancelOrder(
  aOrderId: Integer): TPurchaseOrder;
var
  order: TPurchaseOrder;
begin
  order := Self.purchaseOrderRepository.FindById(aOrderId);
  if not Assigned(order) then
    raise Exception.Create(Format('Pedido de compra com id %d não encontrado', [aOrderId]));

  order.status := CANCELLED;

  Result := Self.purchaseOrderRepository.UpdateStatus(order);

  if Assigned(Result) then
    Self.logger.Info(Format('Pedido de compra alterado: Usuário (ID: %d) cancelou o pedido de compra (ID: %d)', [TSession.GetUser.id, aOrderId]));

  order.Free;
end;

function TPurchaseOrderController.CloseOrder(aOrderId: Integer;
aStorage: TStorage): TPurchaseOrder;
var
  order: TPurchaseOrder;
  item: TPurchaseOrderItem;
  tool, _: TTool;
  i: Integer;
begin
  order := Self.purchaseOrderRepository.FindById(aOrderId);
  if not Assigned(order) then
    raise Exception.Create(Format('Pedido de compra com id %d não encontrado', [aOrderId]));

  for item in order.items do begin
    tool := TTool.Create;

    tool.model := item.model;
    tool.storage := aStorage;

    for i := 0 to item.quantity do begin
      _ := Self.toolRepository.Insert(tool);
      _.Free;
    end;

    tool.Free;
  end;

  order.status := CLOSED;

  Result := Self.purchaseOrderRepository.UpdateStatus(order);
  if Assigned(Result) then
    Self.logger.Info(Format('Pedido de compra alterado: Usuário (ID: %d) recebeu o pedido de compra (ID: %d)', [TSession.GetUser.id, aOrderId]));

  order.Free;
end;

constructor TPurchaseOrderController.Create(
  aPurchaseOrderRepository: IPurchaseOrderRepository;
  aToolRepository: IToolRepository);
begin
  Self.logger := TLogger.GetLogger;
  Self.purchaseOrderRepository := aPurchaseOrderRepository;
  Self.toolRepository := aToolRepository;
end;

function TPurchaseOrderController.CreateOrder(
  aData: TPurchaseOrderDTO): TPurchaseOrder;
var
  order: TPurchaseOrder;
  item: TPurchaseOrderItem;
begin
  order := TPurchaseOrder.Create;
  order.supplier := aData.supplier;
  for item in aData.items do begin
    order.AddItemToOrder(item.model, item.quantity);
  end;

  Result := Self.purchaseOrderRepository.CreatePurchaseOrder(order);
  if Assigned(Result) then
    Self.logger.Info(Format('Pedido de compra emitido: Usuário (ID: %d) emitiu o pedido de compra (ID: %d)', [TSession.GetUser.id, Result.id]));

  order.Free;
end;

function TPurchaseOrderController.GetOrders: TObjectList<TPurchaseOrder>;
begin
  Result := Self.purchaseOrderRepository.FindAll;
end;

end.

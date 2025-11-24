unit PurchaseOrderController;

interface

uses Logging, Session, PurchaseOrderRepositoryInterface, PurchaseOrderModel, PurchaseOrderDTO,
  System.Generics.Collections, System.SysUtils, ToolRepositoryInterface, ToolModel, StorageModel,
  SupplierRepositoryInterface, ToolTypeModel, ToolTypeRepositoryInterface,
  StorageRepositoryInterface;

type TPurchaseOrderController = class
  private
    purchaseOrderRepository: IPurchaseOrderRepository;
    toolRepository: IToolRepository;
    supplierRepository: ISupplierRepository;
    toolTypeRepository: IToolTypeRepository;
    storageRepository: IStorageRepository;
    logger: TLogger;
  public
    function GetOrders(): TObjectList<TPurchaseOrder>;
    function CreateOrder(aData: TPurchaseOrderDTO): TPurchaseOrder;
    function CancelOrder(aOrderId: Integer): TPurchaseOrder;
    function CloseOrder(aOrderId: Integer; aStorageId: Integer): TPurchaseOrder;
    function GetOpen(): TObjectList<TPurchaseOrder>;
    constructor Create(
      aPurchaseOrderRepository: IPurchaseOrderRepository;
      aSupplierRepository: ISupplierRepository;
      aToolTypeRepository: IToolTypeRepository;
      aToolRepository: IToolRepository;
      aStorageRepository: IStorageRepository
    );
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
aStorageId: Integer): TPurchaseOrder;
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

    tool.model := Self.toolTypeRepository.FindById(item.model.id);
    tool.storage := Self.storageRepository.FindById(aStorageId);

    for i := 1 to item.quantity do begin
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
      aSupplierRepository: ISupplierRepository;
      aToolTypeRepository: IToolTypeRepository;
      aToolRepository: IToolRepository;
      aStorageRepository: IStorageRepository
    );
begin
  Self.logger := TLogger.GetLogger;
  Self.purchaseOrderRepository := aPurchaseOrderRepository;
  Self.supplierRepository := aSupplierRepository;
  Self.toolTypeRepository := aToolTypeRepository;
  Self.toolRepository := aToolRepository;
  Self.storageRepository := aStorageRepository;
end;

function TPurchaseOrderController.CreateOrder(
  aData: TPurchaseOrderDTO): TPurchaseOrder;
var
  order: TPurchaseOrder;
  item: TPurchaseOrderItemDTO;
  model: TToolType;
begin
  order := TPurchaseOrder.Create;
  order.supplier := Self.supplierRepository.FindById(aData.supplierId);

  if not Assigned(order.supplier) then
    raise Exception.Create(Format('Fornecedor (ID: %d) não encontrado', [aData.supplierId]));

  try
    for item in aData.items do begin
      model := Self.toolTypeRepository.FindById(item.modelId);

      if not Assigned(model) then
        raise Exception.Create(Format('Ferramenta (ID: %d) não encontrada', [item.modelId]));

      order.AddItemToOrder(model, item.quantity);
    end;
  except
    order.Free;
    raise;
  end;

  Result := Self.purchaseOrderRepository.CreatePurchaseOrder(order);
  if Assigned(Result) then
    Self.logger.Info(Format('Pedido de compra emitido: Usuário (ID: %d) emitiu o pedido de compra (ID: %d)', [TSession.GetUser.id, Result.id]));

  order.Free;
end;

function TPurchaseOrderController.GetOpen: TObjectList<TPurchaseOrder>;
begin
  Result := Self.purchaseOrderRepository.FindOpen;
end;

function TPurchaseOrderController.GetOrders: TObjectList<TPurchaseOrder>;
begin
  Result := Self.purchaseOrderRepository.FindAll;
end;

end.

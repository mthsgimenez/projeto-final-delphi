unit ServiceOrderController;

interface

uses System.Generics.Collections, System.SysUtils, Session, Logging,
  ServiceOrderRepositoryInterface, ServiceOrderModel, ServiceOrderDTO,
  ToolRepositoryInterface, ToolModel,
  SupplierRepositoryInterface, SupplierModel;

type TServiceOrderController = class
  private
    supplierRepository: ISupplierRepository;
    toolRepository: IToolRepository;
    serviceOrderRepository: IServiceOrderRepository;
    logger: TLogger;
  public
    function GetOrders(): TObjectList<TServiceOrder>;
    function CreateOrder(aData: TServiceOrderDTO): TServiceOrder;
    function CancelOrder(aOrderId: Integer): TServiceOrder;
    function CloseOrder(aOrderId: Integer): TServiceOrder;

    constructor Create(
      aServiceOrderRepository: IServiceOrderRepository;
      aToolRepository: IToolRepository;
      aSupplierRepository: ISupplierRepository
    );
end;

implementation

{ TServiceOrderController }

function TServiceOrderController.CancelOrder(aOrderId: Integer): TServiceOrder;
var
  order: TServiceOrder;
  tool, _: TTool;
begin
  order := Self.serviceOrderRepository.FindById(aOrderId);
  if not Assigned(order) then
    raise Exception.Create(Format('Ordem de serviço com id %d não encontrada', [aOrderId]));

  order.status := CANCELLED;

  for tool in order.items do begin
    tool.status := AVAILABLE;

    _ := Self.toolRepository.Update(tool);
    _.Free;
  end;

  Result := Self.serviceOrderRepository.UpdateStatus(order);

  if Assigned(Result) then
    Self.logger.Info(Format('Ordem de serviço alterada: Usuário (ID: %d) cancelou a ordem de serviço (ID: %d)', [TSession.GetUser.id, aOrderId]));

  order.Free;
end;

function TServiceOrderController.CloseOrder(aOrderId: Integer): TServiceOrder;
var
  order: TServiceOrder;
  tool, _: TTool;
begin
  order := Self.serviceOrderRepository.FindById(aOrderId);
  if not Assigned(order) then
    raise Exception.Create(Format('Ordem de serviço com id %d não encontrada', [aOrderId]));

  for tool in order.items do begin
    tool.status := AVAILABLE;
    tool.state := HONED;
    tool.honingNum := tool.honingNum + 1;

    _ := Self.toolRepository.Update(tool);
    _.Free;
  end;

  order.status := CLOSED;

  Result := Self.serviceOrderRepository.UpdateStatus(order);
  if Assigned(Result) then
    Self.logger.Info(Format('Ordem de serviço alterada: Usuário (ID: %d) recebeu a ordem de serviço (ID: %d)', [TSession.GetUser.id, aOrderId]));

  order.Free;
end;

constructor TServiceOrderController.Create(
  aServiceOrderRepository: IServiceOrderRepository;
  aToolRepository: IToolRepository; aSupplierRepository: ISupplierRepository);
begin
  Self.logger := TLogger.GetLogger;
  Self.serviceOrderRepository := aServiceOrderRepository;
  Self.toolRepository := aToolRepository;
  Self.supplierRepository := aSupplierRepository;
end;

function TServiceOrderController.CreateOrder(
  aData: TServiceOrderDTO): TServiceOrder;
var
  order: TServiceOrder;
  toolId: Integer;
  tool: TTool;
begin
  order := TServiceOrder.Create;
  order.supplier := Self.supplierRepository.FindById(aData.supplierId);

  if not Assigned(order.supplier) then
    raise Exception.Create(Format('Fornecedor (ID: %d) não encontrado', [aData.supplierId]));

  try
    for toolId in aData.items do begin
      tool := Self.toolRepository.FindById(toolId);

      if not Assigned(tool) then
        raise Exception.Create(Format('Ferramenta (ID: %d) não encontrada', [toolId]));

      tool.status := HONING;
      order.items.Add(Self.toolRepository.Update(tool));
      tool.Free;
    end;
  except
    order.Free;
    raise;
  end;

  Result := Self.serviceOrderRepository.CreateServiceOrder(order);
  if Assigned(Result) then
    Self.logger.Info(Format('Ordem de serviço emitida: Usuário (ID: %d) emitiu a ordem de serviço (ID: %d)', [TSession.GetUser.id, Result.id]));

  order.Free;
end;

function TServiceOrderController.GetOrders: TObjectList<TServiceOrder>;
begin
  Result := Self.serviceOrderRepository.FindAll;
end;

end.

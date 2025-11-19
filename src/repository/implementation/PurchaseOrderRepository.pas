unit PurchaseOrderRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param,
  PurchaseOrderRepositoryInterface, PurchaseOrderModel, ToolTypeModel, ToolTypeRepositoryInterface;

type TPurchaseOrderRepository = class(TRepositoryBase, IPurchaseOrderRepository)
  private
    helper: TDBHelper;
    toolTypeRepository: IToolTypeRepository;
  public
    function Insert(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function Update(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function FindById(aPurchaseOrderId: Integer): TPurchaseOrder;
    function FindAll(): TObjectList<TPurchaseOrder>;
    function DeleteById(aPurchaseOrderId: Integer): Boolean;
    function ExistsById(aPurchaseOrderId: Integer): Boolean;

    constructor Create(aHelper: TDBHelper; aToolTypeRepository: IToolTypeRepository);
end;

implementation

{ TPurchaseOrderRepository }

constructor TPurchaseOrderRepository.Create(aHelper: TDBHelper;
  aToolTypeRepository: IToolTypeRepository);
begin
  inherited Create;
  Self.helper := aHelper;
  Self.toolTypeRepository := aToolTypeRepository;
end;

function TPurchaseOrderRepository.DeleteById(
  aPurchaseOrderId: Integer): Boolean;
begin
  // noop
end;

function TPurchaseOrderRepository.ExistsById(
  aPurchaseOrderId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('purchase_orders', 'id', aPurchaseOrderId);
end;

function TPurchaseOrderRepository.FindAll: TObjectList<TPurchaseOrder>;
begin
// SELECT * FROM purchase_orders WHERE id = :purchaseOrderId;
// SELECT id_tool_model, tool_quantity FROM purchase_order_tools WHERE id_purchase_order = :purchaseOrderId;
end;

function TPurchaseOrderRepository.FindById(
  aPurchaseOrderId: Integer): TPurchaseOrder;
begin

end;

function TPurchaseOrderRepository.Insert(
  aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
begin

end;

function TPurchaseOrderRepository.Update(
  aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
begin

end;

end.

unit PurchaseOrderRepositoryInterface;

interface

uses System.Generics.Collections, CRUDRepositoryInterface, PurchaseOrderModel, ToolTypeModel;

type IPurchaseOrderRepository = interface
  function CreatePurchaseOrder(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
  function UpdateStatus(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
  function FindById(aPurchaseOrderId: Integer): TPurchaseOrder;
  function FindAll(): TObjectList<TPurchaseOrder>;
end;

implementation

end.

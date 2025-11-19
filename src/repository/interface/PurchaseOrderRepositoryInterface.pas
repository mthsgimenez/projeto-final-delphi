unit PurchaseOrderRepositoryInterface;

interface

uses System.Generics.Collections, CRUDRepositoryInterface, PurchaseOrderModel, ToolTypeModel;

type IPurchaseOrderRepository = interface(ICrudRepository<TPurchaseOrder>)
end;

implementation

end.

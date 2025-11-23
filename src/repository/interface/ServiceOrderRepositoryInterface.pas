unit ServiceOrderRepositoryInterface;

interface

uses System.Generics.Collections, ServiceOrderModel;

type IServiceOrderRepository = interface
  function CreateServiceOrder(aServiceOrder: TServiceOrder): TServiceOrder;
  function UpdateStatus(aServiceOrder: TServiceOrder): TServiceOrder;
  function FindById(aServiceOrderId: Integer): TServiceOrder;
  function FindAll(): TObjectList<TServiceOrder>;
end;

implementation

end.

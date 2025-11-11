unit SupplierDAOInterface;

interface

uses DAOInterface, SupplierModel;

type ISupplierDAO = interface(IDAO<TSupplier>)
  function SelectByToolTypeId(aToolTypeId: Integer): TSupplier;
end;

implementation

end.

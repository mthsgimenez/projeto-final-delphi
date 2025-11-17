unit SupplierRepositoryInterface;

interface

uses CRUDRepositoryInterface, SupplierModel, SupplierDTO, CNPJ;

type ISupplierRepository = interface(ICrudRepository<TSupplier>)
  function SearchCNPJ(aCNPJ: TCNPJ): TSupplierDTO;
end;

implementation

end.

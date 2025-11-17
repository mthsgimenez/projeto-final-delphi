unit CNPJAPIInterface;

interface

uses SupplierDTO;

type ICNPJApi = interface
  function SearchCNPJ(aCNPJ: String): TSupplierDTO;
end;

implementation

end.

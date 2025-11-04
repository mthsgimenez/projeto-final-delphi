unit CNPJAPIInterface;

interface

uses CNPJDTO;

type ICNPJApi = interface
  function SearchCNPJ(aCNPJ: String): TCNPJDTO;
end;

implementation

end.

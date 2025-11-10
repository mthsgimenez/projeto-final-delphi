unit SupplierRepository;

interface

uses
  CrudRepositoryInterface, SupplierModel, DBHelper, SupplierDAOInterface,
  System.Generics.Collections, SupplierDTO, CNPJ, CNPJApiInterface;

type
  TSupplierRepository = class(TInterfacedObject, ICrudRepository<TSupplier>)
  private
    supplierDAO: ISupplierDAO;
    CNPJApi: ICNPJApi;
    helper: TDBHelper;
  public
    function Save(aSupplier: TSupplier): TSupplier;
    function FindById(aId: Integer): TSupplier;
    function FindAll(): TObjectList<TSupplier>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
    function SearchCNPJ(aCNPJ: TCNPJ): TSupplierDTO;
    constructor Create(aSupplierDAO: ISupplierDAO; aCNPJApi: ICNPJApi);
    destructor Destroy; override;
  end;

implementation

{ TSupplierRepository }

constructor TSupplierRepository.Create(aSupplierDAO: ISupplierDAO; aCNPJApi: ICNPJApi);
begin
  Self.supplierDAO := aSupplierDAO;
  Self.CNPJApi := aCNPJApi;
  Self.helper := TDBHelper.Create;
end;

destructor TSupplierRepository.Destroy;
begin
  Self.helper.Free;
  inherited;
end;

function TSupplierRepository.DeleteById(aId: Integer): Boolean;
begin
  Result := Self.supplierDAO.DeleteById(aId);
end;

function TSupplierRepository.ExistsById(aId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('suppliers', 'id', aId);
end;

function TSupplierRepository.FindAll: TObjectList<TSupplier>;
begin
  Result := Self.supplierDAO.SelectAll;
end;

function TSupplierRepository.FindById(aId: Integer): TSupplier;
begin
  Result := Self.supplierDAO.SelectById(aId);
end;

function TSupplierRepository.Save(aSupplier: TSupplier): TSupplier;
begin
  if aSupplier.id = 0 then
    Result := Self.supplierDAO.Insert(aSupplier)
  else
    Result := Self.supplierDAO.Update(aSupplier);
end;

function TSupplierRepository.SearchCNPJ(aCNPJ: TCNPJ): TSupplierDTO;
begin
  Result := Self.CNPJApi.SearchCNPJ(aCNPJ.getCNPJ);
end;

end.


unit SupplierController;

interface

uses
  SupplierRepositoryInterface, SupplierModel, SupplierDTO, System.Generics.Collections,
  System.SysUtils, Logging, Session, CNPJ;

type
  TSupplierController = class
  private
    supplierRepository: ISupplierRepository;
  public
    constructor Create(aSupplierRepository: ISupplierRepository);
    function CreateSupplier(aSupplier: TSupplierDTO): TSupplier;
    function EditSupplier(aId: Integer; aData: TSupplierDTO): TSupplier;
    function GetSupplier(aId: Integer): TSupplier;
    function GetSuppliers: TObjectList<TSupplier>;
    function DeleteSupplier(aId: Integer): Boolean;
    function SearchCNPJ(aCNPJ: String): TSupplierDTO;
  end;

implementation

{ TSupplierController }

constructor TSupplierController.Create(aSupplierRepository: ISupplierRepository);
begin
  inherited Create;
  Self.supplierRepository := aSupplierRepository;
end;

function TSupplierController.CreateSupplier(aSupplier: TSupplierDTO): TSupplier;
var
  supplier: TSupplier;
begin
  if not TCNPJ.IsCnpjValid(aSupplier.CNPJ) then
    raise Exception.Create('CNPJ inválido');

  supplier := TSupplier.Create;
  try
    supplier.tradeName := aSupplier.tradeName;
    supplier.legalName := aSupplier.legalName;
    supplier.CNPJ := TCNPJ.Create(Trim(aSupplier.CNPJ));
    supplier.CEP := aSupplier.CEP;
    supplier.email := aSupplier.email;
    supplier.phone := aSupplier.phone;

    Result := Self.supplierRepository.Insert(supplier);

    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Fornecedor criado: Usuário (ID: %d) %s cadastrou o fornecedor (ID: %d)',
        [TSession.GetUser.id, TSession.GetUser.name, Result.id]
      ));
  finally
    supplier.Free;
  end;
end;

function TSupplierController.DeleteSupplier(aId: Integer): Boolean;
begin
  Result := Self.supplierRepository.DeleteById(aId);

  if Result then
    TLogger.GetLogger.Info(Format(
      'Fornecedor deletado: Usuário (ID: %d) %s deletou o fornecedor (ID: %d)',
      [TSession.GetUser.id, TSession.GetUser.name, aId]
    ));
end;

function TSupplierController.EditSupplier(aId: Integer; aData: TSupplierDTO): TSupplier;
var
  supplier: TSupplier;
begin
  supplier := Self.supplierRepository.FindById(aId);

  if not Assigned(supplier) then
    raise Exception.CreateFmt('Fornecedor com ID %d não encontrado', [aId]);

  if not TCNPJ.IsCnpjValid(aData.CNPJ) then
    raise Exception.Create('CNPJ inválido');

  try
    supplier.tradeName := aData.tradeName;
    supplier.legalName := aData.legalName;
    if Assigned(supplier.CNPJ) then
      supplier.CNPJ.Free;
    supplier.CNPJ := TCNPJ.Create(Trim(aData.CNPJ));
    supplier.CEP := aData.CEP;
    supplier.email := aData.email;
    supplier.phone := aData.phone;

    Result := Self.supplierRepository.Update(supplier);

    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Fornecedor editado: Usuário (ID: %d) %s editou o fornecedor (ID: %d)',
        [TSession.GetUser.id, TSession.GetUser.name, aId]
      ));
  finally
    supplier.Free;
  end;
end;

function TSupplierController.GetSupplier(aId: Integer): TSupplier;
begin
  Result := Self.supplierRepository.FindById(aId);
end;

function TSupplierController.GetSuppliers: TObjectList<TSupplier>;
begin
  Result := Self.supplierRepository.FindAll;
end;

function TSupplierController.SearchCNPJ(aCNPJ: String): TSupplierDTO;
var
  cnpj: TCNPJ;
begin
  try
    cnpj := TCNPJ.Create(aCNPJ);
  except
    raise Exception.Create('CNPJ Inválido');
  end;

  Result := Self.supplierRepository.SearchCNPJ(cnpj);
  if Assigned(cnpj) then
    cnpj.Free;
end;

end.


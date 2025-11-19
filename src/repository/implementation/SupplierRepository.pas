unit SupplierRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param,
  SupplierRepositoryInterface, CNPJ, CNPJApiInterface,
  SupplierModel, SupplierDTO;

type
  TSupplierRepository = class(TRepositoryBase, ISupplierRepository)
  private
    CNPJApi: ICNPJApi;
    helper: TDBHelper;
  public
    function Insert(aSupplier: TSupplier): TSupplier;
    function Update(aSupplier: TSupplier): TSupplier;
    function FindById(aSupplierId: Integer): TSupplier;
    function FindAll(): TObjectList<TSupplier>;
    function ExistsById(aSupplierId: Integer): Boolean;
    function DeleteById(aSupplierId: Integer): Boolean;
    function SearchCNPJ(aCNPJ: TCNPJ): TSupplierDTO;
    constructor Create(aHelper: TDBHelper; aCNPJApi: ICNPJApi);
  end;

implementation

{ TSupplierRepository }

constructor TSupplierRepository.Create(aHelper: TDBHelper; aCNPJApi: ICNPJApi);
begin
  inherited Create;
  Self.helper := aHelper;
  Self.CNPJApi := aCNPJApi;
end;

function TSupplierRepository.DeleteById(aSupplierId: Integer): Boolean;
begin
  Self.Query.SQL.Text := 'DELETE FROM suppliers WHERE id = :supplierId';

  Self.Query.ParamByName('supplierId').AsInteger := aSupplierId;
  try
    try
      Self.Query.ExecSQL;
      Result := Self.Query.RowsAffected > 0;
    except
      Result := False;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TSupplierRepository.ExistsById(aSupplierId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('suppliers', 'id', aSupplierId);
end;

function TSupplierRepository.FindAll: TObjectList<TSupplier>;
var
  suppliers: TObjectList<TSupplier>;
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM suppliers';
  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      suppliers := TObjectList<TSupplier>.Create;
      Result := suppliers;

      while not Self.Query.Eof do begin
        supplier := TSupplier.Create;
        supplier.id := Self.Query.FieldByName('id').AsInteger;
        supplier.tradeName := Self.Query.FieldByName('trade_name').AsString;
        supplier.legalName := Self.Query.FieldByName('legal_name').AsString;
        supplier.CNPJ := TCNPJ.Create(Self.Query.FieldByName('cnpj').AsString);
        supplier.CEP := Self.Query.FieldByName('cep').AsString;
        supplier.email := Self.Query.FieldByName('email').AsString;
        supplier.phone := Self.Query.FieldByName('phone').AsString;

        suppliers.Add(supplier);
        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TSupplierRepository.FindById(aSupplierId: Integer): TSupplier;
var
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM suppliers WHERE id = :supplierId';
  Self.Query.ParamByName('supplierId').AsInteger := aSupplierId;
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      supplier := TSupplier.Create;
      supplier.id := Self.Query.FieldByName('id').AsInteger;
      supplier.tradeName := Self.Query.FieldByName('trade_name').AsString;
      supplier.legalName := Self.Query.FieldByName('legal_name').AsString;
      supplier.CNPJ := TCNPJ.Create(Self.Query.FieldByName('cnpj').AsString);
      supplier.CEP := Self.Query.FieldByName('cep').AsString;
      supplier.email := Self.Query.FieldByName('email').AsString;
      supplier.phone := Self.Query.FieldByName('phone').AsString;

      Result := supplier;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TSupplierRepository.Insert(aSupplier: TSupplier): TSupplier;
var
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text := 'INSERT INTO suppliers(trade_name, legal_name, cnpj, cep, email, phone) ' +
  'VALUES (:tradeName, :legalName, :cnpj, :cep, :email, :phone)';

  Self.Query.ParamByName('tradeName').AsString := aSupplier.tradeName;
  Self.Query.ParamByName('legalName').AsString := aSupplier.legalName;
  Self.Query.ParamByName('cnpj').AsString := aSupplier.CNPJ.getCNPJ;
  Self.Query.ParamByName('cep').AsString := aSupplier.CEP;

  if aSupplier.email = '' then begin
    Self.Query.ParamByName('email').DataType := ftString;
    Self.Query.ParamByName('email').Clear
  end else
    Self.Query.ParamByName('email').AsString := aSupplier.email;

  if aSupplier.phone = '' then begin
    Self.Query.ParamByName('phone').DataType := ftString;
    Self.Query.ParamByName('phone').Clear;
  end else
    Self.Query.ParamByName('phone').AsString := aSupplier.phone;

  try
    if Self.helper.CheckIfAlreadyExists('suppliers', 'cnpj', aSupplier.CNPJ.getCNPJ) then
      raise Exception.Create('Já existe um fornecedor utilizando o CNPJ "' + aSupplier.CNPJ.getCNPJ + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      supplier := TSupplier.Create;
      supplier.id := Self.Query.FieldByName('id').AsInteger;
      supplier.tradeName := Self.Query.FieldByName('trade_name').AsString;
      supplier.legalName := Self.Query.FieldByName('legal_name').AsString;
      supplier.CNPJ := TCNPJ.Create(Self.Query.FieldByName('cnpj').AsString);
      supplier.CEP := Self.Query.FieldByName('cep').AsString;
      supplier.email := Self.Query.FieldByName('email').AsString;
      supplier.phone := Self.Query.FieldByName('phone').AsString;

      Result := supplier;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TSupplierRepository.SearchCNPJ(aCNPJ: TCNPJ): TSupplierDTO;
begin
  Result := Self.CNPJApi.SearchCNPJ(aCNPJ.getCNPJ);
end;

function TSupplierRepository.Update(aSupplier: TSupplier): TSupplier;
var
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text :=
  'UPDATE suppliers SET tradeName = :tradeName, legalName = :legalName, cnpj = :cnpj, ' +
  'cep = :cep, email = :email, phone = :phone WHERE id = :supplierId RETURNING *';

  Self.Query.ParamByName('tradeName').AsString := aSupplier.tradeName;
  Self.Query.ParamByName('legalName').AsString := aSupplier.legalName;
  Self.Query.ParamByName('cnpj').AsString := aSupplier.CNPJ.getCNPJ;
  Self.Query.ParamByName('cep').AsString := aSupplier.CEP;

  if aSupplier.email = '' then
    Self.Query.ParamByName('email').Clear
  else
    Self.Query.ParamByName('email').AsString := aSupplier.email;

  if aSupplier.phone = '' then
    Self.Query.ParamByName('phone').Clear
  else
    Self.Query.ParamByName('phone').AsString := aSupplier.phone;

  Self.Query.ParamByName('supplierId').AsInteger := aSupplier.id;

  try
    if Self.helper.CheckIfAlreadyExistsExcludingId('suppliers', 'cnpj', aSupplier.CNPJ.getCNPJ, aSupplier.id) then
      raise Exception.Create('Já existe um fornecedor utilizando o CNPJ "' + aSupplier.CNPJ.getCNPJ + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      supplier := TSupplier.Create;
      supplier.id := Self.Query.FieldByName('id').AsInteger;
      supplier.tradeName := Self.Query.FieldByName('trade_name').AsString;
      supplier.legalName := Self.Query.FieldByName('legal_name').AsString;
      supplier.CNPJ := TCNPJ.Create(Self.Query.FieldByName('cnpj').AsString);
      supplier.CEP := Self.Query.FieldByName('cep').AsString;
      supplier.email := Self.Query.FieldByName('email').AsString;
      supplier.phone := Self.Query.FieldByName('phone').AsString;

      Result := supplier;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.


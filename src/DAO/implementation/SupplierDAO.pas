unit SupplierDAO;

interface

uses
  DAOBase, SupplierDAOInterface, SupplierModel, DBHelper, CNPJ,
  System.Generics.Collections, System.SysUtils, Data.DB, System.StrUtils;

type
  TSupplierDAO = class(TDAOBase, ISupplierDAO)
  public
    function Insert(aSupplier: TSupplier): TSupplier;
    function SelectById(aSupplierId: Integer): TSupplier;
    function SelectAll(): TObjectList<TSupplier>;
    function Update(aSupplier: TSupplier): TSupplier;
    function DeleteById(aSupplierId: Integer): Boolean;
    function SelectByToolTypeId(aToolTypeId: Integer): TSupplier;
  end;

implementation

{ TSupplierDAO }

function TSupplierDAO.DeleteById(aSupplierId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('DELETE FROM suppliers WHERE id = %d', [aSupplierId]);
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

function TSupplierDAO.Insert(aSupplier: TSupplier): TSupplier;
var
  supplier: TSupplier;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO suppliers(trade_name, legal_name, cnpj, cep, email, phone) ' +
    'VALUES (%s, %s, %s, %s, %s, %s) RETURNING *',
    [
      QuotedStr(aSupplier.tradeName),
      QuotedStr(aSupplier.legalName),
      QuotedStr(aSupplier.CNPJ.getCNPJ),
      QuotedStr(aSupplier.CEP),
      IfThen(aSupplier.email = '', 'NULL', QuotedStr(aSupplier.email)),
      IfThen(aSupplier.phone = '', 'NULL', QuotedStr(aSupplier.phone)),
      aSupplier.id
    ]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExists('suppliers', 'cnpj', aSupplier.CNPJ.getCNPJ) then
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
    helper.Free;
  end;
end;

function TSupplierDAO.SelectById(aSupplierId: Integer): TSupplier;
var
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT * FROM suppliers WHERE id = %d', [aSupplierId]);
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

function TSupplierDAO.SelectByToolTypeId(aToolTypeId: Integer): TSupplier;
var
  supplier: TSupplier;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT s.* FROM tools_models AS t JOIN suppliers AS s ' +
    'ON t.id_supplier = s.id WHERE t.id = %d',
    [aToolTypeId]
  );

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

function TSupplierDAO.SelectAll: TObjectList<TSupplier>;
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

function TSupplierDAO.Update(aSupplier: TSupplier): TSupplier;
var
  supplier: TSupplier;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'UPDATE suppliers SET trade_name = %s, legal_name = %s, cnpj = %s, cep = %s, ' +
    'email = %s, phone = %s WHERE id = %d RETURNING *',
    [
      QuotedStr(aSupplier.tradeName),
      QuotedStr(aSupplier.legalName),
      QuotedStr(aSupplier.CNPJ.getCNPJ),
      QuotedStr(aSupplier.CEP),
      IfThen(aSupplier.email = '', 'NULL', QuotedStr(aSupplier.email)),
      IfThen(aSupplier.phone = '', 'NULL', QuotedStr(aSupplier.phone)),
      aSupplier.id
    ]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('suppliers', 'cnpj', aSupplier.CNPJ.getCNPJ, aSupplier.id) then
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
    helper.Free;
  end;
end;

end.


unit ToolTypeRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase,
  ToolTypeRepositoryInterface, ToolTypeModel, SupplierRepositoryInterface;

type
  TToolTypeRepository = class(TRepositoryBase, IToolTypeRepository)
  private
    supplierRepository: ISupplierRepository;
    helper: TDBHelper;
  public
    function Insert(aToolType: TToolType): TToolType;
    function Update(aToolType: TToolType): TToolType;
    function FindById(aToolId: Integer): TToolType;
    function FindAll(): TObjectList<TToolType>;
    function ExistsById(aToolId: Integer): Boolean;
    function DeleteById(aToolId: Integer): Boolean;
    constructor Create(aHelper: TDBHelper; aSupplierRepository: ISupplierRepository);
  end;

implementation

{ TToolTypeRepository }

constructor TToolTypeRepository.Create(aHelper: TDBHelper;
  aSupplierRepository: ISupplierRepository);
begin
  Self.helper := aHelper;
  Self.supplierRepository := aSupplierRepository;
end;

function TToolTypeRepository.DeleteById(aToolId: Integer): Boolean;
begin
  Self.Query.SQL.Text := 'DELETE FROM tools_models WHERE id = :toolId';
  Self.Query.ParamByName('toolId').AsInteger := aToolId;
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

function TToolTypeRepository.ExistsById(aToolId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('tools_models', 'id', aToolId);
end;

function TToolTypeRepository.FindAll: TObjectList<TToolType>;
var
  toolTypes: TObjectList<TToolType>;
  toolType: TToolType;
begin
  Result := nil;
  Self.Query.SQL.Text := 'SELECT * FROM tools_models';
  try
    Self.Query.Open;
    if not Self.Query.IsEmpty then
    begin
      toolTypes := TObjectList<TToolType>.Create;
      Result := toolTypes;

      while not Self.Query.Eof do
      begin
        toolType := TToolType.Create;
        toolType.id := Self.Query.FieldByName('id').AsInteger;
        toolType.code := Self.Query.FieldByName('code').AsString;
        toolType.description := Self.Query.FieldByName('description').AsString;
        toolType.family := Self.Query.FieldByName('family').AsString;
        toolType.usage := Self.Query.FieldByName('usage').AsString;
        toolType.price := Self.Query.FieldByName('price').AsCurrency;
        toolType.image := Self.Query.FieldByName('image').AsString;

        toolType.supplier := Self.supplierRepository.FindById(
          Self.Query.FieldByName('id_supplier').AsInteger
        );

        toolTypes.Add(toolType);
        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolTypeRepository.FindById(aToolId: Integer): TToolType;
var
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM tools_models WHERE id = :toolId';
  Self.Query.ParamByName('toolId').AsInteger := aToolId;
  try
    Self.Query.Open;
    if not Self.Query.IsEmpty then
    begin
      toolType := TToolType.Create;
      toolType.id := Self.Query.FieldByName('id').AsInteger;
      toolType.code := Self.Query.FieldByName('code').AsString;
      toolType.description := Self.Query.FieldByName('description').AsString;
      toolType.family := Self.Query.FieldByName('family').AsString;
      toolType.usage := Self.Query.FieldByName('usage').AsString;
      toolType.price := Self.Query.FieldByName('price').AsCurrency;
      toolType.image := Self.Query.FieldByName('image').AsString;

      toolType.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );

      Result := toolType;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolTypeRepository.Insert(aToolType: TToolType): TToolType;
var
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'INSERT INTO tools_models(code, description, family, "usage", id_supplier, price, image) ' +
    'VALUES(:code, :description, :family, :usage, :id_supplier, :price, :image) RETURNING *';

  Self.Query.ParamByName('code').AsString := aToolType.code;
  Self.Query.ParamByName('description').AsString := aToolType.description;
  Self.Query.ParamByName('family').AsString := aToolType.family;
  Self.Query.ParamByName('usage').AsString := aToolType.usage;
  Self.Query.ParamByName('id_supplier').AsInteger := aToolType.supplier.id;
  Self.Query.ParamByName('price').AsCurrency := aToolType.price;

  if aToolType.image = '' then
    Self.Query.ParamByName('image').Clear
  else
    Self.Query.ParamByName('image').AsString := aToolType.image;

  try
    if Self.helper.CheckIfAlreadyExists('tools_models', 'code', aToolType.code) then
      raise Exception.Create('Já existe um modelo de ferramenta com o código "' + aToolType.code + '"');

    Self.Query.Open;
    if not Self.Query.IsEmpty then
    begin
      toolType := TToolType.Create;
      toolType.id := Self.Query.FieldByName('id').AsInteger;
      toolType.code := Self.Query.FieldByName('code').AsString;
      toolType.description := Self.Query.FieldByName('description').AsString;
      toolType.family := Self.Query.FieldByName('family').AsString;
      toolType.usage := Self.Query.FieldByName('usage').AsString;
      toolType.price := Self.Query.FieldByName('price').AsCurrency;
      toolType.image := Self.Query.FieldByName('image').AsString;
      toolType.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );

      Result := toolType;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolTypeRepository.Update(aToolType: TToolType): TToolType;
var
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'UPDATE tools_models SET ' +
    '  code = :code, ' +
    '  description = :description, ' +
    '  family = :family, ' +
    '  "usage" = :usage, ' +
    '  id_supplier = :id_supplier, ' +
    '  price = :price, ' +
    '  image = :image ' +
    'WHERE id = :id ' +
    'RETURNING *';

  Self.Query.ParamByName('id').AsInteger := aToolType.id;
  Self.Query.ParamByName('code').AsString := aToolType.code;
  Self.Query.ParamByName('description').AsString := aToolType.description;
  Self.Query.ParamByName('family').AsString := aToolType.family;
  Self.Query.ParamByName('usage').AsString := aToolType.usage;
  Self.Query.ParamByName('id_supplier').AsInteger := aToolType.supplier.id;
  Self.Query.ParamByName('price').AsCurrency := aToolType.price;

  if aToolType.image = '' then
    Self.Query.ParamByName('image').Clear
  else
    Self.Query.ParamByName('image').AsString := aToolType.image;

  try
    if Self.helper.CheckIfAlreadyExistsExcludingId('tools_models', 'code', aToolType.code, aToolType.id) then
      raise Exception.Create('Já existe um modelo de ferramenta com o código "' + aToolType.code + '"');

    Self.Query.Open;
    if not Self.Query.IsEmpty then
    begin
      toolType := TToolType.Create;
      toolType.id := Self.Query.FieldByName('id').AsInteger;
      toolType.code := Self.Query.FieldByName('code').AsString;
      toolType.description := Self.Query.FieldByName('description').AsString;
      toolType.family := Self.Query.FieldByName('family').AsString;
      toolType.usage := Self.Query.FieldByName('usage').AsString;
      toolType.price := Self.Query.FieldByName('price').AsCurrency;
      toolType.image := Self.Query.FieldByName('image').AsString;

      toolType.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );

      Result := toolType;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.


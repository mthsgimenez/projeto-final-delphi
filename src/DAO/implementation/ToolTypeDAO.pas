unit ToolTypeDAO;

interface

uses
  DAOBase, ToolTypeModel, DBHelper, System.StrUtils, FireDAC.Stan.Param,
  ToolTypeDAOInterface, System.Generics.Collections, System.SysUtils, Data.DB;

type
  TToolTypeDAO = class(TDAOBase, IToolTypeDAO)
  public
    function Insert(aTool: TToolType): TToolType;
    function SelectById(aToolId: Integer): TToolType;
    function SelectAll(): TObjectList<TToolType>;
    function Update(aTool: TToolType): TToolType;
    function DeleteById(aToolId: Integer): Boolean;
    function SelectByToolId(aToolId: Integer): TToolType;
  end;

implementation

{ TToolTypeDAO }

function TToolTypeDAO.DeleteById(aToolId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('DELETE FROM tools_models WHERE id = %d', [aToolId]);
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

function TToolTypeDAO.Insert(aTool: TToolType): TToolType;
var
  helper: TDBHelper;
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'INSERT INTO tools_models(code, description, family, "usage", id_supplier, price, image) ' +
    'VALUES(:code, :description, :family, :usage, :id_supplier, :price, :image) RETURNING *';

  Self.Query.ParamByName('code').AsString := aTool.code;
  Self.Query.ParamByName('description').AsString := aTool.description;
  Self.Query.ParamByName('family').AsString := aTool.family;
  Self.Query.ParamByName('usage').AsString := aTool.usage;
  Self.Query.ParamByName('id_supplier').AsInteger := aTool.supplier.id;
  Self.Query.ParamByName('price').AsCurrency := aTool.price;

  if aTool.image = '' then
    Self.Query.ParamByName('image').Clear
  else
    Self.Query.ParamByName('image').AsString := aTool.image;

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExists('tools_models', 'code', aTool.code) then
      raise Exception.Create('Já existe um modelo de ferramenta com o código "' + aTool.code + '"');

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
      toolType.supplier := nil;

      Result := toolType;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

function TToolTypeDAO.SelectAll: TObjectList<TToolType>;
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

        toolType.supplier := nil;

        toolTypes.Add(toolType);
        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolTypeDAO.SelectById(aToolId: Integer): TToolType;
var
  toolType: TToolType;
begin
  Result := nil;
  Self.Query.SQL.Text := Format('SELECT * FROM tools_models WHERE id = %d', [aToolId]);
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

      toolType.supplier := nil;

      Result := toolType;
    end;
  finally
    Self.Query.Close;
  end;
end;


function TToolTypeDAO.SelectByToolId(aToolId: Integer): TToolType;
var
  toolType: TToolType;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT tm.* FROM tools t JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'WHERE t.id = %d',
    [aToolId]
  );

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      toolType := TToolType.Create;
      toolType.id := Self.Query.FieldByName('id').AsInteger;
      toolType.code := Self.Query.FieldByName('code').AsString;
      toolType.description := Self.Query.FieldByName('description').AsString;
      toolType.family := Self.Query.FieldByName('family').AsString;
      toolType.usage := Self.Query.FieldByName('usage').AsString;
      toolType.price := Self.Query.FieldByName('price').AsCurrency;
      toolType.image := Self.Query.FieldByName('image').AsString;

      toolType.supplier := nil;

      Result := toolType;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolTypeDAO.Update(aTool: TToolType): TToolType;
var
  helper: TDBHelper;
  toolType: TToolType;
begin
  Result := nil;
  helper := TDBHelper.Create;

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

  Self.Query.ParamByName('id').AsInteger := aTool.id;
  Self.Query.ParamByName('code').AsString := aTool.code;
  Self.Query.ParamByName('description').AsString := aTool.description;
  Self.Query.ParamByName('family').AsString := aTool.family;
  Self.Query.ParamByName('usage').AsString := aTool.usage;
  Self.Query.ParamByName('id_supplier').AsInteger := aTool.supplier.id;
  Self.Query.ParamByName('price').AsCurrency := aTool.price;

  if aTool.image = '' then
    Self.Query.ParamByName('image').Clear
  else
    Self.Query.ParamByName('image').AsString := aTool.image;

  try
    if helper.CheckIfAlreadyExistsExcludingId('tools_models', 'code', aTool.code, aTool.id) then
      raise Exception.Create('Já existe um modelo de ferramenta com o código "' + aTool.code + '"');

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

      toolType.supplier := nil;

      Result := toolType;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

end.

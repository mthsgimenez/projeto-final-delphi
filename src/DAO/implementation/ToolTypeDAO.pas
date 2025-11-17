unit ToolTypeDAO;

interface

uses
  DAOBase, ToolTypeModel, DBHelper, System.StrUtils,
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

  Self.Query.SQL.Text := Format(
    'INSERT INTO tools_models(code, description, family, "usage", id_supplier, price, image) ' +
    'VALUES(%s, %s, %s, %s, %d, %f, %s) RETURNING *',
    [
      QuotedStr(aTool.code),
      QuotedStr(aTool.description),
      QuotedStr(aTool.family),
      QuotedStr(aTool.usage),
      aTool.supplier.id,
      aTool.price,
      IfThen(aTool.image = '', 'NULL', QuotedStr(aTool.image))
    ]
  );

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

  Self.Query.SQL.Text := Format(
    'UPDATE tools_models SET code = %s, description = %s, family = %s, "usage" = %s, ' +
    'id_supplier = %d, price = %f, image = %s WHERE id = %d RETURNING *',
    [
      QuotedStr(aTool.code),
      QuotedStr(aTool.description),
      QuotedStr(aTool.family),
      QuotedStr(aTool.usage),
      aTool.supplier.id,
      aTool.price,
      IfThen(aTool.image = '', 'NULL', QuotedStr(aTool.image)),
      aTool.id
    ]);

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

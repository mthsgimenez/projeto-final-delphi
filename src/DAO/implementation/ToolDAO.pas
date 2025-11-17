unit ToolDAO;

interface

uses DAOBase, ToolDAOInterface, ToolModel,
  System.Generics.Collections, System.SysUtils, Data.DB;

type
  TToolDAO = class(TDAOBase, IToolDAO)
  public
    function Insert(aTool: TTool): TTool;
    function SelectById(aToolId: Integer): TTool;
    function SelectAll(): TObjectList<TTool>;
    function Update(aTool: TTool): TTool;
    function DeleteById(aToolId: Integer): Boolean;
    function SelectByStatus(aStatus: TStatus): TObjectList<TTool>;
  end;

implementation

{ TToolDAO }

function TToolDAO.DeleteById(aToolId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('DELETE FROM tools WHERE id = %d', [aToolId]);

  try
    Self.Query.ExecSQL;
    Result := Self.Query.RowsAffected > 0;
  except
    Result := False;
  end;
end;

function TToolDAO.SelectById(aToolId: Integer): TTool;
var
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT * FROM tools WHERE id = %d', [aToolId]);

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tool := TTool.Create;
      tool.id := Self.Query.FieldByName('id').AsInteger;
      tool.model := nil;
      tool.state := StringToState(Self.Query.FieldByName('state').AsString);
      tool.honing_num := Self.Query.FieldByName('honing_num').AsInteger;
      tool.storage := nil;
      tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

      Result := tool;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolDAO.SelectAll: TObjectList<TTool>;
var
  tools: TObjectList<TTool>;
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM tools';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tools := TObjectList<TTool>.Create;
      Result := tools;

      while not Self.Query.Eof do begin
        tool := TTool.Create;
        tool.id := Self.Query.FieldByName('id').AsInteger;
        tool.model := nil;
        tool.state := StringToState(Self.Query.FieldByName('state').AsString);
        tool.honing_num := Self.Query.FieldByName('honing_num').AsInteger;
        tool.storage := nil;
        tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

        tools.Add(tool);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolDAO.Insert(aTool: TTool): TTool;
var
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO tools (code, id_tool_model, state, honing_num, id_storage, status) ' +
    'VALUES (%s, %d, %s, %d, %d, %s) RETURNING *',
    [QuotedStr(aTool.model.code), aTool.model.id, QuotedStr(StateToString(aTool.state)), aTool.honing_num, aTool.storage.id, QuotedStr(StatusToString(aTool.status))]);

  Self.Query.Connection.StartTransaction;
  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      tool := TTool.Create;

      tool.id := Self.Query.FieldByName('id').AsInteger;
      tool.code := Self.Query.FieldByName('code').AsString;
      tool.code := Format('%s_%d', [tool.code, tool.id]);
      tool.model := nil;
      tool.state := StringToState(Self.Query.FieldByName('state').AsString);
      tool.honing_num := Self.Query.FieldByName('honing_num').AsInteger;
      tool.storage := nil;
      tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

      Self.Query.Close;

      Self.Query.SQL.Text := Format(
        'UPDATE tools SET code = %s ' +
        'WHERE id = %d',
        [QuotedStr(tool.code), tool.id]
      );

      Self.Query.ExecSQL;

      Self.Query.Connection.Commit;
      Result := tool;
    end;
  except
    Self.Query.Connection.Rollback;
  end;
end;

function TToolDAO.Update(aTool: TTool): TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'UPDATE tools SET state = %s, honing_num = %d, id_storage = %d, status = %s ' +
    'WHERE id = %d RETURNING *',
    [QuotedStr(StateToString(aTool.state)), aTool.honing_num, aTool.storage.id, QuotedStr(StatusToString(aTool.status)), aTool.id]);

  try
    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      Result := TTool.Create;
      Result.id := Self.Query.FieldByName('id').AsInteger;
      Result.model := nil;
      Result.state := StringToState(Self.Query.FieldByName('state').AsString);
      Result.honing_num := Self.Query.FieldByName('honing_num').AsInteger;
      Result.storage := nil;
      Result.status := StringToStatus(Self.Query.FieldByName('status').AsString);
    end;
  finally
    Self.Query.Close;
  end;
end;

function TToolDAO.SelectByStatus(aStatus: TStatus): TObjectList<TTool>;
var
  tools: TObjectList<TTool>;
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text := Format('SELECT * FROM tools WHERE status = %s', [QuotedStr(StatusToString(aStatus))]);

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      tools := TObjectList<TTool>.Create;
      Result := tools;

      while not Self.Query.Eof do begin
        tool := TTool.Create;
        tool.id := Self.Query.FieldByName('id').AsInteger;
        tool.model := nil;
        tool.state := StringToState(Self.Query.FieldByName('state').AsString);
        tool.honing_num := Self.Query.FieldByName('honing_num').AsInteger;
        tool.storage := nil;
        tool.status := StringToStatus(Self.Query.FieldByName('status').AsString);

        tools.Add(tool);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.


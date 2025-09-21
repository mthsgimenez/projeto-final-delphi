unit DBHelper;

interface

uses RepositoryBase, System.SysUtils;

type TDBHelper = class(TRepositoryBase)
  private

  public
    function CheckIfAlreadyExists(aTable: String; aColumn: String; value: Integer): Boolean; overload;
    function CheckIfAlreadyExists(aTable: String; aColumn: String; value: String): Boolean; overload;
end;

implementation

{ TDBHelper }

function TDBHelper.CheckIfAlreadyExists(aTable, aColumn,
  value: String): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%s', [aTable, aColumn, QuotedStr(value)]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

function TDBHelper.CheckIfAlreadyExists(aTable, aColumn: String;
  value: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%d', [aTable, aColumn, value]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

end.

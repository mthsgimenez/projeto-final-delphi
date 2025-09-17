unit DBHelper;

interface

uses RepositoryBase, System.SysUtils, Variants;

type TDBHelper = class(TRepositoryBase)
  public
    function CheckIfAlreadyExists(aTable: String; aColumn: String; value: Variant): Boolean;
end;

implementation

{ TDBHelper }

function TDBHelper.CheckIfAlreadyExists(aTable, aColumn: String;
  value: Variant): Boolean;
begin
  if VarIsStr(value) then begin
    Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%s', [aTable, aColumn, QuotedStr(value)]);
  end else begin
    Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%d', [aTable, aColumn, value]);
  end;

  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

end.

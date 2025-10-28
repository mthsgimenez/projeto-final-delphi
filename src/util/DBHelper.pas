unit DBHelper;

interface

uses DAOBase, System.SysUtils;

type TDBHelper = class(TDAOBase)
  private

  public
    function CheckIfAlreadyExists(aTable: String; aColumn: String; aValue: Integer): Boolean; overload;
    function CheckIfAlreadyExists(aTable: String; aColumn: String; aValue: String): Boolean; overload;
    function CheckIfAlreadyExistsExcludingId(aTable: String; aColumn: String; aValue: String; aId: Integer): Boolean; overload;
    function CheckIfAlreadyExistsExcludingId(aTable: String; aColumn: String; aValue: Integer; aId: Integer): Boolean; overload;
end;

implementation

{ TDBHelper }

function TDBHelper.CheckIfAlreadyExists(aTable, aColumn,
  aValue: String): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%s',
                          [aTable, aColumn, QuotedStr(aValue)]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

function TDBHelper.CheckIfAlreadyExists(aTable, aColumn: String;
  aValue: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%d',
                          [aTable, aColumn, aValue]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

function TDBHelper.CheckIfAlreadyExistsExcludingId(aTable, aColumn: String;
  aValue, aId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%d AND id != %d',
                          [aTable, aColumn, aValue, aId]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

function TDBHelper.CheckIfAlreadyExistsExcludingId(aTable, aColumn,
  aValue: String; aId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format('SELECT COUNT(*) FROM %s WHERE %s=%s AND id != %d',
                          [aTable, aColumn, QuotedStr(aValue), aId]);
  Self.Query.Open();
  Result := Self.Query.Fields[0].AsInteger > 0;
end;

end.

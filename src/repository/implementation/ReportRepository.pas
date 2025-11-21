unit ReportRepository;

interface

uses ReportRepositoryInterface, FireDAC.Comp.Client, System.SysUtils, Data.DB, DBConnection;

type TReportRepository = class(TInterfacedObject, IReportRepository)
  function GetPurchaseReportDataset(aMonthsToList: Integer): TFDQuery;
end;

implementation

{ TReportRepository }

function TReportRepository.GetPurchaseReportDataset(aMonthsToList: Integer): TFDQuery;
var
  query: TFDQuery;
  connection: TFDConnection;
begin
  connection := DBConnection.Connection.FDConnection;
  query := TFDQuery.Create(connection);
  query.Connection := connection;

  query.SQL.Text := Format(
    'SELECT ' +
    '  DATE_TRUNC(''month'', po.issued_at) AS month, ' +
    '  TO_CHAR(DATE_TRUNC(''month'', po.issued_at), ''MM/YYYY'')::varchar AS month_display, ' +
    '  SUM(pot.tool_quantity) AS total_tools, ' +
    '  SUM(pot.tool_quantity * tm.price::numeric) AS total_spent ' +
    'FROM purchase_orders po ' +
    'JOIN purchase_order_tools pot ' +
    '  ON pot.id_purchase_order = po.id ' +
    'JOIN tools_models tm ' +
    '  ON tm.id = pot.id_tool_model ' +
    'WHERE ' +
    '  po.issued_at >= DATE_TRUNC(''month'', CURRENT_DATE) - INTERVAL ''%d months'' ' +
    'GROUP BY ' +
    '  DATE_TRUNC(''month'', po.issued_at) ' +
    'ORDER BY ' +
    '  month DESC;', [aMonthsToList]);

  query.Open;

  Result := query;
end;

end.

unit ReportRepository;

interface

uses ReportRepositoryInterface, FireDAC.Comp.Client, System.SysUtils, Data.DB, DBConnection, FireDAC.Stan.Param;

type TReportRepository = class(TInterfacedObject, IReportRepository)
  function GetSpentDataset(aStartMonth: TDate; aEndMonth: TDate): TFDQuery;
  function GetLowStockDataset(aLimit: Integer): TFDQuery;
  function GetMostBoughtDataset(aStartDate: TDate; aEndDate: TDate): TFDQuery;
  function GetMostHonedDataset(aStartDate: TDate; aEndDate: TDate): TFDQuery;
end;

implementation

{ TReportRepository }

function TReportRepository.GetLowStockDataset(aLimit: Integer): TFDQuery;
var
  query: TFDQuery;
  connection: TFDConnection;
begin
  connection := DBConnection.Connection.FDConnection;
  query := TFDQuery.Create(connection);
  query.Connection := connection;

  query.SQL.Text :=
    'SELECT ' +
    '    tm.code AS model, ' +
    '    tm.description AS description, ' +
    '    COUNT(CASE WHEN t.status = ''AVAILABLE'' THEN 1 END) AS available_stock, ' +
    '    COUNT(CASE WHEN t.status = ''HONING'' THEN 1 END) AS honing_stock, ' +
    '    COUNT(CASE WHEN t.status = ''IN_USE'' THEN 1 END) AS in_use_stock, ' +
    '    COUNT(t.id) AS total_stock ' +
    'FROM tools t ' +
    'JOIN tools_models tm ON tm.id = t.id_tool_model ' +
    'GROUP BY tm.code, tm.description ' +
    'HAVING COUNT(CASE WHEN t.status = ''AVAILABLE'' THEN 1 END) <= :limit ' +
    'ORDER BY available_stock ASC;';
  query.ParamByName('limit').AsInteger := aLimit;

  query.Open;

  Result := query;
end;

function TReportRepository.GetMostBoughtDataset(aStartDate,
  aEndDate: TDate): TFDQuery;
var
  query: TFDQuery;
  connection: TFDConnection;
begin
  connection := DBConnection.Connection.FDConnection;
  query := TFDQuery.Create(connection);
  query.Connection := connection;

  query.SQL.Text :=
    'SELECT ' +
    '    tm.code AS tool_model_code, ' +
    '    tm.description AS tool_model_description, ' +
    '    SUM(pot.tool_quantity) AS total_quantity, ' +
    '    SUM(pot.tool_quantity * tm.price) AS total_spent ' +
    'FROM purchase_order_tools pot ' +
    'JOIN purchase_orders po ON pot.id_purchase_order = po.id ' +
    'JOIN tools_models tm ON pot.id_tool_model = tm.id ' +
    'WHERE po.issued_at BETWEEN :startDate AND :endDate ' +
    'GROUP BY tm.id, tm.code, tm.description ' +
    'ORDER BY total_quantity DESC, total_spent DESC;';
  query.ParamByName('startDate').AsDate := aStartDate;
  query.ParamByName('endDate').AsDate := aEndDate;

  query.Open;

  Result := query;
end;

function TReportRepository.GetMostHonedDataset(aStartDate,
  aEndDate: TDate): TFDQuery;
var
  query: TFDQuery;
  connection: TFDConnection;
begin
  connection := DBConnection.Connection.FDConnection;
  query := TFDQuery.Create(connection);
  query.Connection := connection;

  query.SQL.Text :=
    'SELECT ' +
    '    tm.code AS tool_model_code, ' +
    '    tm.description AS tool_model_description, ' +
    '    COUNT(sot.id) AS total_honing, ' +
    '    SUM(so.price) AS total_spent ' +
    'FROM service_order_tools sot ' +
    'JOIN service_orders so ON sot.id_service_order = so.id ' +
    'JOIN tools t ON sot.id_tool = t.id ' +
    'JOIN tools_models tm ON t.id_tool_model = tm.id ' +
    'WHERE so.issued_at BETWEEN :startDate AND :endDate ' +
    'GROUP BY tm.id, tm.code, tm.description ' +
    'ORDER BY total_honing DESC, total_spent DESC;';
  query.ParamByName('startDate').AsDate := aStartDate;
  query.ParamByName('endDate').AsDate := aEndDate;

  query.Open;

  Result := query;
end;

function TReportRepository.GetSpentDataset(aStartMonth: TDate; aEndMonth: TDate): TFDQuery;
var
  query: TFDQuery;
  connection: TFDConnection;
begin
  connection := DBConnection.Connection.FDConnection;
  query := TFDQuery.Create(connection);
  query.Connection := connection;

  query.SQL.Text :=
    'WITH purchase AS (' + sLineBreak +
    '    SELECT ' + sLineBreak +
    '        s.trade_name AS supplier,' + sLineBreak +
    '        DATE_TRUNC(''month'', po.issued_at) AS month,' + sLineBreak +
    '        TO_CHAR(DATE_TRUNC(''month'', po.issued_at), ''TMMonth YYYY'')::varchar AS month_label,' + sLineBreak +
    '        SUM(t.price::numeric * pot.tool_quantity) AS purchase_spent' + sLineBreak +
    '    FROM purchase_orders po' + sLineBreak +
    '    JOIN suppliers s ON s.id = po.id_supplier' + sLineBreak +
    '    JOIN purchase_order_tools pot ON pot.id_purchase_order = po.id' + sLineBreak +
    '    JOIN tools_models t ON t.id = pot.id_tool_model' + sLineBreak +
    '    WHERE po.status = ''CLOSED''' + sLineBreak +
    '      AND DATE_TRUNC(''month'', po.issued_at) >= DATE_TRUNC(''month'', :startMonth)' + sLineBreak +
    '      AND DATE_TRUNC(''month'', po.issued_at) <= DATE_TRUNC(''month'', :endMonth)' + sLineBreak +
    '    GROUP BY supplier, month' + sLineBreak +
    '),' + sLineBreak +
    'service AS (' + sLineBreak +
    '    SELECT ' + sLineBreak +
    '        s.trade_name AS supplier,' + sLineBreak +
    '        DATE_TRUNC(''month'', so.issued_at) AS month,' + sLineBreak +
    '        TO_CHAR(DATE_TRUNC(''month'', so.issued_at), ''TMMonth YYYY'')::varchar AS month_label,' + sLineBreak +
    '        SUM(so.price::numeric) AS service_spent' + sLineBreak +
    '    FROM service_orders so' + sLineBreak +
    '    JOIN suppliers s ON s.id = so.id_supplier' + sLineBreak +
    '    WHERE so.status = ''CLOSED''' + sLineBreak +
    '      AND DATE_TRUNC(''month'', so.issued_at) >= DATE_TRUNC(''month'', :startMonth)' + sLineBreak +
    '      AND DATE_TRUNC(''month'', so.issued_at) <= DATE_TRUNC(''month'', :endMonth)' + sLineBreak +
    '    GROUP BY supplier, month' + sLineBreak +
    ')' + sLineBreak +
    'SELECT ' + sLineBreak +
    '    COALESCE(p.supplier, s.supplier) AS supplier,' + sLineBreak +
    '    COALESCE(p.month, s.month) AS month,' + sLineBreak +
    '    COALESCE(p.month_label, s.month_label) AS month_label,' + sLineBreak +
    '    p.purchase_spent,' + sLineBreak +
    '    s.service_spent,' + sLineBreak +
    '    COALESCE(p.purchase_spent, 0) + COALESCE(s.service_spent, 0) AS total_spent' + sLineBreak +
    'FROM purchase p' + sLineBreak +
    'FULL JOIN service s ' + sLineBreak +
    '    ON p.supplier = s.supplier' + sLineBreak +
    '    AND p.month = s.month' + sLineBreak +
    'ORDER BY month, supplier;';

  query.ParamByName('startMonth').AsDate := aStartMonth;
  query.ParamByName('endMonth').AsDate := aEndMonth;

  query.Open;

  Result := query;
end;

end.

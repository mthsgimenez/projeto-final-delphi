unit ReportRepositoryInterface;

interface

uses FireDAC.Comp.Client;

type IReportRepository = interface
  function GetPurchaseReportDataset(aMonthsToList: Integer): TFDQuery;
end;

implementation

end.

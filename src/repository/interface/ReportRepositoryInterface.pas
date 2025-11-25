unit ReportRepositoryInterface;

interface

uses FireDAC.Comp.Client;

type IReportRepository = interface
  function GetSpentDataset(aStartMonth: TDate; aEndMonth: TDate): TFDQuery;
  function GetLowStockDataset(aLimit: Integer): TFDQuery;
  function GetMostBoughtDataset(aStartDate: TDate; aEndDate: TDate): TFDQuery;
  function GetMostHonedDataset(aStartDate: TDate; aEndDate: TDate): TFDQuery;
end;

implementation

end.

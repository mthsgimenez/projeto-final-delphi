unit ReportController;

interface

uses frxClass, frxDBSet, ReportRepositoryInterface, System.SysUtils, System.Classes, Windows;

type TReportController = class
  private
    reportRepository: IReportRepository;
  public
    procedure ShowSpentReport(aStartDate: TDate; aEndDate: TDate);
    procedure ShowStockReport(aLimit: Integer);
    procedure ShowUsageReport(aStartDate: TDate; aEndDate: TDate);

    constructor Create(aReportRepository: IReportRepository);
end;

implementation

{ TReportController }

constructor TReportController.Create(aReportRepository: IReportRepository);
begin
  Self.reportRepository := aReportRepository;
end;

procedure TReportController.ShowSpentReport(aStartDate: TDate; aEndDate: TDate);
var
  report: TfrxReport;
  dsSpent: TfrxDBDataset;
  RS: TResourceStream;
begin
  report := TfrxReport.Create(nil);
  dsSpent := TfrxDBDataset.Create(nil);

  RS := TResourceStream.Create(HInstance, 'SpentReport', RT_RCDATA);
  try
    dsSpent.DataSet := Self.reportRepository.GetSpentDataset(aStartDate, aEndDate);
    dsSpent.UserName := 'dsSpent';
    report.DataSets.Add(dsSpent);

    report.LoadFromStream(RS);
    report.ShowReport();
  finally
    RS.Free;
    report.Free;
    dsSpent.Free;
  end;
end;

procedure TReportController.ShowStockReport(aLimit: Integer);
var
  report: TfrxReport;
  dsStock: TfrxDBDataset;
  RS: TResourceStream;
begin
  report := TfrxReport.Create(nil);
  dsStock := TfrxDBDataset.Create(nil);

  RS := TResourceStream.Create(HInstance, 'StockReport', RT_RCDATA);
  try
    dsStock.DataSet := Self.reportRepository.GetLowStockDataset(aLimit);
    dsStock.UserName := 'dsStock';
    report.DataSets.Add(dsStock);

    report.LoadFromStream(RS);
    report.ShowReport();
  finally
    RS.Free;
    report.Free;
    dsStock.Free;
  end;
end;

procedure TReportController.ShowUsageReport(aStartDate, aEndDate: TDate);
var
  report: TfrxReport;
  dsMostBought, dsMostHoned: TfrxDBDataset;
  RS: TResourceStream;
begin
  report := TfrxReport.Create(nil);
  dsMostBought := TfrxDBDataset.Create(nil);
  dsMostHoned := TfrxDBDataset.Create(nil);

  RS := TResourceStream.Create(HInstance, 'UsageReport', RT_RCDATA);
  try
    dsMostBought.DataSet := Self.reportRepository.GetMostBoughtDataset(aStartDate, aEndDate);
    dsMostHoned.DataSet := Self.reportRepository.GetMostHonedDataset(aStartDate, aEndDate);
    dsMostBought.UserName := 'dsMostBought';
    dsMostHoned.UserName := 'dsMostHoned';
    report.DataSets.Add(dsMostBought);
    report.DataSets.Add(dsMostHoned);

    report.LoadFromStream(RS);
    report.ShowReport();
  finally
    RS.Free;
    report.Free;
    dsMostHoned.Free;
    dsMostBought.Free;
  end;
end;

end.

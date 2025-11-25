unit ReportController;

interface

uses frxClass, frxDBSet, ReportRepositoryInterface, System.SysUtils;

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
begin
  report := TfrxReport.Create(nil);
  dsSpent := TfrxDBDataset.Create(nil);

  try
    dsSpent.DataSet := Self.reportRepository.GetSpentDataset(aStartDate, aEndDate);
    dsSpent.UserName := 'dsSpent';
    report.DataSets.Add(dsSpent);

    report.LoadFromFile('spent.fr3');
    report.ShowReport();
  finally
    report.Free;
    dsSpent.Free;
  end;
end;

procedure TReportController.ShowStockReport(aLimit: Integer);
var
  report: TfrxReport;
  dsStock: TfrxDBDataset;
begin
  report := TfrxReport.Create(nil);
  dsStock := TfrxDBDataset.Create(nil);

  try
    dsStock.DataSet := Self.reportRepository.GetLowStockDataset(aLimit);
    dsStock.UserName := 'dsStock';
    report.DataSets.Add(dsStock);

    report.LoadFromFile('stock.fr3');
    report.ShowReport();
  finally
    report.Free;
    dsStock.Free;
  end;
end;

procedure TReportController.ShowUsageReport(aStartDate, aEndDate: TDate);
var
  report: TfrxReport;
  dsMostBought, dsMostHoned: TfrxDBDataset;
begin
  report := TfrxReport.Create(nil);
  dsMostBought := TfrxDBDataset.Create(nil);
  dsMostHoned := TfrxDBDataset.Create(nil);

  try
    dsMostBought.DataSet := Self.reportRepository.GetMostBoughtDataset(aStartDate, aEndDate);
    dsMostHoned.DataSet := Self.reportRepository.GetMostHonedDataset(aStartDate, aEndDate);
    dsMostBought.UserName := 'dsMostBought';
    dsMostHoned.UserName := 'dsMostHoned';
    report.DataSets.Add(dsMostBought);
    report.DataSets.Add(dsMostHoned);

    report.LoadFromFile('usage.fr3');
    report.ShowReport();
  finally
    report.Free;
    dsMostHoned.Free;
    dsMostBought.Free;
  end;
end;

end.

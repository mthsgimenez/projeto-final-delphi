unit ReportController;

interface

uses frxClass, frxDBSet, ReportRepositoryInterface;

type TReportController = class
  private
    reportRepository: IReportRepository;
  public
    procedure ShowPurchaseReport;

    constructor Create(aReportRepository: IReportRepository);
end;

implementation

{ TReportController }

constructor TReportController.Create(aReportRepository: IReportRepository);
begin
  Self.reportRepository := aReportRepository;
end;

procedure TReportController.ShowPurchaseReport;
var
  report: TfrxReport;
  dsExpensesPerMonth: TfrxDBDataset;
begin
  report := TfrxReport.Create(nil);
  dsExpensesPerMonth := TfrxDBDataset.Create(nil);

  try
    dsExpensesPerMonth.DataSet := Self.reportRepository.GetPurchaseReportDataset(3);
    dsExpensesPerMonth.UserName := 'expensesPerMonth';
    report.DataSets.Add(dsExpensesPerMonth);

    report.LoadFromFile('teste.fr3');
    report.ShowReport();
  finally
    report.Free;
    dsExpensesPerMonth.Free;
  end;
end;

end.

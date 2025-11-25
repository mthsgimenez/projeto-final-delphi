unit ReportsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Dependencies, ReportController,
  Vcl.ComCtrls, MessageHelper;

type
  TformReports = class(TForm)
    pcontrolReports: TPageControl;
    tabSpent: TTabSheet;
    buttonShowSpent: TButton;
    editSpentStartDate: TEdit;
    editSpentEndDate: TEdit;
    labelStartDate: TLabel;
    labelEndDate: TLabel;
    tabUsage: TTabSheet;
    Label1: TLabel;
    editUsageStartDate: TEdit;
    Label2: TLabel;
    editUsageEndDate: TEdit;
    buttonShowUsage: TButton;
    tabStock: TTabSheet;
    Label3: TLabel;
    editStockLimit: TEdit;
    buttonShowStock: TButton;
    procedure buttonShowSpentClick(Sender: TObject);
    procedure buttonShowUsageClick(Sender: TObject);
    procedure buttonShowStockClick(Sender: TObject);
  private
    reportController: TReportController;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  formReports: TformReports;

implementation

{$R *.dfm}

{ TformReports }

procedure TformReports.buttonShowSpentClick(Sender: TObject);
var
  startDate, endDate: TDate;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;

  fs.DateSeparator := '/';
  fs.ShortDateFormat := 'dd/MM/yyyy';

  if Self.editSpentStartDate.Text = '' then
    startDate := StrToDate('01/01/2000', fs)
  else
    try
      startDate := StrToDate(Self.editSpentStartDate.Text, fs);
    except
      TMessageHelper.GetInstance.Error('Data de início inválida');
      Exit;
    end;

  if Self.editSpentEndDate.Text = '' then
    endDate := Date
  else
    try
      endDate := StrToDate(Self.editSpentEndDate.Text, fs);
    except
      TMessageHelper.GetInstance.Error('Data de fim inválida');
      Exit;
    end;

  Self.reportController.ShowSpentReport(startDate, endDate);
end;

procedure TformReports.buttonShowStockClick(Sender: TObject);
var
  limit: Integer;
begin
  try
    limit := StrToInt(Self.editStockLimit.Text);
  except
    TMessageHelper.GetInstance.Error('Digite um valor');
    Exit;
  end;

  Self.reportController.ShowStockReport(limit);
end;

procedure TformReports.buttonShowUsageClick(Sender: TObject);
var
  startDate, endDate: TDate;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;

  fs.DateSeparator := '/';
  fs.ShortDateFormat := 'dd/MM/yyyy';

  if Self.editSpentStartDate.Text = '' then
    startDate := StrToDate('01/01/2000', fs)
  else
    try
      startDate := StrToDate(Self.editSpentStartDate.Text, fs);
    except
      TMessageHelper.GetInstance.Error('Data de início inválida');
      Exit;
    end;

  if Self.editSpentEndDate.Text = '' then
    endDate := Date
  else
    try
      endDate := StrToDate(Self.editSpentEndDate.Text, fs);
    except
      TMessageHelper.GetInstance.Error('Data de fim inválida');
      Exit;
    end;

  Self.reportController.ShowUsageReport(startDate, endDate);
end;

constructor TformReports.Create(AOwner: TComponent);
begin
  inherited;
  Self.reportController := TDependencies.GetInstance.GetReportController;
end;

end.

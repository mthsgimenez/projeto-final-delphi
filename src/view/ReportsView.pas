unit ReportsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Dependencies, ReportController;

type
  TformReports = class(TForm)
    panelReports: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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

procedure TformReports.Button1Click(Sender: TObject);
begin
  Self.reportController.ShowPurchaseReport;
end;

constructor TformReports.Create(AOwner: TComponent);
begin
  inherited;
  Self.reportController := TDependencies.GetInstance.GetReportController;
end;

end.

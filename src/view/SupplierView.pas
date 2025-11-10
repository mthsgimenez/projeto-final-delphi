unit SupplierView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Skia, Vcl.Mask;

type
  TformSupplier = class(TForm)
    pcontrolSupplier: TPageControl;
    tabView: TTabSheet;
    gridSuppliers: TStringGrid;
    buttonCreate: TButton;
    buttonDelete: TButton;
    buttonEdit: TButton;
    tabCreate: TTabSheet;
    Label1: TLabel;
    panelCNPJ: TPanel;
    imgSearch: TImage;
    panelTradeName: TPanel;
    MaskEdit1: TMaskEdit;
    procedure gridSuppliersDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  formSupplier: TformSupplier;

implementation

{$R *.dfm}

procedure TformSupplier.gridSuppliersDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
var
  grid: TStringGrid;
begin
  grid := TStringGrid(Sender);

  if ARow = 0 then begin
    grid.Canvas.Brush.Color := RGB($41, $69, $E1);
    grid.Canvas.Font.Color := RGB($FF, $FF, $FF);
  end;

  grid.Canvas.FillRect(Rect);
  grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, grid.Cells[ACol, ARow]);
end;

end.

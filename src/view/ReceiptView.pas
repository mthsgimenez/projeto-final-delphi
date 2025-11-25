unit ReceiptView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Generics.Collections,
  Dependencies, PurchaseOrderModel, ServiceOrderModel, PurchaseOrderController, ServiceOrderController,
  Vcl.Grids, Vcl.StdCtrls, MessageHelper, StorageController, StorageModel;

type TFilterType = (ALL_TYPES, PURCHASE_ORDER, SERVICE_ORDER);

type
  TformReceipt = class(TForm)
    panelReceipt: TPanel;
    gridOrders: TStringGrid;
    buttonClearFilter: TButton;
    comboType: TComboBox;
    labelType: TLabel;
    buttonFilter: TButton;
    buttonCloseOrder: TButton;
    buttonDetails: TButton;
    panelClosePurchaseOrder: TPanel;
    Label1: TLabel;
    buttonClosePurchaseOrder: TButton;
    buttonCancel: TButton;
    listStorages: TListBox;
    procedure gridOrdersSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonCloseOrderClick(Sender: TObject);
    procedure buttonCancelClick(Sender: TObject);
    procedure buttonClosePurchaseOrderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure buttonFilterClick(Sender: TObject);
    procedure buttonClearFilterClick(Sender: TObject);
    procedure gridOrdersDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
  private
    purchaseOrderController: TPurchaseOrderController;
    serviceOrderController: TServiceOrderController;
    storageController: TStorageController;

    purchaseOrders: TObjectList<TPurchaseOrder>;
    serviceOrders: TObjectList<TServiceOrder>;
    storages: TObjectList<TStorage>;

    selectedOrder: TObject;

    procedure UpdateOrdersGrid(aFilterType: TFilterType);
    procedure UpdateStoragesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formReceipt: TformReceipt;

implementation

{$R *.dfm}

{ TformReceipt }

procedure TformReceipt.buttonCancelClick(Sender: TObject);
begin
  Self.panelClosePurchaseOrder.Visible := False;
end;

procedure TformReceipt.buttonClearFilterClick(Sender: TObject);
begin
  Self.comboType.ItemIndex := -1;
end;

procedure TformReceipt.buttonCloseOrderClick(Sender: TObject);
var
  serviceOrder, updatedServiceOrder: TServiceOrder;
begin
  if not Assigned(Self.selectedOrder) then begin
    TMessageHelper.GetInstance.Error('Nenhum pedido selecionado');
    Exit;
  end;

  if Self.selectedOrder is TPurchaseOrder then begin
    Self.panelClosePurchaseOrder.Visible := True;
    Self.UpdateStoragesList;
    Exit;
  end;

  if Self.selectedOrder is TServiceOrder then begin
    serviceOrder := TServiceOrder(Self.selectedOrder);
    updatedServiceOrder := Self.serviceOrderController.CloseOrder(serviceOrder.id);
    if Assigned(updatedServiceOrder) then begin
      TMessageHelper.GetInstance.Info('Ordem de serviço baixada. As ferramentas estão disponíveis no estoque');
      Self.serviceOrders.Remove(serviceOrder);
      updatedServiceOrder.Free;
      Self.UpdateOrdersGrid(ALL_TYPES);
    end;
  end;
end;

procedure TformReceipt.buttonClosePurchaseOrderClick(Sender: TObject);
var
  selectedStorage: TStorage;
  purchaseOrder, updatedPurchaseOrder: TPurchaseOrder;
  index: Integer;
begin
  index := Self.listStorages.ItemIndex;

  if index = -1 then begin
    TMessageHelper.GetInstance.Error('Escolha um estoque para destinar as ferramentas');
    Exit;
  end;

  selectedStorage := TStorage(Self.listStorages.Items.Objects[index]);

  purchaseOrder := TPurchaseOrder(Self.selectedOrder);
  updatedPurchaseOrder := Self.purchaseOrderController.CloseOrder(purchaseOrder.id, selectedStorage.id);
  if Assigned(updatedPurchaseOrder) then begin
    TMessageHelper.GetInstance.Info('Ordem de compra baixada. As ferramentas foram destinadas ao estoque ' + selectedStorage.name);

    Self.purchaseOrders.Remove(purchaseOrder);
    updatedPurchaseOrder.Free;

    Self.panelClosePurchaseOrder.Visible := False;
    Self.UpdateOrdersGrid(ALL_TYPES);
  end;
end;

procedure TformReceipt.buttonFilterClick(Sender: TObject);
var
  fType: TFilterType;
begin
  case Self.comboType.ItemIndex of
    0: fType := PURCHASE_ORDER;
    1: fType := SERVICE_ORDER;
    else FType := ALL_TYPES;
  end;

  Self.UpdateOrdersGrid(fType);
end;

constructor TformReceipt.Create(AOwner: TComponent);
begin
  inherited;
  Self.purchaseOrderController := TDependencies.GetInstance.GetPurchaseOrderController;
  Self.serviceOrderController := TDependencies.GetInstance.GetServiceOrderController;
  Self.storageController := TDependencies.GetInstance.GetStorageController;

  Self.purchaseOrders := Self.purchaseOrderController.GetOpen;
  Self.serviceOrders := Self.serviceOrderController.GetOpen;
  Self.storages := Self.storageController.GetStorages;
end;

destructor TformReceipt.Destroy;
begin
  if Assigned(Self.purchaseOrders) then
    Self.purchaseOrders.Free;
  if Assigned(Self.serviceOrders) then
    Self.serviceOrders.Free;
  if Assigned(Self.storages) then
    Self.storages.Free;
  inherited;
end;

procedure TformReceipt.FormCreate(Sender: TObject);
begin
  Self.UpdateOrdersGrid(ALL_TYPES);
end;

procedure TformReceipt.gridOrdersDrawCell(Sender: TObject; ACol, ARow: LongInt;
  Rect: TRect; State: TGridDrawState);
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

procedure TformReceipt.gridOrdersSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedOrder := Self.gridOrders.Objects[0, ARow];
end;

procedure TformReceipt.UpdateOrdersGrid(aFilterType: TFilterType);
var
  pOrder: TPurchaseOrder;
  sOrder: TServiceOrder;
  i: Integer;
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.CurrencyString := 'R$';
  fs.CurrencyFormat := 0;
  fs.DecimalSeparator := ',';
  fs.ThousandSeparator := '.';

  with Self.gridOrders do begin
    ColCount := 6;
    RowCount := 1;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Tipo';
    Cells[2, 0] := 'Fornecedor';
    Cells[3, 0] := 'Data de emissão';
    Cells[4, 0] := 'Qtd. Itens';
    Cells[5, 0] := 'Valor';
    ColWidths[0] := 50;
    ColWidths[1] := 100;
    ColWidths[2] := 190;
    ColWidths[3] := 130;
    ColWidths[4] := 80;
    ColWidths[5] := 130;

    if aFilterType <> SERVICE_ORDER then
    if Assigned(Self.purchaseOrders) then
      for pOrder in Self.purchaseOrders do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := pOrder;
        Cells[0, i] := IntToStr(pOrder.id);
        Cells[1, i] := 'Compra';
        Cells[2, i] := pOrder.supplier.tradeName;
        Cells[3, i] := FormatDateTime('hh:nn  dd/mm/yyyy', pOrder.issuedAt);
        Cells[4, i] := IntToStr(pOrder.GetTotalQuantity);
        Cells[5, i] := Format('%m', [pOrder.GetTotalPrice], fs);
      end;

    if aFilterType <> PURCHASE_ORDER then
    if Assigned(Self.serviceOrders) then
      for sOrder in Self.serviceOrders do begin

        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := sOrder;
        Cells[0, i] := IntToStr(sOrder.id);
        Cells[1, i] := 'Serviço';
        Cells[2, i] := sOrder.supplier.tradeName;
        Cells[3, i] := FormatDateTime('hh:nn  dd/mm/yyyy', sOrder.issuedAt);
        Cells[4, i] := IntToStr(sOrder.items.Count);
        Cells[5, i] := Format('%m', [sOrder.price], fs);
      end;
  end;
end;

procedure TformReceipt.UpdateStoragesList;
var
  storage: TStorage;
begin
  Self.listStorages.Clear;

  if Assigned(Self.storages) then begin
    for storage in Self.storages do begin
      Self.listStorages.AddItem(storage.name, storage);
    end;
  end;
end;

end.

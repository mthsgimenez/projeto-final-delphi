unit OrderView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Grids, Dependencies, MessageHelper,
  PurchaseOrderModel, PurchaseOrderDTO, PurchaseOrderController,
  ToolTypeModel, ToolTypeController,
  SupplierModel, SupplierController, Vcl.ExtCtrls;

type TFilterStatus = (ALL_STATUSES, OPEN, CLOSED, CANCELLED);
type TFilterType = (ALL_TYPES, PURCHASE_ORDER, SERVICE_ORDER);
type TPicking = (SUPPLIER, TOOLTYPE);

type
  TformOrder = class(TForm)
    pcontrolOrders: TPageControl;
    tabList: TTabSheet;
    tabCreatePurchase: TTabSheet;
    gridOrders: TStringGrid;
    comboType: TComboBox;
    comboStatus: TComboBox;
    buttonFilter: TButton;
    buttonClear: TButton;
    labelType: TLabel;
    labelStatus: TLabel;
    buttonCreatePurchase: TButton;
    buttonCreateService: TButton;
    buttonCancel: TButton;
    buttonBack: TButton;
    buttonSavePurchase: TButton;
    listPreview: TListBox;
    editSupplier: TEdit;
    buttonPickSupplier: TButton;
    editToolType: TEdit;
    buttonPickToolType: TButton;
    editQuantity: TEdit;
    buttonAddToolType: TButton;
    labelPrice: TLabel;
    panelPicker: TPanel;
    gridPick: TStringGrid;
    buttonCancelPick: TButton;
    buttonPick: TButton;
    procedure tabListShow(Sender: TObject);
    procedure buttonFilterClick(Sender: TObject);
    procedure buttonClearClick(Sender: TObject);
    procedure gridOrdersSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonCancelClick(Sender: TObject);
    procedure buttonCreatePurchaseClick(Sender: TObject);
    procedure buttonBackClick(Sender: TObject);
    procedure buttonPickToolTypeClick(Sender: TObject);
    procedure buttonCancelPickClick(Sender: TObject);
    procedure tabCreatePurchaseShow(Sender: TObject);
    procedure gridPickSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonPickClick(Sender: TObject);
    procedure buttonPickSupplierClick(Sender: TObject);
    procedure buttonAddToolTypeClick(Sender: TObject);
    procedure buttonSavePurchaseClick(Sender: TObject);
  private
    selectedOrder: TObject;

    purchaseOrderController: TPurchaseOrderController;
    toolTypeController: TToolTypeController;
    supplierController: TSupplierController;

    purchaseOrders: TObjectList<TPurchaseOrder>;
    suppliers: TObjectList<TSupplier>;
    toolTypes: TObjectList<TToolType>;

    currentPicking: TPicking;
    selectedSupplier: TSupplier;
    selectedToolType: TToolType;

    chosenSupplier: TSupplier;
    chosenToolType: TToolType;

    procedure UpdateSuppliersGrid();
    procedure UpdateToolTypesGrid();
    procedure UpdateOrdersGrid(aFilterType: TFilterType;
      aFilterStatus: TFilterStatus);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formOrder: TformOrder;

implementation

{$R *.dfm}

{ TformOrder }

procedure TformOrder.buttonAddToolTypeClick(Sender: TObject);
var
  quantity: Integer;
  item: TPurchaseOrderItem;
  i: Integer;
  sum: Currency;
begin
  try
    quantity := StrToInt(Self.editQuantity.Text);
  except
    TMessageHelper.GetInstance.Error('Quantidade inválida');
    Exit;
  end;

  if quantity < 1 then begin
    TMessageHelper.GetInstance.Error('Quantidade inválida');
    Exit;
  end;

  if not Assigned(Self.chosenToolType) then begin
    TMessageHelper.GetInstance.Error('Escolha uma ferramenta primeiro');
    Exit;
  end;

  item := TPurchaseOrderItem.Create;
  item.quantity := quantity;
  item.model := Self.chosenToolType;

  Self.listPreview.AddItem(Format('%s %s x%d', [item.model.code, FormatCurr('R$#,0.00', item.model.price), item.quantity]), item);

  sum := 0;
  for i := 0 to Self.listPreview.Items.Count - 1 do begin
    item := TPurchaseOrderItem(Self.listPreview.Items.Objects[i]);

    sum := sum + item.model.price * item.quantity;
  end;

  Self.labelPrice.Caption := FormatCurr('Total: R$#,0.00', sum);
end;

procedure TformOrder.buttonBackClick(Sender: TObject);
begin
  Self.pcontrolOrders.ActivePageIndex := 0;
end;

procedure TformOrder.buttonCancelClick(Sender: TObject);
var
  purchaseOrder: TPurchaseOrder;
  updatedPurchaseOrder: TPurchaseOrder;
begin
  if not Assigned(Self.selectedOrder) then begin
    TMessageHelper.GetInstance.Error('Nenhum pedido selecionado');
    Exit;
  end;

  if not TMessageHelper.GetInstance.Confirmation(
    'Tem certeza que deseja cancelar o pedido selecionado? Essa ação não pode ser desfeita'
  ) then Exit;

  if Self.selectedOrder is TPurchaseOrder then begin
    purchaseOrder := TPurchaseOrder(Self.selectedOrder);
    updatedPurchaseOrder := Self.purchaseOrderController.CancelOrder(purchaseOrder.id);
    if Assigned(updatedPurchaseOrder) then begin
      Self.purchaseOrders.Remove(purchaseOrder);
      Self.purchaseOrders.Add(updatedPurchaseOrder);
      Self.UpdateOrdersGrid(PURCHASE_ORDER, CANCELLED);
    end;
  end;
end;

procedure TformOrder.buttonCancelPickClick(Sender: TObject);
begin
  Self.panelPicker.Visible := False;
  Self.selectedSupplier := nil;
  Self.selectedToolType := nil;
end;

procedure TformOrder.buttonClearClick(Sender: TObject);
begin
  Self.comboType.ItemIndex := -1;
  Self.comboStatus.ItemIndex := -1;
end;

procedure TformOrder.buttonCreatePurchaseClick(Sender: TObject);
begin
  Self.pcontrolOrders.ActivePageIndex := 1;
end;

procedure TformOrder.buttonFilterClick(Sender: TObject);
var
  statusFilter: TFilterStatus;
  typeFilter: TFilterType;
begin
  case Self.comboStatus.ItemIndex of
    0: statusFilter := OPEN;
    1: statusFilter := CLOSED;
    2: statusFilter := CANCELLED;
    else statusFilter := ALL_STATUSES;
  end;

  case Self.comboType.ItemIndex of
    0: typeFilter := PURCHASE_ORDER;
    1: typeFilter := SERVICE_ORDER;
    else typeFilter := ALL_TYPES;
  end;

  Self.UpdateOrdersGrid(typeFilter, statusFilter);
end;

procedure TformOrder.buttonPickClick(Sender: TObject);
begin
  case Self.currentPicking of
    SUPPLIER: begin
      if not Assigned(Self.selectedSupplier) then begin
        TMessageHelper.GetInstance.Error('Nenhum fornecedor selecionado');
        Exit;
      end;

      Self.chosenSupplier := Self.selectedSupplier;
      Self.editSupplier.Text := Self.chosenSupplier.tradeName;
    end;
    TOOLTYPE: begin
      if not Assigned(Self.selectedToolType) then begin
        TMessageHelper.GetInstance.Error('Nenhuma ferramenta selecionada');
        Exit;
      end;

      Self.chosenToolType := Self.selectedToolType;
      Self.editToolType.Text := Self.chosenToolType.code;
    end;
  end;

  Self.selectedSupplier := nil;
  Self.selectedToolType := nil;

  Self.panelPicker.Visible := False;
end;

procedure TformOrder.buttonPickSupplierClick(Sender: TObject);
begin
  Self.currentPicking := SUPPLIER;
  Self.UpdateSuppliersGrid;
  Self.panelPicker.Visible := True;
end;

procedure TformOrder.buttonPickToolTypeClick(Sender: TObject);
begin
  Self.currentPicking := TOOLTYPE;
  Self.UpdateToolTypesGrid;
  Self.panelPicker.Visible := True;
end;

procedure TformOrder.buttonSavePurchaseClick(Sender: TObject);
var
  purchaseOrderDTO: TPurchaseOrderDTO;
  newOrder: TPurchaseOrder;
  item: TPurchaseOrderItem;
  i: Integer;
begin
  if not Assigned(Self.chosenSupplier) then begin
    TMessageHelper.GetInstance.Error('Nenhum fornecedor escolhido');
    Exit;
  end;

  if Self.listPreview.Items.Count < 1 then begin
    TMessageHelper.GetInstance.Error('Não é possível emitir uma ordem de compra vazia');
    Exit;
  end;

  purchaseOrderDTO := TPurchaseOrderDTO.Create;
  purchaseOrderDTO.supplier := Self.chosenSupplier;
  for i := 0 to Self.listPreview.Items.Count - 1 do begin
    item := TPurchaseOrderItem(Self.listPreview.Items.Objects[i]);

    purchaseOrderDTO.items.Add(item);
  end;

  newOrder := Self.purchaseOrderController.CreateOrder(purchaseOrderDTO);
  if not Assigned(newOrder) then begin
    TMessageHelper.GetInstance.Error('Não foi possível emitir a ordem de compra');
    FreeAndNil(purchaseOrderDTO);
    Exit;
  end;

  Self.purchaseOrders.Add(newOrder);
  Self.chosenSupplier := nil;
  Self.chosenToolType := nil;

  Self.pcontrolOrders.ActivePageIndex := 0;
end;

constructor TformOrder.Create(AOwner: TComponent);
begin
  inherited;
  Self.purchaseOrderController := TDependencies.GetInstance.GetPurchaseOrderController;
  Self.toolTypeController := TDependencies.GetInstance.GetToolTypeController;
  Self.supplierController := TDependencies.GetInstance.GetSupplierController;

  Self.purchaseOrders := Self.purchaseOrderController.GetOrders;
end;

destructor TformOrder.Destroy;
begin
  if Assigned(Self.purchaseOrders) then
    FreeAndNil(Self.purchaseOrders);
  if Assigned(Self.toolTypes) then
    FreeAndNil(Self.toolTypes);
  if Assigned(Self.suppliers) then
    FreeAndNil(Self.suppliers);
  inherited;
end;

procedure TformOrder.gridOrdersSelectCell(Sender: TObject; ACol, ARow: LongInt;
  var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedOrder := Self.gridOrders.Objects[0, ARow];
end;

procedure TformOrder.gridPickSelectCell(Sender: TObject; ACol, ARow: LongInt;
  var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  case Self.currentPicking of
    SUPPLIER: Self.selectedSupplier := TSupplier(Self.gridPick.Objects[0, ARow]);
    TOOLTYPE: Self.selectedToolType := TToolType(Self.gridPick.Objects[0, ARow]);
  end;
end;

procedure TformOrder.tabCreatePurchaseShow(Sender: TObject);
begin
  if not Assigned(Self.toolTypes) then
    Self.toolTypes := Self.toolTypeController.GetAll;

  if not Assigned(Self.suppliers) then
    Self.suppliers := Self.supplierController.GetSuppliers;
end;

procedure TformOrder.tabListShow(Sender: TObject);
begin
  Self.UpdateOrdersGrid(ALL_TYPES, ALL_STATUSES);
end;

procedure TformOrder.UpdateOrdersGrid(aFilterType: TFilterType;
  aFilterStatus: TFilterStatus);
var
  order: TPurchaseOrder;
  i: Integer;
begin
  with Self.gridOrders do begin
    RowCount := 1;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Fornecedor';
    Cells[2, 0] := 'Data de emissão';
    Cells[3, 0] := 'Data de atualização';
    Cells[4, 0] := 'Status';
    ColWidths[0] := 80;
    ColWidths[1] := 190;
    ColWidths[2] := 140;
    ColWidths[3] := 140;
    ColWidths[4] := 140;

    if aFilterType <> SERVICE_ORDER then
    if Assigned(Self.purchaseOrders) then
      for order in Self.purchaseOrders do begin
        if aFilterStatus <> ALL_STATUSES then begin
          case aFilterStatus of
            OPEN: if order.status <> PurchaseOrderModel.OPEN then continue;
            CLOSED: if order.status <> PurchaseOrderModel.CLOSED then continue;
            CANCELLED: if order.status <> PurchaseOrderModel.CANCELLED then continue;
          end;
        end;

        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := order;
        Cells[0, i] := IntToStr(order.id);
        Cells[1, i] := order.supplier.tradeName;
        Cells[2, i] := FormatDateTime('hh:nn dd\mm\yyyy', order.issuedAt);
        if order.statusUpdatedAt <> 0.0 then
          Cells[3, i] := FormatDateTime('hh:nn dd\mm\yyyy', order.statusUpdatedAt);
        Cells[4, i] := PurchaseOrderModel.StatusToViewString(order.status);
      end;
  end;
end;

procedure TformOrder.UpdateSuppliersGrid;
var
  supplier: TSupplier;
  i: Integer;
begin
  with Self.gridPick do begin
    RowCount := 1;
    ColCount := 3;
    Cells[0, 0] := 'Nome fantasia';
    Cells[1, 0] := 'Razão social';
    Cells[2, 0] := 'CNPJ';
    ColWidths[0] := 200;
    ColWidths[1] := 200;
    ColWidths[2] := 200;

    if Assigned(Self.suppliers) then
      for supplier in Self.suppliers do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := supplier;
        Cells[0, i] := supplier.tradeName;
        Cells[1, i] := supplier.legalName;
        Cells[2, i] := supplier.CNPJ.getCNPJ;
      end;
  end;
end;

procedure TformOrder.UpdateToolTypesGrid;
var
  toolType: TToolType;
  i: Integer;
begin
  with Self.gridPick do begin
    RowCount := 1;
    ColCount := 4;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Descrição';
    Cells[2, 0] := 'Família';
    Cells[3, 0] := 'Preço';
    ColWidths[0] := 150;
    ColWidths[1] := 150;
    ColWidths[2] := 150;
    ColWidths[3] := 100;

    if Assigned(Self.toolTypes) then
      for toolType in Self.toolTypes do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := toolType;
        Cells[0, i] := toolType.code;
        Cells[1, i] := toolType.description;
        Cells[2, i] := toolType.family;
        Cells[3, i] := FormatCurr('R$#,0.00', toolType.price);
      end;
  end;
end;

end.

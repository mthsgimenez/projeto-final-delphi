unit OrderView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.Generics.Collections, Vcl.ExtCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Grids, Dependencies, MessageHelper,
  PurchaseOrderModel, PurchaseOrderDTO, PurchaseOrderController,
  ServiceOrderModel, ServiceOrderDTO, ServiceOrderController,
  ToolModel, ToolController,
  ToolTypeModel, ToolTypeController,
  SupplierModel, SupplierController;

type TFilterStatus = (ALL_STATUSES, OPEN, CLOSED, CANCELLED);
type TFilterType = (ALL_TYPES, PURCHASE_ORDER, SERVICE_ORDER);
type TPicking = (SUPPLIER, TOOLTYPE, TOOL);

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
    tabCreateService: TTabSheet;
    editSupplier2: TEdit;
    labelSupplier2: TLabel;
    buttonPickSupplier2: TButton;
    listPreviewService: TListBox;
    editTool: TEdit;
    labelTool: TLabel;
    buttonPickTool: TButton;
    editPrice: TEdit;
    labelPrice2: TLabel;
    buttonAddTool: TButton;
    labelDisplayPrice: TLabel;
    buttonBack2: TButton;
    buttonSaveService: TButton;
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
    procedure tabCreatePurchaseHide(Sender: TObject);
    procedure buttonPickSupplier2Click(Sender: TObject);
    procedure buttonPickToolClick(Sender: TObject);
    procedure tabCreateServiceShow(Sender: TObject);
    procedure buttonBack2Click(Sender: TObject);
    procedure buttonCreateServiceClick(Sender: TObject);
    procedure editPriceExit(Sender: TObject);
    procedure buttonAddToolClick(Sender: TObject);
    procedure tabCreateServiceHide(Sender: TObject);
    procedure buttonSaveServiceClick(Sender: TObject);
  private
    selectedOrder: TObject;

    purchaseOrderController: TPurchaseOrderController;
    serviceOrderController: TServiceOrderController;
    toolTypeController: TToolTypeController;
    toolController: TToolController;
    supplierController: TSupplierController;

    purchaseOrders: TObjectList<TPurchaseOrder>;
    serviceOrders: TObjectList<TServiceOrder>;
    suppliers: TObjectList<TSupplier>;
    toolTypes: TObjectList<TToolType>;
    tools: TObjectList<TTool>;

    currentPicking: TPicking;

    selectedSupplier: TSupplier;
    chosenSupplier: TSupplier;

    selectedToolType: TToolType;
    chosenToolType: TToolType;

    selectedTool: TTool;
    chosenTool: TTool;

    orderPrice: Currency;
    purchaseOrderDTO: TPurchaseOrderDTO;
    serviceOrderDTO: TServiceOrderDTO;

    procedure ClearPurchaseCreate;
    procedure ClearServiceCreate;
    procedure UpdateSuppliersGrid();
    procedure UpdateToolTypesGrid();
    procedure UpdateToolGrid();
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

procedure TformOrder.buttonAddToolClick(Sender: TObject);
var
  price: Currency;
  unformattedPrice: String;
  fs: TFormatSettings;
begin
  if not Assigned(Self.chosenTool) then begin
    TMessageHelper.GetInstance.Error('Selecione uma ferramenta');
    Exit;
  end;

  unformattedPrice := StringReplace(Self.editPrice.Text, 'R$', '', [rfReplaceAll]);
  unformattedPrice := Trim(unformattedPrice);

  fs := TFormatSettings.Create;
  fs.CurrencyString := 'R$';
  fs.CurrencyFormat := 0;
  fs.DecimalSeparator := ',';
  fs.ThousandSeparator := '.';

  if not TryStrToCurr(unformattedPrice, price, fs) then begin
    TMessageHelper.GetInstance.Error('Preço inválido');
    Exit;
  end;

  Self.serviceOrderDTO.AddItem(Self.chosenTool.id);
  Self.orderPrice := Self.orderPrice + price;

  Self.listPreviewService.AddItem(Format('%s %s', [Self.chosenTool.code, Self.editPrice.Text]), nil);

  Self.editPrice.Clear;
  Self.editTool.Clear;
  Self.tools.Remove(Self.chosenTool);
  Self.chosenTool := nil;

  Self.labelDisplayPrice.Caption := Format('Total: %m', [Self.orderPrice], fs);
end;

procedure TformOrder.buttonAddToolTypeClick(Sender: TObject);
var
  quantity: Integer;
  item: TPurchaseOrderItemDTO;
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

  item.quantity := quantity;
  item.modelId := Self.chosenToolType.id;
  Self.purchaseOrderDTO.AddItem(item);

  Self.listPreview.AddItem(Format('%s %s x%d', [Self.chosenToolType.code, FormatCurr('R$#,0.00', Self.chosenToolType.price), item.quantity]), nil);

  Self.orderPrice := Self.orderPrice + Self.chosenToolType.price * item.quantity;
  Self.labelPrice.Caption := FormatCurr('Total: R$#,0.00', Self.orderPrice);

  Self.editQuantity.Clear;
end;

procedure TformOrder.buttonBack2Click(Sender: TObject);
begin
  Self.pcontrolOrders.ActivePageIndex := 0;
end;

procedure TformOrder.buttonBackClick(Sender: TObject);
begin
  Self.pcontrolOrders.ActivePageIndex := 0;
end;

procedure TformOrder.buttonCancelClick(Sender: TObject);
var
  purchaseOrder, updatedPurchaseOrder: TPurchaseOrder;
  serviceOrder, updatedServiceOrder: TServiceOrder;
begin
  if not Assigned(Self.selectedOrder) then begin
    TMessageHelper.GetInstance.Error('Nenhum pedido selecionado');
    Exit;
  end;

  if Self.selectedOrder is TPurchaseOrder then begin
    purchaseOrder := TPurchaseOrder(Self.selectedOrder);

    if purchaseOrder.status <> PurchaseOrderModel.OPEN then begin
      TMessageHelper.GetInstance.Error('Não é possível cancelar um pedido que não está aberto');
      Exit;
    end;
  end else if Self.selectedOrder is TServiceOrder then begin
    serviceOrder := TServiceOrder(Self.selectedOrder);

    if ServiceOrder.status <> ServiceOrderModel.OPEN then begin
      TMessageHelper.GetInstance.Error('Não é possível cancelar um pedido que não está aberto');
      Exit;
    end;
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
  end else if Self.selectedOrder is TServiceOrder then begin
    serviceOrder := TServiceOrder(Self.selectedOrder);
    updatedServiceOrder := Self.serviceOrderController.CancelOrder(serviceOrder.id);
    if Assigned(updatedServiceOrder) then begin
      Self.serviceOrders.Remove(serviceOrder);
      Self.serviceOrders.Add(updatedServiceOrder);
      Self.UpdateOrdersGrid(SERVICE_ORDER, CANCELLED);
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

procedure TformOrder.buttonCreateServiceClick(Sender: TObject);
begin
  Self.pcontrolOrders.ActivePageIndex := 2;
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
      Self.editSupplier2.Text := Self.chosenSupplier.tradeName;
    end;
    TOOLTYPE: begin
      if not Assigned(Self.selectedToolType) then begin
        TMessageHelper.GetInstance.Error('Nenhuma ferramenta selecionada');
        Exit;
      end;

      Self.chosenToolType := Self.selectedToolType;
      Self.editToolType.Text := Self.chosenToolType.code;
    end;
    TOOL: begin
      if not Assigned(Self.selectedTool) then begin
        TMessageHelper.GetInstance.Error('Nenhuma ferramenta selecionada');
        Exit;
      end;

      Self.chosenTool := Self.selectedTool;
      Self.editTool.Text := Self.chosenTool.code;
    end;
  end;

  Self.selectedSupplier := nil;
  Self.selectedToolType := nil;
  Self.selectedTool := nil;

  Self.panelPicker.Visible := False;
end;

procedure TformOrder.buttonPickSupplier2Click(Sender: TObject);
begin
  Self.currentPicking := SUPPLIER;
  Self.UpdateSuppliersGrid;
  Self.panelPicker.Visible := True;
end;

procedure TformOrder.buttonPickSupplierClick(Sender: TObject);
begin
  Self.currentPicking := SUPPLIER;
  Self.UpdateSuppliersGrid;
  Self.panelPicker.Visible := True;
end;

procedure TformOrder.buttonPickToolClick(Sender: TObject);
begin
  Self.currentPicking := TOOL;
  Self.UpdateToolGrid;
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
  newOrder: TPurchaseOrder;
begin
  if not Assigned(Self.chosenSupplier) then begin
    TMessageHelper.GetInstance.Error('Nenhum fornecedor escolhido');
    Exit;
  end;

  if Self.listPreview.Items.Count < 1 then begin
    TMessageHelper.GetInstance.Error('Não é possível emitir uma ordem de compra vazia');
    Exit;
  end;

  purchaseOrderDTO.supplierId := Self.chosenSupplier.id;

  newOrder := Self.purchaseOrderController.CreateOrder(purchaseOrderDTO);
  if not Assigned(newOrder) then begin
    TMessageHelper.GetInstance.Error('Não foi possível emitir a ordem de compra');
    Exit;
  end;

  Self.purchaseOrders.Add(newOrder);

  Self.pcontrolOrders.ActivePageIndex := 0;
end;

procedure TformOrder.buttonSaveServiceClick(Sender: TObject);
var
  order: TServiceOrder;
begin
  if not Assigned(Self.chosenSupplier) then begin
    TMessageHelper.GetInstance.Error('Escolha um fornecedor');
    Exit;
  end;

  if Length(Self.serviceOrderDTO.items) < 1 then begin
    TMessageHelper.GetInstance.Error('Nenhuma ferramenta no pedido');
    Exit;
  end;

  Self.serviceOrderDTO.supplierId := Self.chosenSupplier.id;

  order := Self.serviceOrderController.CreateOrder(Self.serviceOrderDTO);

  if not Assigned(order) then begin
    TMessageHelper.GetInstance.Error('Não foi possível emitir a ordem de serviço');
    Exit;
  end;

  Self.serviceOrders.Add(order);

  Self.pcontrolOrders.ActivePageIndex := 0;
end;

procedure TformOrder.ClearPurchaseCreate;
begin
  Self.orderPrice := 0;
  Self.purchaseOrderDTO.supplierId := 0;
  SetLength(Self.purchaseOrderDTO.items, 0);

  Self.editSupplier.Clear;
  Self.editToolType.Clear;
  Self.editQuantity.Clear;
  Self.listPreview.Clear;
  Self.labelPrice.Caption := 'Total: R$0,00';

  Self.chosenSupplier := nil;
  Self.chosenToolType := nil;
  Self.selectedSupplier := nil;
  Self.selectedToolType := nil;
end;

procedure TformOrder.ClearServiceCreate;
begin
  Self.orderPrice := 0;
  Self.serviceOrderDTO.supplierId := 0;
  SetLength(Self.serviceOrderDTO.items, 0);

  Self.editSupplier2.Clear;
  Self.editTool.Clear;
  Self.editPrice.Clear;
  Self.listPreviewService.Clear;
  Self.labelDisplayPrice.Caption := 'Total: R$0,00';

  Self.chosenSupplier := nil;
  Self.chosenTool := nil;
  Self.selectedSupplier := nil;
  Self.selectedTool := nil;
end;

constructor TformOrder.Create(AOwner: TComponent);
begin
  inherited;
  Self.orderPrice := 0;
  SetLength(Self.purchaseOrderDTO.items, 0);
  Self.purchaseOrderDTO.supplierId := 0;

  Self.purchaseOrderController := TDependencies.GetInstance.GetPurchaseOrderController;
  Self.serviceOrderController := TDependencies.GetInstance.GetServiceOrderController;
  Self.toolTypeController := TDependencies.GetInstance.GetToolTypeController;
  Self.toolController := TDependencies.GetInstance.GetToolController;
  Self.supplierController := TDependencies.GetInstance.GetSupplierController;

  Self.purchaseOrders := Self.purchaseOrderController.GetOrders;
  Self.serviceOrders := Self.serviceOrderController.GetOrders;
end;

destructor TformOrder.Destroy;
begin
  if Assigned(Self.purchaseOrders) then
    FreeAndNil(Self.purchaseOrders);
  if Assigned(Self.serviceOrders) then
    FreeAndNil(Self.serviceOrders);
  if Assigned(Self.tools) then
    FreeAndNil(Self.tools);
  if Assigned(Self.toolTypes) then
    FreeAndNil(Self.toolTypes);
  if Assigned(Self.suppliers) then
    FreeAndNil(Self.suppliers);
  inherited;
end;

procedure TformOrder.editPriceExit(Sender: TObject);
var
  price: Currency;
  formattedPrice: String;
  fs: TFormatSettings;
begin
  try
    fs := TFormatSettings.Create;
    fs.CurrencyString := 'R$';
    fs.CurrencyFormat := 0;
    fs.DecimalSeparator := ',';
    fs.ThousandSeparator := '.';

    price := StrToCurr(Self.editPrice.Text, fs);

    formattedPrice := Format('%m', [price], fs);

    Self.editPrice.Text := formattedPrice;
  except
    on E: EConvertError do
    begin
      TMessageHelper.GetInstance.Error('Preço inválido');
    end;
  end;
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
    TOOL: Self.selectedTool := TTool(Self.gridPick.Objects[0, ARow]);
  end;
end;

procedure TformOrder.tabCreatePurchaseHide(Sender: TObject);
begin
  Self.ClearPurchaseCreate;
end;

procedure TformOrder.tabCreatePurchaseShow(Sender: TObject);
begin
  if not Assigned(Self.toolTypes) then
    Self.toolTypes := Self.toolTypeController.GetAll;

  if not Assigned(Self.suppliers) then
    Self.suppliers := Self.supplierController.GetSuppliers;
end;

procedure TformOrder.tabCreateServiceHide(Sender: TObject);
begin
  if Assigned(Self.tools) then
    FreeAndNil(Self.tools);
  Self.ClearServiceCreate;
end;

procedure TformOrder.tabCreateServiceShow(Sender: TObject);
begin
  if not Assigned(Self.tools) then
    Self.tools := Self.toolController.GetAvailableTools;

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
  pOrder: TPurchaseOrder;
  sOrder: TServiceOrder;
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
      for pOrder in Self.purchaseOrders do begin
        if aFilterStatus <> ALL_STATUSES then begin
          case aFilterStatus of
            OPEN: if pOrder.status <> PurchaseOrderModel.OPEN then continue;
            CLOSED: if pOrder.status <> PurchaseOrderModel.CLOSED then continue;
            CANCELLED: if pOrder.status <> PurchaseOrderModel.CANCELLED then continue;
          end;
        end;

        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := pOrder;
        Cells[0, i] := IntToStr(pOrder.id);
        Cells[1, i] := pOrder.supplier.tradeName;
        Cells[2, i] := FormatDateTime('hh:nn  dd/mm/yyyy', pOrder.issuedAt);
        if pOrder.statusUpdatedAt <> 0.0 then
          Cells[3, i] := FormatDateTime('hh:nn  dd/mm/yyyy', pOrder.statusUpdatedAt)
        else
          Cells[3, i] := '';
        Cells[4, i] := PurchaseOrderModel.StatusToViewString(pOrder.status);
      end;

    if aFilterType <> PURCHASE_ORDER then
    if Assigned(Self.serviceOrders) then
      for sOrder in Self.serviceOrders do begin
        if aFilterStatus <> ALL_STATUSES then begin
          case aFilterStatus of
            OPEN: if sOrder.status <> ServiceOrderModel.OPEN then continue;
            CLOSED: if sOrder.status <> ServiceOrderModel.CLOSED then continue;
            CANCELLED: if sOrder.status <> ServiceOrderModel.CANCELLED then continue;
          end;
        end;

        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := sOrder;
        Cells[0, i] := IntToStr(sOrder.id);
        Cells[1, i] := sOrder.supplier.tradeName;
        Cells[2, i] := FormatDateTime('hh:nn  dd/mm/yyyy', sOrder.issuedAt);
        if sOrder.statusUpdatedAt <> 0.0 then
          Cells[3, i] := FormatDateTime('hh:nn  dd/mm/yyyy', sOrder.statusUpdatedAt)
        else
          Cells[3, i] := '';
        Cells[4, i] := ServiceOrderModel.StatusToViewString(sOrder.status);
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

procedure TformOrder.UpdateToolGrid;
var
  tool: TTool;
  i: Integer;
begin
  with Self.gridPick do begin
    RowCount := 1;
    ColCount := 4;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Modelo';
    Cells[2, 0] := 'Estado';
    Cells[3, 0] := 'Num. Afiações';
    ColWidths[0] := 150;
    ColWidths[1] := 150;
    ColWidths[2] := 150;
    ColWidths[3] := 100;

    if Assigned(Self.tools) then
      for tool in Self.tools do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := tool;
        Cells[0, i] := tool.code;
        Cells[1, i] := tool.model.code;
        Cells[2, i] := ToolModel.StateToStringDisplay(tool.state);
        Cells[3, i] := IntToStr(tool.honingNum);
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

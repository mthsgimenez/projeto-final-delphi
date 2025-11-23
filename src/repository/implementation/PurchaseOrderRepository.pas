unit PurchaseOrderRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param, FireDAC.Comp.Client, FireDAC.DApt,
  PurchaseOrderRepositoryInterface, PurchaseOrderModel, ToolTypeModel, ToolTypeRepositoryInterface, SupplierRepositoryInterface;

type TPurchaseOrderRepository = class(TRepositoryBase, IPurchaseOrderRepository)
  private
    helper: TDBHelper;
    toolTypeRepository: IToolTypeRepository;
    supplierRepository: ISupplierRepository;

    procedure GetToolTypes(aPurchaseOrder: TPurchaseOrder);
  public
    function CreatePurchaseOrder(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function UpdateStatus(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function FindById(aPurchaseOrderId: Integer): TPurchaseOrder;
    function FindAll(): TObjectList<TPurchaseOrder>;

    constructor Create(aHelper: TDBHelper; aToolTypeRepository: IToolTypeRepository;
      aSupplierRepository: ISupplierRepository);
end;

implementation

{ TPurchaseOrderRepository }

constructor TPurchaseOrderRepository.Create(
  aHelper: TDBHelper;
  aToolTypeRepository: IToolTypeRepository;
  aSupplierRepository: ISupplierRepository);
begin
  inherited Create;
  Self.helper := aHelper;
  Self.toolTypeRepository := aToolTypeRepository;
  Self.supplierRepository := aSupplierRepository;
end;

function TPurchaseOrderRepository.FindAll: TObjectList<TPurchaseOrder>;
var
  ordersList: TObjectList<TPurchaseOrder>;
  order: TPurchaseOrder;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM purchase_orders ORDER BY issued_at DESC;';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      ordersList := TObjectList<TPurchaseOrder>.Create;

      while not Self.Query.Eof do begin
        order := TPurchaseOrder.Create;

        order.id := Self.Query.FieldByName('id').AsInteger;
        order.supplier := Self.supplierRepository.FindById(
          Self.Query.FieldByName('id_supplier').AsInteger
        );
        order.status := StringToStatus(
          Self.Query.FieldByName('status').AsString
        );
        order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
        if not Self.Query.FieldByName('status_updated_at').IsNull then
          order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;

        Self.GetToolTypes(order);

        ordersList.Add(order);

        Self.Query.Next;
      end;

      Result := ordersList;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPurchaseOrderRepository.FindById(
  aPurchaseOrderId: Integer): TPurchaseOrder;
var
  order: TPurchaseOrder;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM purchase_orders WHERE id = :purchaseOrderId';
  Self.Query.ParamByName('purchaseOrderId').AsInteger := aPurchaseOrderId;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      order := TPurchaseOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
      if not Self.Query.FieldByName('status_updated_at').IsNull then
        order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;

      Self.GetToolTypes(order);

      Result := order;
    end;
  finally
    Self.Query.Close;
  end;
end;

procedure TPurchaseOrderRepository.GetToolTypes(aPurchaseOrder: TPurchaseOrder);
var
  toolsQuery: TFDQuery;
  toolType: TToolType;
begin
  toolsQuery := TFDQuery.Create(Self.Query.Connection);
  toolsQuery.Connection := Self.Query.Connection;

  toolsQuery.SQL.Text :=
    'SELECT id_tool_model, tool_quantity FROM purchase_order_tools WHERE id_purchase_order = :purchaseOrderId';
  toolsQuery.ParamByName('purchaseOrderId').AsInteger := aPurchaseOrder.id;

  try
    toolsQuery.Open;

    if not toolsQuery.IsEmpty then begin
      while not toolsQuery.Eof do begin
        toolType := Self.toolTypeRepository.FindById(
          toolsQuery.FieldByName('id_tool_model').AsInteger
        );

        aPurchaseOrder.AddItemToOrder(
          toolType,
          toolsQuery.FieldByName('tool_quantity').AsInteger
        );

        toolsQuery.Next;
      end;
    end;
  finally
    toolsQuery.Close;
    toolsQuery.Free;
  end;
end;

function TPurchaseOrderRepository.CreatePurchaseOrder(
  aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
var
  item: TPurchaseOrderItem;
  order: TPurchaseOrder;
  insertStr: String;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'INSERT INTO purchase_orders (id_supplier) VALUES (:supplierId) RETURNING *';
  Self.Query.ParamByName('supplierId').AsInteger := aPurchaseOrder.supplier.id;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      order := TPurchaseOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;

      Self.Query.Close;

      insertStr := '';
      for item in aPurchaseOrder.items do begin
        if insertStr <> '' then
          insertStr := insertStr + ',';

        insertStr := insertStr + Format('(%d, %d, %d)', [order.id, item.model.id, item.quantity]);
      end;

      Self.Query.SQL.Text :=
        'INSERT INTO purchase_order_tools (id_purchase_order, id_tool_model, tool_quantity) VALUES ' + insertStr;

      try
        Self.Query.ExecSQL;

        Self.GetToolTypes(order);

        Result := order;
      except
        order.Free;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPurchaseOrderRepository.UpdateStatus(
  aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
var
  order: TPurchaseOrder;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'UPDATE purchase_orders SET ' +
    'status = :status, status_updated_at = :updateDate ' +
    'WHERE id = :purchaseOrderId RETURNING *';

  Self.Query.ParamByName('status').AsString := StatusToString(aPurchaseOrder.status);
  Self.Query.ParamByName('updateDate').AsDateTime := Now;
  Self.Query.ParamByName('purchaseOrderId').AsInteger := aPurchaseOrder.id;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      order := TPurchaseOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
      if not Self.Query.FieldByName('status_updated_at').IsNull then
        order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;

      Self.GetToolTypes(order);

      Result := order;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

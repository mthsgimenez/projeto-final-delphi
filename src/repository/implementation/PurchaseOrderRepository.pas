unit PurchaseOrderRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param,
  PurchaseOrderRepositoryInterface, PurchaseOrderModel, ToolTypeModel, ToolTypeRepositoryInterface, SupplierRepositoryInterface;

type TPurchaseOrderRepository = class(TRepositoryBase, IPurchaseOrderRepository)
  private
    helper: TDBHelper;
    toolTypeRepository: IToolTypeRepository;
    supplierRepository: ISupplierRepository;
  public
    function Insert(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function Update(aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
    function FindById(aPurchaseOrderId: Integer): TPurchaseOrder;
    function FindAll(): TObjectList<TPurchaseOrder>;
    function DeleteById(aPurchaseOrderId: Integer): Boolean;
    function ExistsById(aPurchaseOrderId: Integer): Boolean;

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

function TPurchaseOrderRepository.DeleteById(
  aPurchaseOrderId: Integer): Boolean;
begin
  // noop
end;

function TPurchaseOrderRepository.ExistsById(
  aPurchaseOrderId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('purchase_orders', 'id', aPurchaseOrderId);
end;

function TPurchaseOrderRepository.FindAll: TObjectList<TPurchaseOrder>;
begin
end;

function TPurchaseOrderRepository.FindById(
  aPurchaseOrderId: Integer): TPurchaseOrder;
var
  order: TPurchaseOrder;
  toolType: TToolType;
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
      if not Self.Query.FieldByName('updated_at').IsNull then
        order.statusUpdatedAt := Self.Query.FieldByName('updated_at').AsDateTime;

      Self.Query.Close;
      Self.Query.SQL.Text :=
        'SELECT id_tool_model, tool_quantity FROM purchase_order_tools WHERE id_purchase_order = :purchaseOrderId';
      Self.Query.ParamByName('purchaseOrderId').AsInteger := aPurchaseOrderId;
      try
        Self.Query.Open;

        if not Self.Query.IsEmpty then begin
          while not Self.Query.Eof do begin
            toolType := Self.toolTypeRepository.FindById(
              Self.Query.FieldByName('id_tool_model').AsInteger
            );

            order.AddItemToOrder(
              toolType,
              Self.Query.FieldByName('tool_quantity').AsInteger
            );

            Self.Query.Next;
          end;
        end;

        Result := order;
      except
        order.Free;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPurchaseOrderRepository.Insert(
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

        order.AddItemToOrder(item.model, item.quantity);
      end;

      Self.Query.SQL.Text :=
        'INSERT INTO purchase_order_tools (id_purchase_order, id_tool_model, tool_quantity) VALUES ' + insertStr;

      try
        Self.Query.ExecSQL;

        Result := order;
      except
        order.Free;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPurchaseOrderRepository.Update(
  aPurchaseOrder: TPurchaseOrder): TPurchaseOrder;
begin

end;

end.

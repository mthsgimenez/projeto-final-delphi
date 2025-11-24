unit ServiceOrderRepository;

interface

uses System.Generics.Collections, System.SysUtils, DBHelper, RepositoryBase, Data.DB, FireDAC.Stan.Param, FireDAC.Comp.Client, FireDAC.DApt,
  ServiceOrderRepositoryInterface, ServiceOrderModel, ToolModel, ToolRepositoryInterface, SupplierRepositoryInterface;

type TServiceOrderRepository = class(TRepositoryBase, IServiceOrderRepository)
  private
    supplierRepository: ISupplierRepository;
    toolRepository: IToolRepository;

    procedure GetTools(aServiceOrder: TServiceOrder);
  public
    function CreateServiceOrder(aServiceOrder: TServiceOrder): TServiceOrder;
    function UpdateStatus(aServiceOrder: TServiceOrder): TServiceOrder;
    function FindById(aServiceOrderId: Integer): TServiceOrder;
    function FindAll(): TObjectList<TServiceOrder>;
    function FindOpen(): TObjectList<TServiceOrder>;

    constructor Create(aToolRepository: IToolRepository;
      aSupplierRepository: ISupplierRepository);
end;

implementation

{ TServiceOrderRepository }

constructor TServiceOrderRepository.Create(aToolRepository: IToolRepository;
  aSupplierRepository: ISupplierRepository);
begin
  inherited Create;

  Self.toolRepository := aToolRepository;
  Self.supplierRepository := aSupplierRepository;
end;

function TServiceOrderRepository.CreateServiceOrder(
  aServiceOrder: TServiceOrder): TServiceOrder;
var
  insertStr: String;
  order: TServiceOrder;
  tool: TTool;
begin
  Result := nil;

  Self.Query.SQL.Text :=
  'INSERT INTO service_orders (id_supplier, price) VALUES (:supplierId, :price) RETURNING *';
  Self.Query.ParamByName('supplierId').AsInteger := aServiceOrder.supplier.id;
  Self.Query.ParamByName('price').AsCurrency := aServiceOrder.price;

  Self.Query.Connection.StartTransaction;
  try
    try
      Self.Query.Open;
    except
      Self.Query.Connection.Rollback;
      Exit;
    end;

    if not Self.Query.IsEmpty then begin
      order := TServiceOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := ServiceOrderModel.StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
      order.price := Self.Query.FieldByName('price').AsCurrency;

      Self.Query.Close;

      insertStr := '';
      for tool in aServiceOrder.items do begin
        if insertStr <> '' then
          insertStr := insertStr + ',';

        insertStr := insertStr + Format('(%d, %d)', [order.id, tool.id]);
      end;

      Self.Query.SQL.Text :=
        'INSERT INTO service_order_tools (id_service_order, id_tool) VALUES ' + insertStr;

      try
        Self.Query.ExecSQL;
        Self.Query.Connection.Commit;

        Self.GetTools(order);

        Result := order;
      except
        Self.Query.Connection.Rollback;
        order.Free;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TServiceOrderRepository.FindAll: TObjectList<TServiceOrder>;
var
  ordersList: TObjectList<TServiceOrder>;
  order: TServiceOrder;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM service_orders ORDER BY issued_at DESC;';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      ordersList := TObjectList<TServiceOrder>.Create;

      while not Self.Query.Eof do begin
        order := TServiceOrder.Create;

        order.id := Self.Query.FieldByName('id').AsInteger;
        order.supplier := Self.supplierRepository.FindById(
          Self.Query.FieldByName('id_supplier').AsInteger
        );
        order.status := ServiceOrderModel.StringToStatus(
          Self.Query.FieldByName('status').AsString
        );
        order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
        if not Self.Query.FieldByName('status_updated_at').IsNull then
          order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;
        order.price := Self.Query.FieldByName('price').AsCurrency;

        Self.GetTools(order);

        ordersList.Add(order);

        Self.Query.Next;
      end;

      Result := ordersList;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TServiceOrderRepository.FindById(
  aServiceOrderId: Integer): TServiceOrder;
var
  order: TServiceOrder;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM service_orders WHERE id = :serviceOrderId';
  Self.Query.ParamByName('serviceOrderId').AsInteger := aServiceOrderId;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      order := TServiceOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := ServiceOrderModel.StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
      if not Self.Query.FieldByName('status_updated_at').IsNull then
        order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;
      order.price := Self.Query.FieldByName('price').AsCurrency;

      Self.GetTools(order);

      Result := order;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TServiceOrderRepository.FindOpen: TObjectList<TServiceOrder>;
var
  ordersList: TObjectList<TServiceOrder>;
  order: TServiceOrder;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM service_orders WHERE "status" = ''OPEN'' ORDER BY issued_at DESC';

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      ordersList := TObjectList<TServiceOrder>.Create;

      while not Self.Query.Eof do begin
        order := TServiceOrder.Create;

        order.id := Self.Query.FieldByName('id').AsInteger;
        order.supplier := Self.supplierRepository.FindById(
          Self.Query.FieldByName('id_supplier').AsInteger
        );
        order.status := ServiceOrderModel.StringToStatus(
          Self.Query.FieldByName('status').AsString
        );
        order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
        if not Self.Query.FieldByName('status_updated_at').IsNull then
          order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;
        order.price := Self.Query.FieldByName('price').AsCurrency;

        Self.GetTools(order);

        ordersList.Add(order);

        Self.Query.Next;
      end;

      Result := ordersList;
    end;
  finally
    Self.Query.Close;
  end;
end;

procedure TServiceOrderRepository.GetTools(aServiceOrder: TServiceOrder);
var
  toolsQuery: TFDQuery;
  tool: TTool;
begin
  toolsQuery := TFDQuery.Create(Self.Query.Connection);
  toolsQuery.Connection := Self.Query.Connection;

  toolsQuery.SQL.Text :=
    'SELECT id_tool FROM service_order_tools WHERE id_service_order = :serviceOrderId';
  toolsQuery.ParamByName('serviceOrderId').AsInteger := aServiceOrder.id;

  try
    toolsQuery.Open;

    if not toolsQuery.IsEmpty then begin
      while not toolsQuery.Eof do begin
        tool := Self.toolRepository.FindById(
          toolsQuery.FieldByName('id_tool').AsInteger
        );

        aServiceOrder.items.Add(tool);

        toolsQuery.Next;
      end;
    end;
  finally
    toolsQuery.Close;
    toolsQuery.Free;
  end;
end;

function TServiceOrderRepository.UpdateStatus(
  aServiceOrder: TServiceOrder): TServiceOrder;
var
  order: TServiceOrder;
begin
  Result := nil;

  Self.Query.SQL.Text :=
    'UPDATE service_orders SET ' +
    'status = :status, status_updated_at = :updateDate ' +
    'WHERE id = :serviceOrderId RETURNING *';

  Self.Query.ParamByName('status').AsString := ServiceOrderModel.StatusToString(aServiceOrder.status);
  Self.Query.ParamByName('updateDate').AsDateTime := Now;
  Self.Query.ParamByName('serviceOrderId').AsInteger := aServiceOrder.id;

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      order := TServiceOrder.Create;

      order.id := Self.Query.FieldByName('id').AsInteger;
      order.supplier := Self.supplierRepository.FindById(
        Self.Query.FieldByName('id_supplier').AsInteger
      );
      order.status := ServiceOrderModel.StringToStatus(
        Self.Query.FieldByName('status').AsString
      );
      order.issuedAt := Self.Query.FieldByName('issued_at').AsDateTime;
      if not Self.Query.FieldByName('status_updated_at').IsNull then
        order.statusUpdatedAt := Self.Query.FieldByName('status_updated_at').AsDateTime;
      order.price := Self.Query.FieldByName('price').AsCurrency;

      Self.GetTools(order);

      Result := order;
    end;
  finally
    Self.Query.Close;
  end;
end;

end.

unit ToolTypeRepository;

interface

uses
  CrudRepositoryInterface, ToolTypeModel, DBHelper,
  ToolTypeDAOInterface, SupplierDAOInterface,
  System.Generics.Collections;

type
  TToolTypeRepository = class(TInterfacedObject, ICrudRepository<TToolType>)
  private
    toolTypeDAO: IToolTypeDAO;
    supplierDAO: ISupplierDAO;
    helper: TDBHelper;
  public
    function Save(aToolType: TToolType): TToolType;
    function FindById(aId: Integer): TToolType;
    function FindAll(): TObjectList<TToolType>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
    constructor Create(aToolTypeDAO: IToolTypeDAO; aSupplierDAO: ISupplierDAO);
    destructor Destroy; override;
  end;

implementation

{ TToolTypeRepository }

constructor TToolTypeRepository.Create(aToolTypeDAO: IToolTypeDAO; aSupplierDAO: ISupplierDAO);
begin
  Self.toolTypeDAO := aToolTypeDAO;
  Self.supplierDAO := aSupplierDAO;
  Self.helper := TDBHelper.Create;
end;

destructor TToolTypeRepository.Destroy;
begin
  Self.helper.Free;
  inherited;
end;

function TToolTypeRepository.DeleteById(aId: Integer): Boolean;
begin
  Result := Self.toolTypeDAO.DeleteById(aId);
end;

function TToolTypeRepository.ExistsById(aId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('tools_models', 'id', aId);
end;

function TToolTypeRepository.FindAll: TObjectList<TToolType>;
var
  toolTypes: TObjectList<TToolType>;
  toolType: TToolType;
begin
  toolTypes := Self.toolTypeDAO.SelectAll;

  if Assigned(toolTypes) and (toolTypes.Count > 0) then
    for toolType in toolTypes do
      toolType.supplier := Self.supplierDAO.SelectByToolTypeId(toolType.id);

  Result := toolTypes;
end;

function TToolTypeRepository.FindById(aId: Integer): TToolType;
var
  toolType: TToolType;
begin
  toolType := Self.toolTypeDAO.SelectById(aId);

  if Assigned(toolType) then
    toolType.supplier := Self.supplierDAO.SelectByToolTypeId(toolType.id);

  Result := toolType;
end;

function TToolTypeRepository.Save(aToolType: TToolType): TToolType;
var
  toolType: TToolType;
begin
  if aToolType.id = 0 then begin
    toolType := Self.toolTypeDAO.Insert(aToolType)
  end else begin
    toolType := Self.toolTypeDAO.Update(aToolType);
  end;

  if Assigned(toolType) then
    toolType.supplier := Self.supplierDAO.SelectByToolTypeId(toolType.id);

  Result := toolType;
end;

end.


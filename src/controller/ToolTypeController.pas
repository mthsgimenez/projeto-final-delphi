unit ToolTypeController;

interface

uses
  ToolTypeRepository, ToolTypeModel, ToolTypeDTO, SupplierRepository, SupplierModel,
  Logging, Session, System.Generics.Collections, System.SysUtils;

type
  TToolTypeController = class
  private
    toolTypeRepository: TToolTypeRepository;
    supplierRepository: TSupplierRepository;
  public
    constructor Create(aToolTypeRepository: TToolTypeRepository; aSupplierRepository: TSupplierRepository);
    function GetAll: TObjectList<TToolType>;
    function GetById(aToolId: Integer): TToolType;
    function CreateToolType(aToolTypeDTO: TToolTypeDTO): TToolType;
    function EditToolType(aToolTypeId: Integer; aToolTypeDTO: TToolTypeDTO): TToolType;
    function DeleteToolType(aToolTypeId: Integer): Boolean;
  end;

implementation

{ TToolTypeController }

constructor TToolTypeController.Create(aToolTypeRepository: TToolTypeRepository;
  aSupplierRepository: TSupplierRepository);
begin
  Self.toolTypeRepository := aToolTypeRepository;
  Self.supplierRepository := aSupplierRepository;
end;

function TToolTypeController.GetAll: TObjectList<TToolType>;
begin
  Result := Self.toolTypeRepository.FindAll;
end;

function TToolTypeController.GetById(aToolId: Integer): TToolType;
begin
  Result := Self.toolTypeRepository.FindById(aToolId);
end;

function TToolTypeController.CreateToolType(aToolTypeDTO: TToolTypeDTO): TToolType;
var
  toolType: TToolType;
  supplier: TSupplier;
begin
  toolType := TToolType.Create;
  try
    supplier := Self.supplierRepository.FindById(aToolTypeDTO.id_supplier);
    if not Assigned(supplier) then
      raise Exception.Create('Fornecedor com id ' + IntToStr(aToolTypeDTO.id_supplier) + ' não encontrado.');

    toolType.code := aToolTypeDTO.code;
    toolType.description := aToolTypeDTO.description;
    toolType.family := aToolTypeDTO.family;
    toolType.usage := aToolTypeDTO.usage;
    toolType.price := aToolTypeDTO.price;
    toolType.image := aToolTypeDTO.image;
    toolType.supplier := supplier;

    Result := Self.toolTypeRepository.Save(toolType);

    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Modelo de ferramenta criado: Usuário (ID: %d) criou o modelo (ID: %d)',
        [TSession.GetUser.id, Result.id]
      ));
  finally
    toolType.Free;
  end;
end;

function TToolTypeController.EditToolType(aToolTypeId: Integer; aToolTypeDTO: TToolTypeDTO): TToolType;
var
  toolType: TToolType;
  supplier: TSupplier;
begin
  toolType := Self.toolTypeRepository.FindById(aToolTypeId);

  if not Assigned(toolType) then
    raise Exception.Create('Modelo de ferramenta com id ' + IntToStr(aToolTypeId) + ' não encontrado.');

  try
    supplier := Self.supplierRepository.FindById(aToolTypeDTO.id_supplier);
    if not Assigned(supplier) then
      raise Exception.Create('Fornecedor com id ' + IntToStr(aToolTypeDTO.id_supplier) + ' não encontrado.');

    toolType.code := aToolTypeDTO.code;
    toolType.description := aToolTypeDTO.description;
    toolType.family := aToolTypeDTO.family;
    toolType.usage := aToolTypeDTO.usage;
    toolType.price := aToolTypeDTO.price;
    toolType.image := aToolTypeDTO.image;
    toolType.supplier := supplier;

    Result := Self.toolTypeRepository.Save(toolType);

    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Modelo de ferramenta alterado: Usuário (ID: %d) alterou o modelo (ID: %d)',
        [TSession.GetUser.id, aToolTypeId]
      ));
  finally
    toolType.Free;
  end;
end;

function TToolTypeController.DeleteToolType(aToolTypeId: Integer): Boolean;
begin
  Result := Self.toolTypeRepository.DeleteById(aToolTypeId);

  if Result then
    TLogger.GetLogger.Info(Format(
      'Modelo de ferramenta deletado: Usuário (ID: %d) deletou o modelo (ID: %d)',
      [TSession.GetUser.id, aToolTypeId]
    ));
end;

end.


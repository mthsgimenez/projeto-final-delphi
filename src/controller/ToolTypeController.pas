unit ToolTypeController;

interface

uses
  ToolTypeRepositoryInterface, ToolTypeModel, ToolTypeDTO, SupplierRepositoryInterface, SupplierModel,
  Logging, Session, System.Generics.Collections, System.SysUtils, Vcl.Graphics, System.IOUtils;

type
  TToolTypeController = class
  private
    toolTypeRepository: IToolTypeRepository;
    supplierRepository: ISupplierRepository;
    function SaveImage(aPath: String): String;
  public
    constructor Create(aToolTypeRepository: IToolTypeRepository; aSupplierRepository: ISupplierRepository);
    function GetAll: TObjectList<TToolType>;
    function GetById(aToolId: Integer): TToolType;
    function CreateToolType(aToolTypeDTO: TToolTypeDTO): TToolType;
    function EditToolType(aToolTypeId: Integer; aToolTypeDTO: TToolTypeDTO): TToolType;
    function DeleteToolType(aToolTypeId: Integer): Boolean;
    function LoadImage(aPath: String): TPicture;
  end;

implementation

{ TToolTypeController }

constructor TToolTypeController.Create(aToolTypeRepository: IToolTypeRepository;
  aSupplierRepository: ISupplierRepository);
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

function TToolTypeController.LoadImage(aPath: String): TPicture;
var
  picture: TPicture;
begin
  if not FileExists(aPath) then
    raise Exception.Create('Imagem não encontrada');

  picture := TPicture.Create;

  try
    picture.LoadFromFile(aPath);
    Result := picture;
  except
    on e: Exception do begin
      picture.Free;
      raise Exception.Create('Erro ao carregar imagem: ' + e.Message);
    end;
  end;
end;

function TToolTypeController.SaveImage(aPath: String): String;
var
  imagesFolder, destPath, fileName: string;
begin
  if not TFile.Exists(aPath) then
    raise Exception.Create('Imagem não encontrada.');

  try
    imagesFolder := TPath.Combine(ExtractFilePath(ParamStr(0)), 'images');

    fileName := TPath.GetFileName(aPath);

    destPath := TPath.Combine(imagesFolder, fileName);
    TFile.Copy(aPath, destPath, True);

    if not TFile.Exists(destPath) then
      raise Exception.Create('Não foi possível salvar a imagem');

    Result := destPath
  except
    on E: Exception do
      raise Exception.Create('Não foi possível salvar a imagem: ' + E.Message);
  end;
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
    toolType.supplier := supplier;
    if aToolTypeDTO.image <> '' then begin
      try
        toolType.image := Self.SaveImage(aToolTypeDTO.image);
      except
        on e: Exception do begin
          toolType.image := '';
        end;
      end;
    end;

    Result := Self.toolTypeRepository.Insert(toolType);

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
    toolType.supplier.Free;
    toolType.supplier := supplier;

    if aToolTypeDTO.image <> toolType.image then begin
      try
        toolType.image := Self.SaveImage(aToolTypeDTO.image);
      except
        on e: Exception do begin
          toolType.image := '';
        end;
      end;
    end;

    Result := Self.toolTypeRepository.Update(toolType);

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


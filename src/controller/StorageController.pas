unit StorageController;

interface

uses StorageRepositoryInterface, StorageModel, StorageDTO, System.Generics.Collections, System.SysUtils, Session, Logging, ToolTypeModel, ToolModel;

type TStorageController = class
  private
    storageRepository: IStorageRepository;
  public
    constructor Create(aStorageRepository: IStorageRepository);
    function CreateStorage(aStorage: TStorageDTO): TStorage;
    function EditStorage(aId: Integer; aData: TStorageDTO): TStorage;
    function GetStorage(aId: Integer): TStorage;
    function GetStorages: TObjectList<TStorage>;
    function DeleteStorage(aId: Integer): Boolean;
    function GetToolTypes(aStorageId: Integer): TObjectList<TToolType>;
    function GetTools(aStorageId: Integer; aToolTypeId: Integer): TObjectList<TTool>;
  end;

implementation

{ TStorageController }

constructor TStorageController.Create(aStorageRepository: IStorageRepository);
begin
  inherited Create;
  Self.storageRepository := aStorageRepository;
end;

function TStorageController.CreateStorage(aStorage: TStorageDTO): TStorage;
var
  storage: TStorage;
begin
  storage := TStorage.Create;
  try
    storage.name := aStorage.name;

    Result := Self.storageRepository.Insert(storage);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Estoque criado: Usuário (ID: %d) criou o estoque (ID: %d)',
        [TSession.GetUser.id, Result.id]
      ));
  finally
    storage.Free;
  end;
end;

function TStorageController.DeleteStorage(aId: Integer): Boolean;
begin
  Result := Self.storageRepository.DeleteById(aId);
  if Result then
    TLogger.GetLogger.Info(Format(
      'Estoque deletado: Usuário (ID: %d) deletou o estoque (ID: %d)',
      [TSession.GetUser.id, aId]
    ));
end;

function TStorageController.EditStorage(aId: Integer; aData: TStorageDTO): TStorage;
var
  storage: TStorage;
begin
  storage := Self.storageRepository.FindById(aId);

  try
    storage.name := aData.name;

    Result := Self.storageRepository.Update(storage);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Estoque editado: Usuário (ID: %d) editou o estoque (ID: %d)',
        [TSession.GetUser.id, Result.id]
      ));
  finally
    storage.Free;
  end;
end;

function TStorageController.GetStorage(aId: Integer): TStorage;
begin
  Result := Self.storageRepository.FindById(aId);
end;

function TStorageController.GetStorages: TObjectList<TStorage>;
begin
  Result := Self.storageRepository.FindAll;
end;

function TStorageController.GetTools(aStorageId: Integer;
  aToolTypeId: Integer): TObjectList<TTool>;
begin
  Result := Self.storageRepository.GetToolsInStorage(aStorageId, aToolTypeId);
end;

function TStorageController.GetToolTypes(
  aStorageId: Integer): TObjectList<TToolType>;
begin
  Result := Self.storageRepository.GetToolTypesInStorage(aStorageId);
end;

end.


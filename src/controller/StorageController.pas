unit StorageController;

interface

uses StorageRepository, StorageModel, StorageDTO, System.Generics.Collections, System.SysUtils, Session, Logging, ToolTypeModel;

type TStorageController = class
  private
    storageRepository: TStorageRepository;
  public
    constructor Create(aStorageRepository: TStorageRepository);
    function CreateStorage(aStorage: TStorageDTO): TStorage;
    function EditStorage(aId: Integer; aData: TStorageDTO): TStorage;
    function GetStorage(aId: Integer): TStorage;
    function GetStorages: TObjectList<TStorage>;
    function DeleteStorage(aId: Integer): Boolean;
    function GetToolTypes(aStorageId: Integer): TObjectList<TToolType>;
  end;

implementation

{ TStorageController }

constructor TStorageController.Create(aStorageRepository: TStorageRepository);
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

    Result := Self.storageRepository.Save(storage);
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

    Result := Self.storageRepository.Save(storage);
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

function TStorageController.GetToolTypes(
  aStorageId: Integer): TObjectList<TToolType>;
var
  storage: TStorage;
begin
  storage := Self.storageRepository.FindById(aStorageId);

  if not Assigned(storage) then
    raise Exception.Create(Format('Estoque com id %d não encontrado', [aStorageId]));

  Result := Self.storageRepository.FindToolTypes(storage);
  storage.Free;
end;

end.


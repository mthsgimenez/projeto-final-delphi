unit StorageController;

interface

uses StorageRepository, StorageModel, StorageDTO, System.Generics.Collections, System.SysUtils;

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
  Result := nil;

  storage := TStorage.Create;
  try
    storage.name := aStorage.name;

    Result := Self.storageRepository.Save(storage);
  finally
    storage.Free;
  end;
end;

function TStorageController.DeleteStorage(aId: Integer): Boolean;
begin
  Result := Self.storageRepository.DeleteById(aId);
end;

function TStorageController.EditStorage(aId: Integer; aData: TStorageDTO): TStorage;
var
  storage: TStorage;
begin
  storage := Self.storageRepository.FindById(aId);

  try
    storage.name := aData.name;

    Result := Self.storageRepository.Save(storage);
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

end.


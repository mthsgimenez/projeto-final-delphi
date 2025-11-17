unit StorageRepositoryInterface;

interface

uses CRUDRepositoryInterface, StorageModel, ToolTypeModel, ToolModel,
  System.Generics.Collections;

type IStorageRepository = interface(ICrudRepository<TStorage>)
  function GetToolTypesInStorage(aStorageId: Integer): TObjectList<TToolType>;
  function GetToolsInStorage(aStorageId: Integer; aToolTypeId: Integer): TObjectList<TTool>;
end;

implementation

end.

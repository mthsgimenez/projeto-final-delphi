unit StorageDAOInterface;

interface

uses DAOInterface, StorageModel, ToolTypeModel, System.Generics.Collections;

type IStorageDAO = interface(IDAO<TStorage>)
  function GetToolTypes(aStorage: TStorage): TObjectList<TToolType>;
end;

implementation

end.

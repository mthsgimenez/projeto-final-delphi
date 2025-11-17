unit StorageDAOInterface;

interface

uses DAOInterface, StorageModel, ToolTypeModel, System.Generics.Collections, ToolModel;

type IStorageDAO = interface(IDAO<TStorage>)
  function GetToolTypes(aStorage: TStorage): TObjectList<TToolType>;
  function SelectByToolId(aToolId: Integer): TStorage;
  function GetTools(aStorage: TStorage; aToolType: TToolType): TObjectList<TTool>;
end;

implementation

end.

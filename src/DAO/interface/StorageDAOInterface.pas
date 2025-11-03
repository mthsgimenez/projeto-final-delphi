unit StorageDAOInterface;

interface

uses DAOInterface, StorageModel;

type IStorageDAO = interface(IDAO<TStorage>)
  function GetToolCount(aStorage: TStorage): Integer;
end;

implementation

end.

unit ToolRepositoryInterface;

interface

uses CRUDRepositoryInterface, ToolModel, System.Generics.Collections;

type IToolRepository = interface(ICrudRepository<TTool>)
  function FindAvailable: TObjectList<TTool>;
end;

implementation

end.

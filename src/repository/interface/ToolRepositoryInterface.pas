unit ToolRepositoryInterface;

interface

uses CRUDRepositoryInterface, ToolModel;

type IToolRepository = interface(ICrudRepository<TTool>)
end;

implementation

end.

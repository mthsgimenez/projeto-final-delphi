unit ToolTypeRepositoryInterface;

interface

uses CRUDRepositoryInterface, ToolTypeModel;

type IToolTypeRepository = interface(ICrudRepository<TToolType>)
end;

implementation

end.

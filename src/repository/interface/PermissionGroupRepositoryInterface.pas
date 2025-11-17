unit PermissionGroupRepositoryInterface;

interface

uses CRUDRepositoryInterface, PermissionGroupModel;

type IPermissionGroupRepository = interface(ICrudRepository<TPermissionGroup>)
end;

implementation

end.

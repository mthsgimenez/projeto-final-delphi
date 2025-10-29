unit PermissionGroupDAOInterface;

interface

uses DAOInterface, PermissionGroupModel;

type IPermissionGroupDAO = interface(IDAO<TPermissionGroup>)
  function SelectByUserId(aUserId: Integer): TPermissionGroup;
end;

implementation

end.

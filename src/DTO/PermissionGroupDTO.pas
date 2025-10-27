unit PermissionGroupDTO;

interface

uses Permissions;

type TPermissionGroupDTO = record
  name: String;
  permissions: TPermissionsSet;
end;

implementation

end.

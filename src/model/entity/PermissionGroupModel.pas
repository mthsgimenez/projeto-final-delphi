unit PermissionGroupModel;

interface

uses Permissions;

type TPermissionGroup = class
  public
    id: Integer;
    name: String;
    permissions: TPermissionsSet;
    function hasPermission(aPermission: TPermissions): Boolean;
    constructor Create;
end;

implementation

{ TPermissionGroup }

constructor TPermissionGroup.Create;
begin
  Self.permissions := [];
end;

function TPermissionGroup.hasPermission(aPermission: TPermissions): Boolean;
begin
  Result := aPermission in Self.permissions;
end;

end.

unit Permissions;

interface

uses System.SysUtils;

type TPermissions = (
    USERS_CREATE = 1,
    USERS_UPDATE = 2,
    USERS_DELETE = 3,
    USERS_PERMISSIONS = 4
);

type TPermissionsSet = set of TPermissions;

function IntToPermission(aInt: Integer): TPermissions;

implementation

function IntToPermission(aInt: Integer): TPermissions;
begin
  if (aInt < Ord(Low(TPermissions))) or (aInt > Ord(High(TPermissions))) then begin
    raise Exception.Create('Invalid permission int');
  end;

  Result := TPermissions(aInt);
end;

end.

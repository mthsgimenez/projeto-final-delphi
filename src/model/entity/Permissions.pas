unit Permissions;

interface

uses System.SysUtils;

type TPermissions = (
    USERS_REGISTER = 1,
    USERS_EDIT = 2,
    USERS_PERMISSIONS = 3
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

unit UserModel;

interface

uses Bcrypt, Permissions;

type TUserModel = class
  private
    hash: String;
  public
    id: Integer;
    name: String;
    login: String;
    permissions: TPermissionsSet;
    procedure SetPassword(aPassword: String);
    function CheckPassword(aPassword: String): Boolean;
    procedure SetHash(aHash: String);
    function GetHash: String;
    function hasPermission(aPermission: TPermissions): Boolean;
end;

implementation

{ TUserModel }

function TUserModel.CheckPassword(aPassword: String): Boolean;
var
  needsRehash: Boolean;
begin
  Result := TBCrypt.CheckPassword(aPassword, Self.hash, needsRehash);
end;

function TUserModel.GetHash: String;
begin
  Result := Self.hash;
end;

function TUserModel.hasPermission(aPermission: TPermissions): Boolean;
begin
  Result := aPermission in Self.permissions;
end;

procedure TUserModel.SetHash(aHash: String);
begin
  if Length(aHash) = 60 then Self.hash := aHash;
end;

procedure TUserModel.SetPassword(aPassword: String);
begin
  Self.hash := TBCrypt.HashPassword(aPassword, 12);
end;

end.

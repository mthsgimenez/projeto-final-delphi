unit UserModel;

interface

uses Bcrypt, PermissionGroupModel;

type TUserModel = class
  private
    hash: String;
  public
    id: Integer;
    name: String;
    login: String;
    permissionGroup: TPermissionGroup;
    procedure SetPassword(aPassword: String);
    function CheckPassword(aPassword: String): Boolean;
    procedure SetHash(aHash: String);
    function GetHash: String;
    destructor Destroy; override;
end;

implementation

{ TUserModel }

function TUserModel.CheckPassword(aPassword: String): Boolean;
var
  needsRehash: Boolean;
begin
  Result := TBCrypt.CheckPassword(aPassword, Self.hash, needsRehash);
end;

destructor TUserModel.Destroy;
begin
  Self.permissionGroup.Free;
  inherited Destroy;
end;

function TUserModel.GetHash: String;
begin
  Result := Self.hash;
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

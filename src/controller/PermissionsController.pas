unit PermissionsController;

interface

uses PermissionGroupRepository, PermissionGroupModel, Permissions, PermissionGroupDTO, System.Generics.Collections;

type TPermissionController = class
  private
    permissionRepository: TPermissionGroupRepository;
  public
    function GetGroups: TObjectList<TPermissionGroup>;
    constructor Create(aPermissionRepository: TPermissionGroupRepository);
    function CreateGroup(aGroup: TPermissionGroupDTO): TPermissionGroup;
    function EditGroup(aGroupId: Integer; aData: TPermissionGroupDTO): TPermissionGroup;
    function DeleteGroup(aGroupId: Integer): Boolean;
end;

implementation

{ TPermissionController }

constructor TPermissionController.Create(aPermissionRepository: TPermissionGroupRepository);
begin
  Self.permissionRepository := aPermissionRepository;
end;

function TPermissionController.CreateGroup(
  aGroup: TPermissionGroupDTO): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;
  group := TPermissionGroup.Create;
  group.permissions := [];
  try
    group.name := aGroup.name;
    group.permissions := aGroup.permissions;
    Result := Self.permissionRepository.Save(group);
  finally
    group.Free;
  end;
end;

function TPermissionController.DeleteGroup(aGroupId: Integer): Boolean;
begin
  Result := Self.permissionRepository.DeleteById(aGroupId);
end;

function TPermissionController.EditGroup(aGroupId: Integer;
  aData: TPermissionGroupDTO): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;

  group := Self.permissionRepository.FindById(aGroupId);
  try
    group.name := aData.name;
    group.permissions := aData.permissions;
    Result := Self.permissionRepository.Save(group);
  finally
    group.Free;
  end;
end;

function TPermissionController.GetGroups: TObjectList<TPermissionGroup>;
begin
  Result := Self.permissionRepository.FindAll;
end;

end.

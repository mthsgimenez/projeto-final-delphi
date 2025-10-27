unit PermissionsController;

interface

uses PermissionGroupRepository, PermissionGroupModel, Permissions, PermissionGroupDTO, System.Generics.Collections;

type TPermissionController = class
  private
    PermissionRepository: TPermissionGroupRepository;
  public
    function GetGroups: TObjectList<TPermissionGroup>;
    constructor Create;
    function CreateGroup(aGroup: TPermissionGroupDTO): TPermissionGroup;
    function EditGroup(aGroupId: Integer; aData: TPermissionGroupDTO): TPermissionGroup;
    function DeleteGroup(aGroupId: Integer): Boolean;
end;

implementation

{ TPermissionController }

constructor TPermissionController.Create;
begin
  Self.PermissionRepository := TPermissionGroupRepository.GetInstance;
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
    Result := Self.PermissionRepository.Save(group);
  finally
    group.Free;
  end;
end;

function TPermissionController.DeleteGroup(aGroupId: Integer): Boolean;
begin
  Result := Self.PermissionRepository.DeleteById(aGroupId);
end;

function TPermissionController.EditGroup(aGroupId: Integer;
  aData: TPermissionGroupDTO): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;

  group := Self.PermissionRepository.FindById(aGroupId);
  try
    group.name := aData.name;
    group.permissions := aData.permissions;
    Result := Self.PermissionRepository.Save(group);
  finally
    group.Free;
  end;
end;

function TPermissionController.GetGroups: TObjectList<TPermissionGroup>;
begin
  Result := Self.PermissionRepository.FindAll;
end;

end.

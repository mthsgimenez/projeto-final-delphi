unit PermissionsController;

interface

uses PermissionGroupRepository, PermissionGroupModel, Permissions, System.Generics.Collections;

type TPermissionController = class
  private
    PermissionRepository: TPermissionGroupRepository;
  public
    function GetGroups: TObjectList<TPermissionGroup>;
    constructor Create;
end;

implementation

{ TPermissionController }

constructor TPermissionController.Create;
begin
  Self.PermissionRepository := TPermissionGroupRepository.GetInstance;
end;

function TPermissionController.GetGroups: TObjectList<TPermissionGroup>;
begin
  Result := Self.PermissionRepository.FindAll;
end;

end.

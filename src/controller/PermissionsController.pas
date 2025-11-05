unit PermissionsController;

interface

uses PermissionGroupRepository, PermissionGroupModel, Permissions, PermissionGroupDTO,
  UserRepository, UserModel, Logging, Session,
  System.Generics.Collections, System.SysUtils;

type TPermissionController = class
  private
    permissionRepository: TPermissionGroupRepository;
    userRepository: TUserRepository;
  public
    function GetGroups: TObjectList<TPermissionGroup>;
    constructor Create(aPermissionRepository: TPermissionGroupRepository; aUserRepository: TUserRepository);
    function CreateGroup(aGroup: TPermissionGroupDTO): TPermissionGroup;
    function EditGroup(aGroupId: Integer; aData: TPermissionGroupDTO): TPermissionGroup;
    function DeleteGroup(aGroupId: Integer): Boolean;
    function AddUserToGroup(aUserId: Integer; aGroupId: Integer): TUserModel;
    function RemoveUserFromGroup(aUserId: Integer): Boolean;
end;

implementation

{ TPermissionController }

function TPermissionController.AddUserToGroup(aUserId,
  aGroupId: Integer): TUserModel;
var
  user: TUserModel;
  group: TPermissionGroup;
begin
  user := Self.userRepository.FindById(aUserId);

  if not Assigned(user) then
    raise Exception.Create('Usuário com id ' + IntToStr(aUserId) + ' não encontrado.');

  if Assigned(user.permissionGroup) then begin
    user.Free;
    raise Exception.Create('Usuário já pertence à um grupo');
  end;

  group := Self.permissionRepository.FindById(aGroupId);
  if not Assigned(group) then
    raise Exception.Create('Grupo com id ' + IntToStr(aGroupId) + ' não encontrado.');

  user.permissionGroup := group;
  Result := Self.userRepository.Save(user);

  TLogger.GetLogger.Info(Format(
    'Grupo de permissões alterado: Usuário (ID: %d) foi adicionado ao grupo de permissões (ID: %d) pelo usuário (ID: %d)',
    [user.id, group.id, TSession.GetUser.id]
  ));

  user.Free;
end;

constructor TPermissionController.Create(
  aPermissionRepository: TPermissionGroupRepository;
  aUserRepository: TUserRepository);
begin
  Self.permissionRepository := aPermissionRepository;
  Self.userRepository := aUserRepository;
end;

function TPermissionController.CreateGroup(
  aGroup: TPermissionGroupDTO): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  group := TPermissionGroup.Create;
  group.permissions := [];
  try
    group.name := aGroup.name;
    group.permissions := aGroup.permissions;
    Result := Self.permissionRepository.Save(group);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Grupo de permissões adicionado: Usuário (ID: %d) criou o grupo de permissões (ID: %d)',
        [TSession.GetUser.id, Result.id]
      ));

  finally
    group.Free;
  end;
end;

function TPermissionController.DeleteGroup(aGroupId: Integer): Boolean;
begin
  Result := Self.permissionRepository.DeleteById(aGroupId);
  if Result then
    TLogger.GetLogger.Info(Format(
      'Grupo de permissões deletado: Usuário (ID: %d) deletou o grupo de permissões (ID: %d)',
      [TSession.GetUser.id, aGroupId]
    ));
end;

function TPermissionController.EditGroup(aGroupId: Integer;
  aData: TPermissionGroupDTO): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  group := Self.permissionRepository.FindById(aGroupId);
  try
    group.name := aData.name;
    group.permissions := aData.permissions;
    Result := Self.permissionRepository.Save(group);
    if Assigned(Result) then
      TLogger.GetLogger.Info(Format(
        'Grupo de permissões alterado: Usuário (ID: %d) alterou as permissões do grupo de permissões (ID: %d)',
        [TSession.GetUser.id, aGroupId]
      ));
  finally
    group.Free;
  end;
end;

function TPermissionController.GetGroups: TObjectList<TPermissionGroup>;
begin
  Result := Self.permissionRepository.FindAll;
end;

function TPermissionController.RemoveUserFromGroup(aUserId: Integer): Boolean;
var
  user: TUserModel;
  _: TUserModel;
  groupId: Integer;
begin
  user := Self.userRepository.FindById(aUserId);

  if not Assigned(user) then
    raise Exception.Create('Usuário com id ' + IntToStr(aUserId) + ' não encontrado.');

  if not Assigned(user.permissionGroup) then begin
    user.Free;
    raise Exception.Create('Usuário não está em nenhum grupo');
  end;

  groupId := user.permissionGroup.id;
  user.permissionGroup.Free;
  user.permissionGroup := nil;
  try
    _ := Self.userRepository.Save(user);
    if Assigned(_) then
    TLogger.GetLogger.Info(Format(
      'Grupo de permissões alterado: Usuário (ID: %d) removeu o usuário (ID: %d) do grupo de permissões (ID: %d)',
      [TSession.GetUser.id, _.id, groupId]
    ));
    _.Free;
  finally
    user.Free;
  end;
  Result := True;
end;

end.

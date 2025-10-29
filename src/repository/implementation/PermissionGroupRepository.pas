unit PermissionGroupRepository;

interface

uses CrudRepositoryInterface, PermissionGroupModel, PermissionGroupDAOInterface, DBHelper,
  System.Generics.Collections;

type TPermissionGroupRepository = class(TInterfacedObject, ICrudRepository<TPermissionGroup>)
  private
    permissionDAO: IPermissionGroupDAO;
    helper: TDBHelper;
  public
    function Save(aGroup: TPermissionGroup): TPermissionGroup;
    function FindById(aGroupId: Integer): TPermissionGroup;
    function FindAll(): TObjectList<TPermissionGroup>;
    function DeleteById(aGroupId: Integer): Boolean;
    function ExistsById(aGroupId: Integer): Boolean;
    function FindByUserId(aUserId: Integer): TPermissionGroup;
    constructor Create(aPermissionDAO: IPermissionGroupDAO);
    destructor Destroy; override;
end;

implementation

{ TPermissionGroupRepository }

constructor TPermissionGroupRepository.Create(aPermissionDAO: IPermissionGroupDAO);
begin
  Self.permissionDAO := aPermissionDAO;
  Self.helper := TDBHelper.Create;
end;

destructor TPermissionGroupRepository.Destroy;
begin
  Self.helper.Free;
  inherited;
end;

function TPermissionGroupRepository.DeleteById(aGroupId: Integer): Boolean;
begin
  Result := Self.permissionDAO.DeleteById(aGroupId);
end;

function TPermissionGroupRepository.ExistsById(aGroupId: Integer): Boolean;
begin
  Result := helper.CheckIfAlreadyExists('permission_groups', 'id', aGroupId);
end;

function TPermissionGroupRepository.FindAll: TObjectList<TPermissionGroup>;
begin
  Result := Self.permissionDAO.SelectAll;
end;

function TPermissionGroupRepository.FindById(
  aGroupId: Integer): TPermissionGroup;
begin
  Result := Self.permissionDAO.SelectById(aGroupId);
end;

function TPermissionGroupRepository.FindByUserId(
  aUserId: Integer): TPermissionGroup;
begin
  Result := Self.permissionDAO.SelectByUserId(aUserId);
end;

function TPermissionGroupRepository.Save(
  aGroup: TPermissionGroup): TPermissionGroup;
begin
  if aGroup.id = 0 then begin
    Result := Self.permissionDAO.Insert(aGroup);
  end else begin
    Result := Self.permissionDAO.Update(aGroup);
  end;
end;

end.

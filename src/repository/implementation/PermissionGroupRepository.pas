unit PermissionGroupRepository;

interface

uses CrudRepositoryInterface, PermissionGroupModel,
  System.Generics.Collections;

type TPermissionGroupRepository = class(TInterfacedObject, ICrudRepository<TPermissionGroup>)
  function Save(aGroup: TPermissionGroup): TPermissionGroup;
  function FindById(aGroupId: Integer): TPermissionGroup;
  function FindAll(): TObjectList<TPermissionGroup>;
  function DeleteById(aGroupId: Integer): Boolean;
  function ExistsById(aGroupId: Integer): Boolean;
  function FindByUserId(aUserId: Integer): TPermissionGroup;
end;

implementation

{ TPermissionGroupRepository }

function TPermissionGroupRepository.DeleteById(aGroupId: Integer): Boolean;
begin

end;

function TPermissionGroupRepository.ExistsById(aGroupId: Integer): Boolean;
begin

end;

function TPermissionGroupRepository.FindAll: TObjectList<TPermissionGroup>;
begin

end;

function TPermissionGroupRepository.FindById(
  aGroupId: Integer): TPermissionGroup;
begin

end;

function TPermissionGroupRepository.FindByUserId(
  aUserId: Integer): TPermissionGroup;
begin

end;

function TPermissionGroupRepository.Save(
  aGroup: TPermissionGroup): TPermissionGroup;
begin

end;

end.

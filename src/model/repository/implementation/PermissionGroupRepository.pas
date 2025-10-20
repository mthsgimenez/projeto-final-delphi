unit PermissionGroupRepository;

interface

uses RepositoryBase, CrudRepositoryInterface, PermissionGroupModel, Permissions, System.SysUtils, FireDAC.Comp.Client, FireDAC.DApt, System.Classes, System.Generics.Collections, DBHelper, System.StrUtils, Data.DB;
type TPermissionGroupRepository = class(TRepositoryBase, ICrudRepository<TPermissionGroup>)
  private
    class var instance: TPermissionGroupRepository;
    function GetPermissions(aId: Integer): TPermissionsSet;
    procedure UpdatePermissions(aGroup: TPermissionGroup);
    constructor Create;
  public
    class function GetInstance: TPermissionGroupRepository;
    function Save(aGroup: TPermissionGroup): TPermissionGroup;
    function FindById(aId: Integer): TPermissionGroup;
    function FindAll: TObjectList<TPermissionGroup>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
end;


implementation

{ TPermissionGroupRepository }

constructor TPermissionGroupRepository.Create;
begin
  inherited Create;
end;

function TPermissionGroupRepository.DeleteById(aId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format(
    'DELETE FROM permission_groups WHERE id = %d',
    [aId]
  );

  try
    Self.Query.ExecSQL;
    Result := Self.Query.RowsAffected > 0;
  finally
    Self.Query.Close;
  end;
end;

function TPermissionGroupRepository.ExistsById(aId: Integer): Boolean;
var
  helper: TDBHelper;
begin
  helper := TDBHelper.Create;
  Result := helper.CheckIfAlreadyExists('permission_groups', 'id', aId);
  helper.Free;
end;

function TPermissionGroupRepository.FindAll: TObjectList<TPermissionGroup>;
var
  groups: TObjectList<TPermissionGroup>;
  group: TPermissionGroup;
begin
  Result := nil;

  Self.Query.SQL.Text := 'SELECT * FROM permission_groups';
  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      groups := TObjectList<TPermissionGroup>.Create;
      Result := groups;

      while not Self.Query.Eof do begin
        group := TPermissionGroup.Create;
        group.id := Self.Query.FieldByName('id').AsInteger;
        group.name := Self.Query.FieldByName('name').AsString;
        group.permissions := Self.GetPermissions(group.id);

        groups.Add(group);

        Self.Query.Next;
      end;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPermissionGroupRepository.FindById(aId: Integer): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT * FROM permission_groups WHERE id = %d',
    [aId]
  );

  try
    Self.Query.Open;

    if not Self.Query.IsEmpty then begin
      group := TPermissionGroup.Create;
      group.id := Self.Query.FieldByName('id').AsInteger;
      group.name := Self.Query.FieldByName('name').AsString;
      group.permissions := Self.GetPermissions(group.id);

      Result := group;
    end;
  finally
    Self.Query.Close;
  end;
end;

class function TPermissionGroupRepository.GetInstance: TPermissionGroupRepository;
begin
  if not Assigned(instance) then
    instance := TPermissionGroupRepository.Create;
  Result := instance;
end;

function TPermissionGroupRepository.GetPermissions(
  aId: Integer): TPermissionsSet;
var
  permQuery: TFDQuery;
  permissions: TPermissionsSet;
  permId: Integer;
begin
  permQuery := TFDQuery.Create(Self.Query.Connection);
  permQuery.Connection := Self.Query.Connection;

  permQuery.SQL.Text := Format(
    'SELECT id_permission AS id FROM permission_groups_permissions ' +
    'WHERE id_pgroup = %d',
    [aId]
  );

  try
    permQuery.Open();
    while not permQuery.Eof do begin
      permId := permQuery.FieldByName('id').AsInteger;
      permissions := permissions + [IntToPermission(permId)];

      permQuery.Next;
    end;

    Result := permissions;
  finally
    permQuery.Close;
    permQuery.Free;
  end;
end;

function TPermissionGroupRepository.Save(
  aGroup: TPermissionGroup): TPermissionGroup;
var
  group: TPermissionGroup;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO permission_groups VALUES (%s, %s) ' +
    'ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name ' +
    'RETURNING *',
    [IfThen(aGroup.id = 0, 'DEFAULT', IntToStr(aGroup.id)), QuotedStr(aGroup.name)]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExistsExcludingId('permission_groups', 'name', aGroup.name, aGroup.id) then
      raise Exception.Create('Já existe um grupo com o nome "' + aGroup.name + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      group := TPermissionGroup.Create;
      group.id := Self.Query.FieldByName('id').AsInteger;
      group.name := Self.Query.FieldByName('name').AsString;

      group.permissions := aGroup.permissions;
      Self.UpdatePermissions(group);
      group.permissions := Self.GetPermissions(group.id);

      Result := group;
    end;
  finally
    Self.Query.Close;
    helper.Free;
  end;
end;

procedure TPermissionGroupRepository.UpdatePermissions(
  aGroup: TPermissionGroup);
var
  pQuery: TFDQuery;
  perm: TPermissions;
  arguments: TStringList;
begin
  arguments := TStringList.Create;
  arguments.QuoteChar := #0;

  for perm in aGroup.permissions do begin
    arguments.Add(Format('(%d, %d)', [aGroup.id, Integer(perm)]));
  end;

  pQuery := TFDQuery.Create(Self.Query.Connection);
  pQuery.Connection := Self.Query.Connection;
  pQuery.Connection.StartTransaction;

  try
    try
      pQuery.SQL.Text := Format(
        'DELETE FROM permission_groups_permissions WHERE id_pgroup = %d',
        [aGroup.id]
      );
      pQuery.ExecSQL;

      if arguments.Count > 0 then begin
        pQuery.SQL.Text := Format(
          'INSERT INTO permission_groups_permissions (id_pgroup, id_permission) VALUES %s',
          [arguments.DelimitedText]
        );
        pQuery.ExecSQL;
      end;

      pQuery.Connection.Commit;
    except
      on e: Exception do begin
        pQuery.Connection.Rollback;
        raise Exception.Create('Erro ao atualizar as permissões do usuário ' + aGroup.name + ': ' + e.Message);
      end;
    end;
  finally
    arguments.Free;
    pQuery.Free;
  end;
end;

end.

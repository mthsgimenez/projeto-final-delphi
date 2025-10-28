unit PermissionGroupDAO;

interface

uses DAOInterface, DAOBase, Permissions, PermissionGroupModel,
System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,
DBHelper, Data.DB, FireDAC.Comp.Client, FireDAC.DApt;

type TPermissionGroupDAO = class(TDAOBase, IDAO<TPermissionGroup>)
  private
    function GetPermissions(aGroupId: Integer): TPermissionsSet;
    procedure UpdatePermissions(aGroup: TPermissionGroup);
  public
    function Insert(aGroup: TPermissionGroup): TPermissionGroup;
    function SelectById(aGroupId: Integer): TPermissionGroup;
    function SelectAll(): TObjectList<TPermissionGroup>;
    function Update(aGroup: TPermissionGroup): TPermissionGroup;
    function DeleteById(aGroupId: Integer): Boolean;
    function SelectByUserId(aUserId: Integer): TPermissionGroup;
end;

implementation

{ TPermissionGroupDAO }

function TPermissionGroupDAO.DeleteById(aGroupId: Integer): Boolean;
begin
  Self.Query.SQL.Text := Format(
    'DELETE FROM permission_groups WHERE id = %d',
    [aGroupId]
  );

  try
    try
      Self.Query.ExecSQL;
      Result := Self.Query.RowsAffected > 0;
    except
      Result := False;
    end;
  finally
    Self.Query.Close;
  end;
end;

function TPermissionGroupDAO.GetPermissions(aGroupId: Integer): TPermissionsSet;
var
  permQuery: TFDQuery;
  permissions: TPermissionsSet;
  permId: Integer;
begin
  permissions := [];

  permQuery := TFDQuery.Create(Self.Query.Connection);
  permQuery.Connection := Self.Query.Connection;

  permQuery.SQL.Text := Format(
    'SELECT id_permission AS id FROM permission_groups_permissions ' +
    'WHERE id_pgroup = %d',
    [aGroupId]
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

function TPermissionGroupDAO.Insert(aGroup: TPermissionGroup): TPermissionGroup;
var
  group: TPermissionGroup;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'INSERT INTO permission_groups ("name") VALUES (%s) RETURNING *;',
    [QuotedStr(aGroup.name)]
  );

  helper := TDBHelper.Create;
  try
    if helper.CheckIfAlreadyExists('permission_groups', 'name', aGroup.name) then
      raise Exception.Create('Já existe um grupo com o nome "' + aGroup.name + '"');

    Self.Query.Open();

    if not Self.Query.IsEmpty then begin
      group := TPermissionGroup.Create;
      group.id := Self.Query.FieldByName('id').AsInteger;
      group.name := Self.Query.FieldByName('name').AsString;

      group.permissions := [];
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

function TPermissionGroupDAO.SelectAll: TObjectList<TPermissionGroup>;
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

function TPermissionGroupDAO.SelectById(aGroupId: Integer): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT * FROM permission_groups WHERE id = %d',
    [aGroupId]
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

function TPermissionGroupDAO.SelectByUserId(aUserId: Integer): TPermissionGroup;
var
  group: TPermissionGroup;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'SELECT pgroup.* FROM users AS u ' +
    'JOIN permission_groups AS pgroup ' +
    'ON pgroup.id = u.id_pgroup ' +
    'WHERE u.id = %d',
    [aUserId]
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

function TPermissionGroupDAO.Update(aGroup: TPermissionGroup): TPermissionGroup;
var
  group: TPermissionGroup;
  helper: TDBHelper;
begin
  Result := nil;

  Self.Query.SQL.Text := Format(
    'UPDATE permission_groups SET "name" = %s WHERE id = %d RETURNING *',
    [QuotedStr(aGroup.name), aGroup.id]
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

      group.permissions := [];
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

procedure TPermissionGroupDAO.UpdatePermissions(aGroup: TPermissionGroup);
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

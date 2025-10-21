unit PermissionsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, PermissionsController, System.Generics.Collections, PermissionGroupModel,
  Vcl.CheckLst, Permissions;

type
  TformPermissions = class(TForm)
    panelContainer: TPanel;
    listGroups: TListBox;
    listPermissions: TCheckListBox;
    procedure FormShow(Sender: TObject);
    procedure listGroupsClick(Sender: TObject);
  private
    lastSelectedItem: Integer;
    permissionController: TPermissionController;
    groups: TObjectList<TPermissionGroup>;
    procedure UpdatePermissionList(aGroup: TPermissionGroup);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formPermissions: TformPermissions;

implementation

{$R *.dfm}

{ TformPermissions }

constructor TformPermissions.Create(AOwner: TComponent);
begin
  inherited;
  Self.permissionController := TPermissionController.Create;
  Self.groups := Self.permissionController.GetGroups;
  Self.lastSelectedItem := -1;
end;

destructor TformPermissions.Destroy;
begin
  Self.groups.Free;
  Self.permissionController.Free;
  inherited;
end;

procedure TformPermissions.FormShow(Sender: TObject);
var
  group: TPermissionGroup;
begin
  for group in Self.groups do
    Self.listGroups.AddItem(group.name, group);
end;

procedure TformPermissions.listGroupsClick(Sender: TObject);
var
  selectedGroup: TPermissionGroup;
begin
  if (Self.listGroups.ItemIndex <> Self.lastSelectedItem) and
  (Self.listGroups.ItemIndex <> -1) then begin
    Self.lastSelectedItem := Self.listGroups.ItemIndex;

    selectedGroup := TPermissionGroup(
      Self.listGroups.Items.Objects[Self.listGroups.ItemIndex]
    );

    Self.UpdatePermissionList(selectedGroup);
  end;
end;

procedure TformPermissions.UpdatePermissionList(aGroup: TPermissionGroup);
var
  i: Integer;
begin
  for I := 0 to Self.listPermissions.Items.Count - 1 do begin
    Self.listPermissions.Checked[i] := aGroup.hasPermission(IntToPermission(i + 1));
  end;
end;

end.

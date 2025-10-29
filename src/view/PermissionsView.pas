unit PermissionsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, PermissionsController, System.Generics.Collections, PermissionGroupModel,
  Vcl.CheckLst, Permissions, Vcl.ComCtrls, MessageHelper, PermissionGroupDTO, Dependencies,
  Vcl.Grids, UserModel, UserController;

type
  TformPermissions = class(TForm)
    pcontrolPermissions: TPageControl;
    tabList: TTabSheet;
    tabCreate: TTabSheet;
    listGroups: TListBox;
    listPermissionsView: TCheckListBox;
    buttonCreate: TButton;
    buttonEdit: TButton;
    buttonDelete: TButton;
    buttonUsers: TButton;
    editName: TEdit;
    listPermissions: TCheckListBox;
    labelName: TLabel;
    labelPermissions: TLabel;
    buttonSave: TButton;
    tabUsers: TTabSheet;
    gridUsers: TStringGrid;
    buttonRemoveUser: TButton;
    buttonAddUser: TButton;
    buttonBack: TButton;
    comboUsers: TComboBox;
    procedure listGroupsClick(Sender: TObject);
    procedure buttonEditClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
    procedure buttonCreateClick(Sender: TObject);
    procedure tabListShow(Sender: TObject);
    procedure buttonDeleteClick(Sender: TObject);
    procedure buttonUsersClick(Sender: TObject);
    procedure buttonBackClick(Sender: TObject);
    procedure gridUsersDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure gridUsersSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonRemoveUserClick(Sender: TObject);
  private
    selectedUser: TUserModel;
    users: TObjectList<TUserModel>;
    lastSelectedItem: Integer;
    permissionController: TPermissionController;
    userController: TUserController;
    selectedGroup: TPermissionGroup;
    groups: TObjectList<TPermissionGroup>;
    messageHelper: TMessageHelper;
    procedure UpdateGroupsList;
    procedure UpdatePermissionList(aList: TCheckListBox; aGroup: TPermissionGroup);
    procedure UpdateUsersList(aGroup: TPermissionGroup);
    procedure UpdateUsersCombobox;
    procedure ClearEdits;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formPermissions: TformPermissions;

implementation

{$R *.dfm}

{ TformPermissions }

procedure TformPermissions.buttonBackClick(Sender: TObject);
begin
  Self.selectedGroup := nil;
  Self.selectedUser := nil;
  Self.pcontrolPermissions.ActivePage := Self.pcontrolPermissions.Pages[0];
end;

procedure TformPermissions.buttonCreateClick(Sender: TObject);
begin
  Self.selectedGroup := nil;
  Self.pcontrolPermissions.ActivePage := Self.pcontrolPermissions.Pages[1];
end;

procedure TformPermissions.buttonDeleteClick(Sender: TObject);
var
  confirmation, deleted: Boolean;
begin
  if not Assigned(Self.selectedGroup) then begin
    Self.messageHelper.Warning('Nenhum grupo selecionado');
    Exit;
  end;

  confirmation := Self.messageHelper.Confirmation('Deseja mesmo deletar o grupo ' + Self.selectedGroup.name + '?');
  if confirmation then begin
    deleted := Self.permissionController.DeleteGroup(Self.selectedGroup.id);
    if deleted then begin
      Self.groups.Remove(Self.selectedGroup);
      Self.UpdateGroupsList;
      Self.selectedGroup := nil;
      Exit;
    end;
    Self.messageHelper.Error('Não foi possível excluir o grupo. Remova todos os usuários do grupo antes de excluí-lo.');
  end;
end;

procedure TformPermissions.buttonEditClick(Sender: TObject);
begin
  if not Assigned(Self.selectedGroup) then begin
    Self.messageHelper.Warning('Nenhum grupo selecionado');
    Exit;
  end;

  Self.editName.Text := Self.selectedGroup.name;
  Self.UpdatePermissionList(Self.listPermissions, Self.selectedGroup);

  Self.pcontrolPermissions.ActivePage := Self.pcontrolPermissions.Pages[1];
end;

procedure TformPermissions.buttonRemoveUserClick(Sender: TObject);
begin
  if not Assigned(self.selectedUser) then begin
    Self.messageHelper.Warning('Nenhum usuário selecionado');
    Exit;
  end;

  Self.selectedUser.permissionGroup := nil;
end;

procedure TformPermissions.buttonSaveClick(Sender: TObject);
var
  data: TPermissionGroupDTO;
  group: TPermissionGroup;
  i: Integer;
begin
  if Trim(Self.editName.Text) = '' then begin
    Self.messageHelper.Error('Nome do grupo não pode estar vazio.');
    Exit;
  end;

  data.permissions := [];
  data.name := Trim(Self.editName.Text);

  for i := 0 to Self.listPermissions.Count - 1 do begin
    if Self.listPermissions.Checked[i] then
      data.permissions := data.permissions + [IntToPermission(i + 1)];
  end;

  if Assigned(Self.selectedGroup) then begin
    group := Self.permissionController.EditGroup(Self.selectedGroup.id, data);
    if Assigned(group) then begin
      Self.groups.Remove(Self.selectedGroup);
      Self.groups.Add(group);
    end;
  end else begin
    group := Self.permissionController.CreateGroup(data);
    if Assigned(group) then
      Self.groups.Add(group);
  end;

  Self.selectedGroup := nil;
  Self.ClearEdits;
  Self.pcontrolPermissions.ActivePage := Self.pcontrolPermissions.Pages[0];
end;

procedure TformPermissions.buttonUsersClick(Sender: TObject);
begin
  if not Assigned(Self.selectedGroup) then begin
    Self.messageHelper.Warning('Nenhum grupo selecionado');
    Exit;
  end;

  Self.UpdateUsersList(Self.selectedGroup);
  Self.UpdateUsersCombobox;
  Self.selectedUser := nil;
  Self.pcontrolPermissions.ActivePage := Self.pcontrolPermissions.Pages[2];
end;

procedure TformPermissions.ClearEdits;
var
  i: Integer;
begin
  Self.editName.Clear;
  for i := 0 to Self.listPermissions.Count - 1 do begin
    Self.listPermissions.Checked[i] := False;
  end;
end;

constructor TformPermissions.Create(AOwner: TComponent);
begin
  inherited;
  Self.permissionController := TDependencies.GetInstance.GetPermissionController;
  Self.userController := TDependencies.GetInstance.GetUserController;
  Self.selectedGroup := nil;
  Self.groups := Self.permissionController.GetGroups;
  Self.messageHelper := TMessageHelper.GetInstance;
  Self.lastSelectedItem := -1;
  Self.users := Self.userController.GetUsers;
end;

destructor TformPermissions.Destroy;
begin
  Self.users.Free;
  Self.groups.Free;
  inherited;
end;

procedure TformPermissions.gridUsersDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
var
  grid: TStringGrid;
begin
  grid := TStringGrid(Sender);

  if ARow = 0 then begin
    grid.Canvas.Brush.Color := RGB($41, $69, $E1);
    grid.Canvas.Font.Color := RGB($FF, $FF, $FF);
  end;

  grid.Canvas.FillRect(Rect);
  grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, grid.Cells[ACol, ARow]);
end;

procedure TformPermissions.gridUsersSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedUser := TUserModel(Self.gridUsers.Objects[0, ARow]);
end;

procedure TformPermissions.listGroupsClick(Sender: TObject);
begin
  if (Self.listGroups.ItemIndex <> Self.lastSelectedItem) and
  (Self.listGroups.ItemIndex <> -1) then begin
    Self.lastSelectedItem := Self.listGroups.ItemIndex;

    Self.selectedGroup := TPermissionGroup(
      Self.listGroups.Items.Objects[Self.listGroups.ItemIndex]
    );

    Self.UpdatePermissionList(Self.listPermissionsView, Self.selectedGroup);
  end;
end;

procedure TformPermissions.tabListShow(Sender: TObject);
begin
  Self.UpdateGroupsList;
end;

procedure TformPermissions.UpdateGroupsList;
var
  group: TPermissionGroup;
begin
  Self.listGroups.Clear;
  for group in Self.groups do
    Self.listGroups.AddItem(group.name, group);
end;

procedure TformPermissions.UpdatePermissionList(aList: TCheckListBox; aGroup: TPermissionGroup);
var
  i: Integer;
begin
  for i := 0 to aList.Items.Count - 1 do begin
    aList.Checked[i] := aGroup.hasPermission(IntToPermission(i + 1));
  end;
end;

procedure TformPermissions.UpdateUsersCombobox;
var
  user: TUserModel;
begin
  Self.comboUsers.Clear;

  if Assigned(Self.users) then
    for user in Self.users do
      if not Assigned(user.permissionGroup) then
        Self.comboUsers.AddItem(user.login, user);
end;

procedure TformPermissions.UpdateUsersList(aGroup: TPermissionGroup);
var
  user: TUserModel;
  i: Integer;
begin
  with Self.gridUsers do begin
    RowCount := 1;
    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Login';
    ColWidths[0] := Width div 2;
    ColWidths[1] := Width div 2 - 5;

    if Assigned(Self.users) then
      for user in Self.users do begin
        if Assigned(user.permissionGroup) then
          if user.permissionGroup.id = aGroup.id then begin
            i := RowCount;
            RowCount := RowCount + 1;
            Objects[0, i] := user;
            Cells[0, i] := user.name;
            Cells[1, i] := user.login;
            Cells[2, i] := '';
            if Assigned(user.permissionGroup) then
              Cells[2, i] := user.permissionGroup.name;
          end;
      end;
  end;
end;

end.

unit UserView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls, UserController, UserDTO, UserModel, System.Generics.Collections, Permissions, Session, MessageHelper;

type
  TformUser = class(TForm)
    pcontrolUser: TPageControl;
    tabList: TTabSheet;
    tabCreate: TTabSheet;
    gridUsers: TStringGrid;
    buttonCreate: TButton;
    buttonDelete: TButton;
    buttonEdit: TButton;
    editName: TEdit;
    editLogin: TEdit;
    editPassword: TEdit;
    buttonSave: TButton;
    buttonCancel: TButton;
    listPermissions: TListView;
    procedure buttonCreateClick(Sender: TObject);
    procedure tabListShow(Sender: TObject);
    procedure buttonEditClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
    procedure buttonDeleteClick(Sender: TObject);
    procedure buttonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridUsersSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
  private
    controller: TUserController;
    selectedUser: TUserModel;
    users: TObjectList<TUserModel>;
    messageHelper: TMessageHelper;
    procedure UpdateGrid;
    procedure ClearEdits;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formUser: TformUser;

implementation

{$R *.dfm}

procedure TformUser.buttonCancelClick(Sender: TObject);
begin
  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[0];
  Self.ClearEdits;
end;

procedure TformUser.buttonCreateClick(Sender: TObject);
begin
  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[1];
end;

procedure TformUser.buttonDeleteClick(Sender: TObject);
begin
  if not Assigned(Self.selectedUser) then begin
    Self.messageHelper.Warning('Nenhum usuário selecionado.');
    Exit;
  end;

  if TSession.GetInstance.GetUser.id = Self.selectedUser.id then begin
    Self.messageHelper.Error('Você não pode desativar seu próprio usuário');
    Exit;
  end;

  if Self.messageHelper.Confirmation('Tem certeza de que quer desativar o usuário ' + Self.selectedUser.name + '?') then begin
    Self.controller.DeleteUser(Self.selectedUser.id);
    Self.users.Remove(Self.selectedUser);
    Self.selectedUser := nil;
    Self.UpdateGrid;
  end;
end;

procedure TformUser.buttonEditClick(Sender: TObject);
begin
  if not Assigned(Self.selectedUser) then begin
    Self.messageHelper.Warning('Nenhum usuário selecionado.');
    Exit;
  end;

  Self.editName.Text := Self.selectedUser.name;
  Self.editLogin.Text := Self.selectedUser.login;
  Self.listPermissions.Items[0].Checked := Self.selectedUser.hasPermission(TPermissions.USERS_CREATE);
  Self.listPermissions.Items[1].Checked := Self.selectedUser.hasPermission(TPermissions.USERS_UPDATE);
  Self.listPermissions.Items[2].Checked := Self.selectedUser.hasPermission(TPermissions.USERS_DELETE);
  Self.listPermissions.Items[3].Checked := Self.selectedUser.hasPermission(TPermissions.USERS_PERMISSIONS);

  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[1];
end;

procedure TformUser.buttonSaveClick(Sender: TObject);
var
  data: TUserDTO;
  user: TUserModel;
  errors: TStringList;
  perm: TListItem;
begin
  data.name := editName.Text;
  data.login := editLogin.Text;
  data.password := editPassword.Text;
  data.permissions := [];

  for perm in Self.listPermissions.Items do begin
    if perm.Checked then
      data.permissions := data.permissions + [IntToPermission(perm.Index + 1)];
  end;

  if Assigned(Self.selectedUser) then begin
    errors := data.ValidateDTO(False);

    try
      if errors.Count > 0 then begin
        Self.messageHelper.Error(errors.Text);
        Exit;
      end;
    finally
      errors.Free;
    end;

    user := Self.controller.EditUser(Self.selectedUser.id, data);

    Self.users.Remove(Self.selectedUser);
    Self.selectedUser := nil;

    Self.users.Add(user);
  end else begin
    errors := data.ValidateDTO(True);
    try
      if errors.Count > 0 then begin
        Self.messageHelper.Error(errors.Text);
        Exit;
      end;
    finally
      errors.Free;
    end;

    user := Self.controller.CreateUser(data);
    Self.users.Add(user);
  end;

  Self.ClearEdits;
  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[0];
end;

procedure TformUser.ClearEdits;
begin
  Self.editName.Clear;
  Self.editLogin.Clear;
  Self.editPassword.Clear;

  for var item in Self.listPermissions.Items do begin
    item.Checked := False;
  end;
end;

constructor TformUser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.controller := TUserController.Create;
  Self.users := Self.controller.GetUsers;
  Self.messageHelper := TMessageHelper.GetInstance;
end;

destructor TformUser.Destroy;
begin
  Self.users.Free;
  Self.controller.Free;
  inherited Destroy;
end;

procedure TformUser.FormCreate(Sender: TObject);
var
  user: TUserModel;
begin
  user := TSession.GetInstance.GetUser;

  Self.buttonCreate.Visible := user.hasPermission(TPermissions.USERS_CREATE);
  Self.buttonEdit.Visible := user.hasPermission(TPermissions.USERS_UPDATE);
  Self.buttonDelete.Visible := user.hasPermission(TPermissions.USERS_DELETE);
  Self.listPermissions.Visible := user.hasPermission(TPermissions.USERS_PERMISSIONS);
end;

procedure TformUser.gridUsersSelectCell(Sender: TObject; ACol, ARow: LongInt;
  var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedUser := TUserModel(Self.gridUsers.Objects[0, ARow]);
end;

procedure TformUser.tabListShow(Sender: TObject);
begin
  Self.UpdateGrid;
end;

procedure TformUser.UpdateGrid;
var
  user: TUserModel;
  i: Integer;
begin
  with Self.gridUsers do begin
    RowCount := 1;
    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Login';
    ColWidths[0] := Width div 2;
    ColWidths[1] := Width div 2;

    if Assigned(Self.users) then
      for user in Self.users do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := user;
        Cells[0, i] := user.name;
        Cells[1, i] := user.login;
      end;
  end;
end;

end.

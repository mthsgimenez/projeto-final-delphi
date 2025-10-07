unit UserView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls, UserController, UserDTO, UserModel, System.Generics.Collections, Permissions, Session;

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
  private
    controller: TUserController;
    selectedUserId: Integer;
    procedure UpdateGrid(aUsers: TObjectList<TUserModel>);
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
  Self.selectedUserId := 0;
end;

procedure TformUser.buttonCreateClick(Sender: TObject);
begin
  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[1];
end;

procedure TformUser.buttonDeleteClick(Sender: TObject);
var
  confirmation: Integer;
begin
  confirmation := MessageDlg(
    'Tem certeza de que quer desativar o usuário selecionado?',
    mtConfirmation,
    [mbYes, mbNo],
    0);

  if confirmation = mrYes then begin
    Self.selectedUserId := StrToInt(Self.gridUsers.Cells[0, Self.gridUsers.Row]);
    Self.controller.DeleteUser(Self.selectedUserId);
    Self.selectedUserId := 0;
    Self.tabListShow(nil);
  end;
end;

procedure TformUser.buttonEditClick(Sender: TObject);
var
  user: TUserModel;
begin
  Self.selectedUserId := StrToInt(Self.gridUsers.Cells[0, Self.gridUsers.Row]);

  user := Self.controller.GetUser(Self.selectedUserId);

  try
    Self.editName.Text := user.name;
    Self.editLogin.Text := user.login;
    Self.listPermissions.Items[0].Checked := user.hasPermission(TPermissions.USERS_CREATE);
    Self.listPermissions.Items[1].Checked := user.hasPermission(TPermissions.USERS_UPDATE);
    Self.listPermissions.Items[2].Checked := user.hasPermission(TPermissions.USERS_DELETE);
    Self.listPermissions.Items[3].Checked := user.hasPermission(TPermissions.USERS_PERMISSIONS);
  finally
    user.Free;
  end;

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

  if Self.selectedUserId <> 0 then begin
    errors := data.ValidateDTO(False);
    try
      if errors.Count > 0 then raise Exception.Create(errors.Text);    
    finally
      errors.Free;
    end;
    
    user := Self.controller.EditUser(Self.selectedUserId, data);
    user.Free;

    Self.selectedUserId := 0;
  end else begin
    errors := data.ValidateDTO(True);
    try
      if errors.Count > 0 then raise Exception.Create(errors.Text);    
    finally
      errors.Free;
    end;

    user := Self.controller.CreateUser(data);
    user.Free;
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
end;

destructor TformUser.Destroy;
begin
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

procedure TformUser.tabListShow(Sender: TObject);
var
  users: TObjectList<TUserModel>;
begin
  users := Self.controller.GetUsers;
  Self.UpdateGrid(users);
  users.Free;
end;

procedure TformUser.UpdateGrid(aUsers: TObjectList<TUserModel>);
var
  user: TUserModel;
  i: Integer;
begin
  with Self.gridUsers do begin
    RowCount := 1;
    Cells[0, 0] := 'Id';
    Cells[1, 0] := 'Name';
    Cells[2, 0] := 'Login';
    ColWidths[0] := 100;
    ColWidths[1] := 250;
    ColWidths[2] := 250;

    if aUsers <> nil then
    for user in aUsers do begin
      i := RowCount;
      RowCount := RowCount + 1;
      Cells[0, i] := IntToStr(user.id);
      Cells[1, i] := user.name;
      Cells[2, i] := user.login;
    end;
  end;
end;

end.

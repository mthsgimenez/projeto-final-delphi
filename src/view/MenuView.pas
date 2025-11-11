unit MenuView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, UserView, Vcl.Skia, PermissionsView, Session, Permissions, UserModel, SupplierView, ToolTypeView;

type
  TLogoutCallback = procedure of object;

  TformMenu = class(TForm)
    panelContainer: TPanel;
    panelMenu: TPanel;
    imgMenu: TImage;
    shapeSeparator: TShape;
    panelUser: TPanel;
    imgUser: TImage;
    labelUser: TLabel;
    buttonUserMenu: TSpeedButton;
    panelPermissions: TPanel;
    imgPermissions: TImage;
    labelPermissions: TLabel;
    buttonPermissions: TSpeedButton;
    labelUsername: TLabel;
    imgLogout: TImage;
    panelSuppliers: TPanel;
    labelSuppliers: TLabel;
    imgSuppliers: TImage;
    buttonSuppliers: TSpeedButton;
    panelToolTypes: TPanel;
    labelToolTypes: TLabel;
    Image1: TImage;
    buttonToolTypes: TSpeedButton;
    procedure imgMenuClick(Sender: TObject);
    procedure buttonUserMenuClick(Sender: TObject);
    procedure buttonPermissionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgLogoutClick(Sender: TObject);
    procedure buttonSuppliersClick(Sender: TObject);
    procedure buttonToolTypesClick(Sender: TObject);
  private
    logoutCallback: TLogoutCallback;
    openWidth: Integer;
    closedWidth: Integer;
    isMenuOpen: Boolean;
    activeForm: TForm;
    procedure ToggleMenu;
    procedure ChangeForm(aForm: TFormClass);
  public
    procedure setLogoutCallback(aCallback: TLogoutCallback);
    constructor Create(AOwner: TComponent; closedWidth: Integer; openWidth: Integer); reintroduce;
    destructor Destroy; override;
  end;

var
  formMenu: TformMenu;

implementation

{$R *.dfm}

{ TformMenu }

procedure TformMenu.buttonPermissionsClick(Sender: TObject);
begin
  Self.ChangeForm(TformPermissions);
  if Self.isMenuOpen then Self.ToggleMenu;
end;

procedure TformMenu.buttonSuppliersClick(Sender: TObject);
begin
  Self.ChangeForm(TformSupplier);
  if Self.isMenuOpen then Self.ToggleMenu;
end;

procedure TformMenu.buttonToolTypesClick(Sender: TObject);
begin
  Self.ChangeForm(TformToolType);
  if Self.isMenuOpen then Self.ToggleMenu;
end;

procedure TformMenu.buttonUserMenuClick(Sender: TObject);
begin
  Self.ChangeForm(TformUser);
  if Self.isMenuOpen then Self.ToggleMenu;
end;

procedure TformMenu.ChangeForm(aForm: TFormClass);
begin
  Self.activeForm.Free;
  Self.activeForm := aForm.Create(Self.panelContainer);
  Self.activeForm.Parent := Self.panelContainer;
  Self.activeForm.BorderStyle := bsNone;
  Self.activeForm.Show;
end;

constructor TformMenu.Create(AOwner: TComponent; closedWidth,
  openWidth: Integer);
begin
  inherited Create(AOwner);
  Self.closedWidth := closedWidth;
  Self.openWidth := openWidth;
  Self.isMenuOpen := False;
  Self.panelMenu.Width := Self.closedWidth;
end;

destructor TformMenu.Destroy;
begin
  Self.activeForm.Free;
  inherited;
end;

procedure TformMenu.FormCreate(Sender: TObject);
var
  user: TUserModel;
begin
  user := TSession.GetUser;

  if Assigned(user.permissionGroup) then begin
    Self.panelPermissions.Visible := user.permissionGroup.hasPermission(GROUP_PERMISSIONS)
  end else begin
    Self.panelPermissions.Visible := False;
  end;

  Self.labelUsername.Caption := user.login;
end;

procedure TformMenu.imgLogoutClick(Sender: TObject);
begin
  Self.logoutCallback;
end;

procedure TformMenu.imgMenuClick(Sender: TObject);
begin
  Self.ToggleMenu;
end;

procedure TformMenu.setLogoutCallback(aCallback: TLogoutCallback);
begin
  Self.logoutCallback := aCallback;
end;

procedure TformMenu.ToggleMenu;
begin
  if Self.isMenuOpen then begin
    Self.isMenuOpen := False;
    Self.panelMenu.Width := Self.closedWidth;
    Exit;
  end;

  Self.isMenuOpen := True;
  Self.panelMenu.Width := Self.openWidth;
end;

end.

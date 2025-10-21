unit MenuView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, UserView, Vcl.Skia, PermissionsView;

type
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
    procedure imgMenuClick(Sender: TObject);
    procedure buttonUserMenuClick(Sender: TObject);
    procedure buttonPermissionsClick(Sender: TObject);
  private
    openWidth: Integer;
    closedWidth: Integer;
    isMenuOpen: Boolean;
    activeForm: TForm;
    procedure ToggleMenu;
    procedure ChangeForm(aForm: TFormClass);
  public
    constructor Create(AOwner: TComponent; closedWidth: Integer; openWidth: Integer); reintroduce;
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

procedure TformMenu.imgMenuClick(Sender: TObject);
begin
  Self.ToggleMenu;
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

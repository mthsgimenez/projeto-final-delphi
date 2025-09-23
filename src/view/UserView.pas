unit UserView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls, UserController, UserDTO, UserModel, System.Generics.Collections;

type
  TformUser = class(TForm)
    pcontrolUser: TPageControl;
    tabList: TTabSheet;
    tabCreate: TTabSheet;
    gridUsers: TStringGrid;
    buttonCreate: TButton;
    buttonDelete: TButton;
    procedure buttonCreateClick(Sender: TObject);
    procedure tabListShow(Sender: TObject);
  private
    controller: TUserController;
    procedure UpdateGrid(aUsers: TObjectList<TUserModel>);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formUser: TformUser;

implementation

{$R *.dfm}

procedure TformUser.buttonCreateClick(Sender: TObject);
begin
  Self.pcontrolUser.ActivePage := Self.pcontrolUser.Pages[1];
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

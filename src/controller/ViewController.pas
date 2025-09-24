unit ViewController;

interface

uses MainViewInterface, Vcl.Forms;

type TViewController = class
  private
    mainView: IMainView;
    class var instance: TViewController;
    constructor Create(aMainView: IMainView);
  public
    class function GetInstance(aMainView: IMainView): TViewController;
    procedure ChangeForm(aForm: TFormClass);
    procedure OpenForm(aForm: TFormClass);
end;

implementation

{ TViewController }

procedure TViewController.ChangeForm(aForm: TFormClass);
begin
  Self.mainView.ChangeForm(aForm);
end;

constructor TViewController.Create(aMainView: IMainView);
begin
  Self.mainView := aMainView;
end;

class function TViewController.GetInstance(
  aMainView: IMainView): TViewController;
begin
  if instance = nil then
    instance := TViewController.Create(aMainView);

  Result := instance;
end;

procedure TViewController.OpenForm(aForm: TFormClass);
begin
  Self.mainView.OpenForm(aForm);
end;

end.

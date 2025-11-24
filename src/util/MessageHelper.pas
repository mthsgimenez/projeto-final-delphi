unit MessageHelper;

interface

uses Vcl.Dialogs, System.UITypes;

type TMessageHelper = class
  private
    constructor Create;
    class var instance: TMessageHelper;
  public
    class function GetInstance: TMessageHelper;
    procedure Warning(aMessage: String);
    procedure Error(aMessage: String);
    procedure Info(aMessage: String);
    function Confirmation(aMessage: String): Boolean;
end;

implementation

{ TMessageHelper }

function TMessageHelper.Confirmation(aMessage: String): Boolean;
var
  res: Integer;
begin
  res := MessageDlg(
    aMessage,
    mtConfirmation,
    [mbYes, mbNo],
    0);

  Result := res = mrYes;
end;

constructor TMessageHelper.Create;
begin

end;

procedure TMessageHelper.Error(aMessage: String);
begin
  MessageDlg(
    aMessage,
    mtError,
    [mbOk],
    0);
end;

class function TMessageHelper.GetInstance: TMessageHelper;
begin
  if instance = nil then
    instance := TMessageHelper.Create;

  Result := instance;
end;

procedure TMessageHelper.Info(aMessage: String);
begin
  MessageDlg(
    aMessage,
    mtInformation,
    [mbOk],
    0);
end;

procedure TMessageHelper.Warning(aMessage: String);
begin
  MessageDlg(
    aMessage,
    mtWarning,
    [mbOk],
    0);
end;

end.

unit MainViewInterface;

// Interface utilizada para não causar
// referência circular entre MainView e ViewController

interface

uses Vcl.Forms;

type IMainView = interface
  procedure ChangeForm(aForm: TFormClass);
  procedure OpenForm(aForm: TFormClass);
end;

implementation

end.

unit MainViewInterface;

// Interface utilizada para n�o causar
// refer�ncia circular entre MainView e ViewController

interface

uses Vcl.Forms;

type IMainView = interface
  procedure ChangeForm(aForm: TFormClass);
end;

implementation

end.

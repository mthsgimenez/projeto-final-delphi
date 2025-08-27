program MTTools;

uses
  Vcl.Forms,
  LoginView in 'src\views\LoginView.pas' {formLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TformLogin, formLogin);
  Application.Run;
end.

program SyntaxTest;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uSyntaxTest in 'uSyntaxTest.pas',
  UUtils in 'UUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program SyntaxTest;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uSyntaxTest in 'uSyntaxTest.pas',
  LEMMATIZERLib_TLB in 'C:\Program Files\Borland\Delphi7\Imports\LEMMATIZERLib_TLB.pas',
  AGRAMTABLib_TLB in 'C:\Program Files\Borland\Delphi7\Imports\AGRAMTABLib_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

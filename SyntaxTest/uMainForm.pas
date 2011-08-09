unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses uSyntaxTest;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  memo2.Lines.Text:= TransformString( memo1.Lines.Text);
end;

end.

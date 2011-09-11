unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, xmldom, XMLIntf, msxmldom, XMLDoc;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    XMLDocument1: TXMLDocument;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
var
  log:TStringList;
begin
  log:=TStringList.Create;
  ProcessUserInput( memo1.Lines.Text,log);
  memo2.Lines.Text:=log.Text;
  log.Free;
  Label1.Caption:=IntToStr(ElementCount)+'/'+IntToStr(PatternCount);
end;

procedure TForm1.Button2Click(Sender: TObject);
//var
  //log:TStringList;
begin
  //log:=TStringList.Create;
  //memo2.Lines.Text:= TestAIML( 'd:\OpenAI\_SRC\WebAlice\Zett.aiml\1.aiml');
  memo2.Lines.Text:=TestTextFile( 'C:\Users\Zkir\Desktop\phrases.rpt',
                                  'C:\Users\Zkir\Desktop\phrases.2l.rpt' );
  //memo2.Lines.Text:=log.Text;
  //log.Free;
end;

end.

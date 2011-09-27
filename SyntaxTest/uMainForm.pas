unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, xmldom, XMLIntf, msxmldom, XMLDoc, ComCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function TestTextFile(SourceFileName,ResultFileName:String):String;
  end;

var
  Form1: TForm1;

implementation

uses uSyntaxTest;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  log:TStringList;
  t1,t2:TDateTime;
  AA:TSyntaxTransform;
begin
  t1:=Time;
  log:=TStringList.Create;
  AA:=TSyntaxTransform.Create ;
  AA.ProcessUserInput( memo1.Lines.Text,log);
  memo2.Lines.Text:=log.Text;
  log.Free;
  AA.Free;
  t2:=Time;
  memo2.Lines.Insert(0, TimeToStr(t2-t1) );
  memo2.Lines.Insert(1, FloatToStr(24*60*60*(t2-t1)/1) );
  Label1.Caption:=IntToStr(g_WordformCount)+'/'+IntToStr(g_ElementCount)+'/'+IntToStr(g_PatternCount);
end;

procedure TForm1.Button2Click(Sender: TObject);
//var
  //log:TStringList;
  var l:string;
begin
  //log:=TStringList.Create;
  //memo2.Lines.Text:= TestAIML( 'd:\OpenAI\_SRC\WebAlice\Zett.aiml\1.aiml');
  l:='3w';
  memo2.Lines.Text:=TestTextFile( 'd:\OpenAI\_SRC\phrases.'+l+'.txt',
                                  'd:\OpenAI\_SRC\phrases.'+l+'.test.txt' );
  //memo2.Lines.Text:=log.Text;
  //log.Free;
end;


function TForm1.TestTextFile(SourceFileName,ResultFileName:String):String;
var
  CurrentFile : TStringList;
  NewFile: TStringList;
  TextFile : TStringList;
  i:integer;
  SourcePhrase,ResultPhrase:String;
  t1,t2:TDateTime;
  AA:TSyntaxTransform;
begin
  CurrentFile := TStringList.Create;
  NewFile := TStringList.Create;

  TextFile:= TStringList.Create ;
  TextFile.LoadFromFile(SourceFileName);

  ProgressBar1.Min:=0;
  ProgressBar1.Max:=TextFile.Count-1;
  t1:=Time;
  AA:=TSyntaxTransform.Create ;
  for i := 0 to TextFile.Count-1  do
    begin
      SourcePhrase:=TextFile[i];
      CurrentFile.Add(SourcePhrase);
      ResultPhrase:=AA.ProcessUserInput(TextFile[i],CurrentFile);
      CurrentFile.Add('');
      NewFile.Add(SourcePhrase+' --> '+ResultPhrase);
      ProgressBar1.Position:=i;
      Application.DoApplicationIdle;
    end;

  NewFile.SaveToFile(ResultFileName);
  AA.Free;
  t2:=Time;
  CurrentFile.Insert(0, TimeToStr(t2-t1) );
  CurrentFile.Insert(1, FloatToStr(24*60*60/TextFile.Count*(t2-t1)) );

  result:=   CurrentFile.Text;
  TextFile.Free;
  NewFile.Free;

end;


end.

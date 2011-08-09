unit uSyntaxTest;

interface
uses Classes;

Function TransformString(strInput: String): String;


implementation
uses  StrUtils, SysUtils,ActiveX,
LEMMATIZERLib_TLB,AGRAMTABLib_TLB;

type
 //  словоформа
   TWordForm = class
    private
      function GetNumberOfVariants:integer;
    public
      WordForm:String;//словоформа, которая стоит во фразе
      //исходных форм, а следовательно и граммем
      //(наборов возможных грамматических признаков) может быть несколько

      PartOfSpeach:TStringList;//часть речи
      Grammems:TStringList; // граммемы, в строковом виде
      Lemma:TStringList; //исходная форма слова, лемма

      property NumberOfVariants:integer read GetNumberOfVariants;
      constructor Create(const aWordForm: String);
  end;

  //Лемматизированная фраза. Представляет просто список входящих в нее словоформ
  TPhrase = TList;

  //Элемент синтаксического шаблона.
  //Довольно очевидно, что это не что иное как "фразовая категория"
  //ключевое отличие элемента шаблона от словоформы в том что он обладает
  // ЕДИНСТВЕННЫМ набором граммем.
  TSyntaxPatternElement = class
      PartOfSpeach:String;//часть речи
      Grammems:TStringList; // граммемы, в строковом виде, разделенные
      WordForm:String;//словоформа, которая может стоять в шаблоне (а может и не стоять)

      //Заматченные элементы
      MatchedPartOfSpeach:String;//часть речи
      MatchedGrammems:String; // граммемы, через запятую
      MatchedWordForm:String;//словоформа
      MatchedLemma:String; // лемма

      function CompareGrammems(strGrammems:String): boolean;//сравнение шаблона граммем с данными

      constructor Create(const strWordForm,strPartOfSpeach,strGrammems: String);
  end;

  //тоже, очевидно, список (последовательность) элементов "слов-пустышек"
  TSyntaxPattern =class
    private
      FPatternElements:TList;
      function GetElementCount:integer;
      function GetElement(Index : Integer):TSyntaxPatternElement;
    public
      TrasformationFormula:string;
      property ElementCount:integer read GetElementCount;
      property Elements[Index : Integer]:TSyntaxPatternElement  read GetElement;  default;
      function AddElement(const strWordForm,strPartOfSpeach,strGrammems: String):integer; //Добавление элемента в шаблон

      //Тестирование  фразы (Phrase), на соответствие синтаксическому шаблону (Pattern).
      function TestPhrase(Phrase:TPhrase;
                          var intMatchedWords, intUnmatchedWords:integer):boolean;

      //Трансформация фразы во "внутреннее" представление
      function ProcessTrasformationFormula():string;
      constructor Create();
  end;

var
  RusLemmatizer : ILemmatizer;
  RusGramTab :    IGramTab;

//Разделение строки на части по разделителю
function Split (str: String; strSeparator:String ):TStringList;
  var t:TStringList;
begin
  t:=TStringList.create; //создаём класс
  t.text:=stringReplace(str,strSeparator,#13#10,[rfReplaceAll]);//мы заменяем все разделители на символы конца строки
  result:= t;
end;
function min(a,b:integer):integer;
begin
  if a< b then
    Result:=a
  else
    Result:=b;
end;


//Элемент синтаксического шаблона
constructor TSyntaxPatternElement.Create(const strWordForm,strPartOfSpeach,strGrammems: String);
begin
  if  strWordForm<>'' then
    begin
      //задано конкретное слово
      WordForm:=strWordForm;
      PartOfSpeach:='';
      Grammems:=nil; //TStringList.Create;
    end
  else
    begin
      WordForm:='*';
      PartOfSpeach:=strPartOfSpeach;
      Grammems:=Split(strGrammems,',') ;
    end;
end;

//сравнение шаблона граммем с данными
function TSyntaxPatternElement.CompareGrammems(strGrammems:String): boolean;
var i:integer;
begin
  result:=true;
  //понимаем под "совпадением" наличие грамеммы в списке
  //возможно это не совсем аккуратно, может быть следует давать отрицательный результат,
  //только если граммема образа не совместима с граммами слова, например если указан ип, а у слова вп
  //а положительный, если у него нет такой граммемы, слова совместимо с любой г.
  // данной категории, например глагол в наст. вр. совместим с любым родом.
   for i:=0 to Grammems.Count-1   do
   begin
     //Мы перебираем граммемы образца (в нем они заданны каждая в отдельном элементе списка)
     if Pos(Grammems[i] +',',strGrammems )=0 then
     begin
       result:=false;
       break;
     end
   end

end;

//TSyntaxPattern  - синтаксический шаблон
constructor TSyntaxPattern.Create();
begin
  FPatternElements:=TList.create;
end;
function TSyntaxPattern.GetElementCount:integer;
begin
  Result:=FPatternElements.Count;
end;

function TSyntaxPattern.GetElement(index:integer):TSyntaxPatternElement;
begin
  Result:=FPatternElements[index];
end ;

function TSyntaxPattern.AddElement(const strWordForm,strPartOfSpeach,strGrammems: String):integer;
var
  aSyntaxPatternElement:TSyntaxPatternElement;
begin
  aSyntaxPatternElement := TSyntaxPatternElement.Create(strWordForm,strPartOfSpeach,strGrammems);
  Result:=FPatternElements.Add(aSyntaxPatternElement);
end;

//Самая веселая операция в данном классе - заматченный шаблон трансформируется
function TSyntaxPattern.ProcessTrasformationFormula():string;
var i: integer;
    tmp: String;
    token:String;
begin
  tmp:=TrasformationFormula;
  for i:=0 to ElementCount-1 do
  begin
    token:='#'+IntToStr(i+1)+'l';
    if Pos(token,tmp)<>0 then
      tmp:=StringReplace (tmp,token, Elements[i].MatchedLemma,[rfReplaceAll]) ;

    token:='#'+IntToStr(i+1);
    if Pos(token,tmp)<>0 then
      tmp:=StringReplace (tmp,token, Elements[i].MatchedWordForm,[rfReplaceAll]) ;
  end;
  Result:= tmp;
end;


//Тестирование  фразы (Phrase), на соответствие синтаксическому шаблону (Pattern).
//Тестирование осуществляется с начала фразы.
//Возвращается
// intMatchedWords   - число сопоставленных слов в начале фразы
// intUnmatchedWords - число не сопоставленных слов в хвосте фразы
// Функция возвращает True, если фраза соответствует шаблону
function TSyntaxPattern.TestPhrase(Phrase:TPhrase;
                                   var intMatchedWords, intUnmatchedWords:integer):boolean;
var i,j:integer;
    PE:TSyntaxPatternElement ;
    WF: TWordForm;
    blnMatched:boolean;
begin
  intMatchedWords := 0;

  //идти в цикле по словам фразы и сверять грамеммы. Если несовпадение - выйти.
  for i:=0 to min( self.ElementCount-1, Phrase.Count-1)    do
  begin
    PE:=self[i];
    //Теперь перебираем варианты для словоформой с тем же номером
    WF:=Phrase[i];
    blnMatched:=False;
    for j:=0 to WF.NumberOfVariants-1  do
    begin
      if PE.WordForm<>'*' then
      begin
      //конкретное слово
        if PE.WordForm<>WF.WordForm then
          continue;//соответствия не найдено, нужно перейти к следующему варианту
      end
      else
      begin
        //Часть речи
        if PE.PartOfSpeach <> WF.PartOfSpeach[j]  then
          continue;//соответствия не найдено, нужно перейти к следующему варианту словоформы.

        //граммемы
        if not PE.CompareGrammems(WF.Grammems[j]) then
          continue;//соответствия не найдено, нужно перейти к следующему варианту словоформы.
      end;

      blnMatched:=True;
      //соответствие найдено, нужно перейти к следующему слову
      //(омонимией пренебрегаем. Если фраза соответствует шаблону, берем первый вариант)

      PE.MatchedWordForm:= WF.WordForm;
      PE.MatchedPartOfSpeach:= WF.PartOfSpeach[j]  ;
      PE.MatchedGrammems:= WF.Grammems[j]  ;
      PE.MatchedLemma:= WF.Lemma[j] ;
      break;

    end;
    if blnMatched then
      intMatchedWords:=intMatchedWords + 1
    else
    begin
      //Данная словоформа НЕ соответствует шаблону. поиск закончен
      break;

    end;
  end;
  Result := (self.ElementCount = intMatchedWords);
  intUnmatchedWords := Phrase.Count-intMatchedWords;
end;



//Лемматизация
constructor TWordForm.Create(const aWordForm: String);
var
     ParadigmCollection : IParadigmCollection;
     Paradigm : IParadigm;
     OneAncode, SrcAncodes : string;
     i,j : integer;
begin
  WordForm:=aWordForm;
  Lemma:=TStringList.Create;
  Grammems:=TStringList.Create;
  PartOfSpeach:= TStringList.Create;
  ParadigmCollection := RusLemmatizer.CreateParadigmCollectionFromForm(aWordForm, 1, 1);
  if (ParadigmCollection.Count = 0) then
  begin
    writeln('not found');
    exit;
  end;

  for j:=0 to ParadigmCollection.Count-1 do
  begin
    Paradigm := ParadigmCollection.Item[j];
    i:=1;

    SrcAncodes := Paradigm.SrcAncode;
    while  i < Length(SrcAncodes) do
    begin
      OneAncode := Copy(SrcAncodes,i,2);
      Lemma.Add ( Paradigm.Norm);
      PartOfSpeach.Add(RusGramTab.GetPartOfSpeechStr( RusGramTab.GetPartOfSpeech(OneAncode)));
      Grammems.Add(RusGramTab.GrammemsToStr( RusGramTab.GetGrammems(OneAncode) ));
      inc (i, 2);
    end;
  end;
end ;

function TWordForm.GetNumberOfVariants():integer;
begin
  result:=PartOfSpeach.Count;
end;



//' Наша задача - разделить текст (ввод пользователя) на предложения (клаузы)
//' Будем пытаться распознать во входной строке наперед заданные синтаксические шаблоны
//' каждый отдельный найденный шаблон будем считатать клаузой
//'
//' Требуется найти наиболее "жадное" разбиение, т.е. рабиение с *наименьшим* числом клауз
//' (не исключено, что потребуется "поиск и возврат")
//'
//' кроме того, нам нужно привести клаузы во внутреннее представление, с левым развертыванием
//' (зависимые элементы предшествуют главным (как в японском ^ ^) )
//
// Сверхидея - разделить текст на клаузы, а сами клаузы привести к левому развертыванию (со строгим порядком)
// чтобы можно было сравнивать с образцами, не парясь порядком слов.

//  1. на первом этапе нужно разбить "текст" на слова
//  2. на втором этапе нужно присвоить словам (словоформам) грамматические
//     признаки.  для этого будем использовать компонент от AOT, он умеет это делать.
//  3. на третьем этапе нужно "узнавать синтаксические шаблоны во входной строке"



//2. Лемматизация, присваивание граммем  (грамматических признаков)
// даже если одна словоформа опознается как принадлежащая нескольким исходным словам,
// (например, "мыла" <- мыть|мыло, они запоминаются все, узнаванию синтаксических структур
// это мешать не должно.
function Lemmatize(WordList:TStringList):TPhrase;
var i:integer;
    Phrase: TPhrase;

begin
  Phrase:= TPhrase.create;
  for i:=0 to WordList.Count-1 do
  begin

    Phrase.Add(TWordForm.Create(WordList[i]) );
  end;
  result:=Phrase;
end;

function SyntaxAnalysis(Phrase: TPhrase; var intMatchedWords, intUnmatchedWords:integer): TSyntaxPattern;
var
  Pattern,Pattern2,Pattern3: TSyntaxPattern;
  PatternList: TList;
  i:integer;
begin
  //Cоздаем некий список шаблонов, которые мы будем проверять
  PatternList:=  TList.Create;
  //Мама моет раму
  Pattern:= TSyntaxPattern.Create;
  Pattern.AddElement( '','С','им' );
  Pattern.AddElement( '','Г','' );
  Pattern.AddElement( '','С','вн' );
  Pattern.TrasformationFormula:='#1l [кто] #3l [кого] #2 [.]';
  PatternList.Add(Pattern);

   // раму  моет Мама
  Pattern2:= TSyntaxPattern.Create;

  Pattern2.AddElement( '','С','вн' );
  Pattern2.AddElement( '','Г','' );
  Pattern2.AddElement( '','С','им' );

  Pattern2.TrasformationFormula:='#3l [кто] #1l [кого] #2 [.]';
  PatternList.Add(Pattern2);

  //Обновите пожалуйста Молдову
  Pattern3:= TSyntaxPattern.Create;

  Pattern3.AddElement( '','Г','пвл' );
  Pattern3.AddElement( 'пожалуйста','','' );
  Pattern3.AddElement( '','С','вн' );

  Pattern3.TrasformationFormula:='#2 [.] #3l [кого] #1 [.]';
  PatternList.Add(Pattern3);


  //Проверка в цикле, начиная от наиболее специфичных
  Result:=nil;
  //Я хочу потестировать фразу на соответствие некому синтаксическому шаблону.
  for i:=0 to PatternList.Count-1 do
  begin
    if TSyntaxPattern(PatternList[i]).TestPhrase(Phrase, intMatchedWords, intUnmatchedWords) then
    begin
      //Cоответствие найдено
      Result:= PatternList[i];
      break;
    end
  end;

  PatternList.Free;
end;

//' Главная функция
//' strInput - входная строка, предполагается что она "токенизирована"
// (из нее удалены пробелы и спец. символы)
Function TransformString(strInput: String): String;
  var SplittedPhrase:TStringList;
      i,j:integer;
      Phrase: TPhrase;

      MatchedPattern: TSyntaxPattern;
      log : TStringList;
      tmp: String;
      intMatchedWords, intUnmatchedWords:integer;
begin


  log:=TStringList.Create;

  // 1. разбиение на слова
  SplittedPhrase:= Split(trim(strInput),' ');

//2. присвоение словоформам грамматических признаков
// даже если одна словоформа опознается как принадлежащая нескольким исходным словам,
// (например, "мыла" <- мыть|мыло, они запоминаются все, узнаванию синтаксических структур
// это мешать не должно.
//На этом этапе нам нужно получить некую структуру (Phrase), в которой будут содержаться
//слова исходной фразы, вместе с соответствующими им грамеммами (грамматическими признаками )
// и леммами (исходными формами)

  Phrase:=Lemmatize(SplittedPhrase);


  //3.
  //Синтаксический разбор. На этом этапе мы должны получить структуру данных, которая
  //отражает синтаксическую структуру предложения и содержит слова с приписанными
  //синтаксическими признаками (зависимостями?)

  //на текущий момент такой структурой является заматченный шаблон.

   MatchedPattern:=SyntaxAnalysis(Phrase, intMatchedWords, intUnmatchedWords);

  //4. Трансформация
   if MatchedPattern <> nil then
     log.Add(MatchedPattern.ProcessTrasformationFormula);
   log.Add( IntToStr( intMatchedWords) +'/'+  IntToStr( intUnmatchedWords));

  //вывод того что получилось.

  result:='';
  for i:=0 to Phrase.Count-1 do
  begin
    log.add(TWordForm(Phrase[i]).WordForm+':');
    tmp:='';
    tmp:=tmp+(' {');
    for j:=0 to TWordForm(Phrase[i]).Lemma.Count-1 do
    begin
      tmp:=tmp+(' '+TWordForm(Phrase[i]).Lemma[j]);
      tmp:=tmp+(' '+TWordForm(Phrase[i]).PartOfSpeach [j]);
      tmp:=tmp+(' '+TWordForm(Phrase[i]).Grammems[j]);
      tmp:=tmp+(';')
    end;
    tmp:=tmp+' }';
    log.add(tmp);
  end;
  result:=Log.Text;
  SplittedPhrase.Free;
  Phrase.Free;
  log.Free;
  MatchedPattern.Free;
End;


//Инициализация  лемматизатора.
var   hr :  HRESULT;
initialization
try
    hr := CoInitialize(nil);
    if (hr <> S_OK) then
    begin
       // writeln('cannot load Component Object Model(COM) library');
        halt(1);
    end;
     // loading morphological dicitonary
    RusLemmatizer := CoLemmatizerRussian.Create;
    if  (RusLemmatizer = nil) then
    begin
        //writeln('cannot load lemmatizer');
        halt(1);
    end;
    RusLemmatizer.LoadDictionariesRegistry();
    // loading table of gram-codes
    RusGramTab := CoRusGramTab.Create;
    if  (RusGramTab = nil) then
    begin
        writeln('cannot load table for grammatical codes');
        halt(1);
    end;
    RusGramTab.Load;


except
   // writeln('an exception occurred!');
end;

finalization
  CoUninitialize();
end.

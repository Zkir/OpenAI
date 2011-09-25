unit uSyntaxTest;

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

interface
uses Classes,IniFiles;

//Function TransformString(strInput: String; log:TStringList): String;
function ProcessUserInput(strInput: String; log:TStringList):String;
function TestAIML(FileName:String):String;

var
  g_WordformCount:integer;
  g_ElementCount:integer;
  g_PatternCount:integer;
implementation
uses  StrUtils, SysUtils,ActiveX,  LibXMLParser,  XMLDoc, XMLIntf,  UUtils,
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
      destructor Destroy; Override;
  end;

  //Лемматизированная фраза. Представляет просто список входящих в нее словоформ
  TPhrase = TObjectList;

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

      function isTerminalElement:boolean; //Является ли данный элемент терминальным,
      //или же "фразовой категорией"
      function CompareGrammems(strGrammems:String): boolean;overload;//сравнение шаблона граммем с данными
      function CompareGrammems(lstGrammems:TStringList): boolean; overload;

      constructor Create(const strWordForm,strPartOfSpeach,strGrammems: String);
      destructor Destroy; Override;
  end;

  //тоже, очевидно, список (последовательность) элементов "слов-пустышек"
  // т.е. элементов, которым назначены части речи и грамеммы, а лемма может быть любой
  TSyntaxPattern=class
    private
      FPatternElements:TList;
      FRootElement:TSyntaxPatternElement;
      FVariables:THashedStringList;
      function GetElementCount:integer;
      function GetElement(Index : Integer):TSyntaxPatternElement;
    public
      TrasformationFormula:string;
      property ElementCount:integer read GetElementCount;
      property Elements[Index : Integer]:TSyntaxPatternElement  read GetElement;  default;
      function ExplicitWordformCount:integer;
      //Добавление элемента в шаблон
      function AddElement(const strWordForm,strPartOfSpeach,strGrammems: String):integer;
      //Задание корневого элемента
      function SetRootElement(const strPartOfSpeach,strGrammems: String):integer;

      //Тестирование  фразы (Phrase), на соответствие синтаксическому шаблону (Pattern).
      function TestPhrase(Phrase:TPhrase;
                          var intMatchedWords, intUnmatchedWords:integer):boolean;
      function SetVariableValue(VarName,VarValue:String):boolean;
      //Трансформация фразы во "внутреннее" представление
      function ProcessTrasformationFormula():string;
      constructor Create();
      constructor CreateCopy(Source:TSyntaxPattern);
      destructor Destroy; override;
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
  inherited Create;
  inc(g_ElementCount);
  if  (strWordForm<>'')and (strWordForm<>'*') then
    begin
      //задано конкретное слово
      WordForm:=strWordForm;
      PartOfSpeach:='';
      Grammems:=TStringList.Create;   // nil;
    end
  else
    begin
      WordForm:='*';
      PartOfSpeach:=strPartOfSpeach;
      Grammems:=Split(strGrammems,',') ;
    end;
end;

destructor TSyntaxPatternElement.Destroy;
begin
  Grammems.Free;
  dec(g_ElementCount);
  inherited Destroy;
end;

//сравнение шаблона граммем с данными
//понимаем под "совпадением" наличие грамеммы в списке
//Все грамеммы данного шаблона должны присутствовать в эталоне, чтобы было защитано совпадение
  //возможно это не совсем аккуратно, может быть следует давать отрицательный результат,
  //только если граммема образа не совместима с граммами слова, например если указан ип,
  //а у слова вп
  //а положительный, если у него нет такой граммемы, слова совместимо с любой г.
  // данной категории, например глагол в наст. вр. совместим с любым родом.
function TSyntaxPatternElement.CompareGrammems(strGrammems:String): boolean;
var i:integer;
begin
  result:=true;
  for i:=0 to Grammems.Count-1   do
  begin
    //Мы перебираем граммемы образца (в нем они заданны каждая в отдельном элементе списка)
    if  Pos(Grammems[i] +',',strGrammems )=0 then
    begin
      result:=false;
      break;
    end
  end
end;

//То же самое, только граммемы в виде списка
function TSyntaxPatternElement.CompareGrammems(lstGrammems:TStringList): boolean;
var i:integer;
begin
  result:=true;
  for i:=0 to Grammems.Count-1   do
  begin
    //Мы перебираем граммемы образца (в нем они заданны каждая в отдельном элементе списка)
    if lstGrammems.IndexOf(Grammems[i])=-1 then
    begin
      result:=false;
      break;
    end;
  end;
end;

function TSyntaxPatternElement.isTerminalElement:boolean;
begin
  Result:=not ((PartOfSpeach='Иг')or (PartOfSpeach='Гг') );
end;

//TSyntaxPattern  - синтаксический шаблон
constructor TSyntaxPattern.Create();
begin
  FPatternElements:=TList.Create;
  FRootElement:=nil;
  FVariables:=THashedStringList.Create;
  inc(g_PatternCount);
end;
constructor TSyntaxPattern.CreateCopy(Source:TSyntaxPattern);
var i:integer;
begin
  FPatternElements:=TList.Create;
  FVariables:=THashedStringList.Create;
  FRootElement:=nil;
  inc(g_PatternCount);
  //Копируем содержимое
  TrasformationFormula:= Source.TrasformationFormula;
  if Source.FRootElement<>nil then
    SetRootElement(Source.FRootElement.PartOfSpeach, Source.FRootElement.Grammems.Text);

  for i:= 0 to Source.ElementCount-1 do
    AddElement(Source[i].WordForm, Source[i].PartOfSpeach, Source[i].Grammems.text);

  FVariables.Text:=Source.FVariables.Text;
end;

destructor TSyntaxPattern.Destroy();
var i:integer;
begin
  //Освободим корневой элемент данного шаблона
  FRootElement.Free;
  //Освободим входящие в данный шаблон элементы
  for i := 0 to FPatternElements.Count-1  do
    TSyntaxPatternElement(FPatternElements[i]).Free;
  FPatternElements.free;
  FVariables.Free;
  dec(g_PatternCount);
end;

function TSyntaxPattern.GetElementCount:integer;
begin
  Result:=FPatternElements.Count;
end;

function TSyntaxPattern.ExplicitWordformCount:integer;
var i:integer;
begin
  Result:=0;
  for i := 0 to ElementCount-1 do
  begin
    if Elements[i].WordForm<>'*' then
      inc(Result);
  end;
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

function TSyntaxPattern.SetRootElement(const strPartOfSpeach,strGrammems: String):integer;
begin
  FRootElement:=TSyntaxPatternElement.Create('',strPartOfSpeach,strGrammems);
  Result:=0;
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
      tmp:=StringReplace (tmp,token, AnsiUpperCase( Elements[i].MatchedWordForm),[rfReplaceAll]) ;
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
    WF:=TWordForm(Phrase[i]);
    blnMatched:=False;
    for j:=0 to WF.NumberOfVariants-1  do
    begin
      if PE.WordForm<>'*' then
      begin
      //конкретное слово
        if AnsiUpperCase(PE.WordForm)<>AnsiUpperCase(WF.WordForm) then
          continue;//соответствия не найдено, нужно перейти к следующему варианту
      end
      else
      begin
        //Часть речи
        if (PE.PartOfSpeach<>'*') and (PE.PartOfSpeach <> WF.PartOfSpeach[j])  then
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

function TSyntaxPattern.SetVariableValue(VarName,VarValue:String):boolean;
var
  i,j: Integer;
  procedure RemoveGrammem(Grammems:TStringList;value:String);
    var intIndex:integer;
  begin
    intIndex:= Grammems.IndexOf(value);
    if intIndex <>-1 then
    begin
      Grammems.delete(intIndex );
    end;

  end;
begin
  //первым делом удалим переменную из списка
  FVariables.Delete(FVariables.IndexOfName(VarName));
  //Заменяем переменную  на значение во всех элементах
  for i := 0 to ElementCount-1 do
  begin

    for j := 0 to Elements[i].Grammems.Count-1 do
      Elements[i].Grammems[j]:=ReplaceStr(Elements[i].Grammems[j],VarName,VarValue);

    //Следует проверить, совместимо ли значение переменной с данной частью речи
    //и другими грамеммами
    if Elements[i].PartOfSpeach = 'С' then
      begin
        RemoveGrammem(Elements[i].Grammems,'1л');
        RemoveGrammem(Elements[i].Grammems,'2л');
        RemoveGrammem(Elements[i].Grammems,'3л');
      end;
    if Elements[i].PartOfSpeach = 'Г' then
    begin
      if Elements[i].Grammems.IndexOf('прш')<> -1 then
        begin
          RemoveGrammem(Elements[i].Grammems,'1л');
          RemoveGrammem(Elements[i].Grammems,'2л');
          RemoveGrammem(Elements[i].Grammems,'3л');
        end;
      if (Elements[i].Grammems.IndexOf('нст')<> -1) or
         (Elements[i].Grammems.IndexOf('буд')<> -1) or
         (Elements[i].Grammems.IndexOf('мн')<> -1) then
        begin
          RemoveGrammem(Elements[i].Grammems,'мр');
          RemoveGrammem(Elements[i].Grammems,'жр');
          RemoveGrammem(Elements[i].Grammems,'ср');
        end;
    end;
    //Прилагательные во множественном числе лишины рода
    if (Elements[i].PartOfSpeach = 'П') or
       (Elements[i].PartOfSpeach = 'МС-П') or
       (Elements[i].PartOfSpeach = 'КР_ПРИЛ')  then
    begin
      if (Elements[i].Grammems.IndexOf('мн')<> -1) then
        begin
          RemoveGrammem(Elements[i].Grammems,'мр');
          RemoveGrammem(Elements[i].Grammems,'жр');
          RemoveGrammem(Elements[i].Grammems,'ср');
        end;
    end;
  end;
  //и в том числе в корневом элементе, если есть.
  if FRootElement<>nil then
    for j := 0 to  FRootElement.Grammems.Count-1 do
      FRootElement.Grammems[j]:=ReplaceStr(FRootElement.Grammems[j],VarName,VarValue);

end;

//Лемматизация
constructor TWordForm.Create(const aWordForm: String);
var
     ParadigmCollection : IParadigmCollection;
     Paradigm : IParadigm;
     OneAncode, SrcAncodes : string;
     i,j : integer;
     strLemma,strPartOfSpeach:string;
begin
  inc(g_WordformCount);
  WordForm:=aWordForm;
  Lemma:=TStringList.Create;
  Grammems:=TStringList.Create;
  PartOfSpeach:= TStringList.Create;
  ParadigmCollection := RusLemmatizer.CreateParadigmCollectionFromForm(aWordForm, 1, 1);
  if (ParadigmCollection.Count = 0) then
  begin
    Raise Exception.Create('Не найдена парадигма для слова '+aWordForm);
  end;

  for j:=0 to ParadigmCollection.Count-1 do
  begin
    Paradigm:=nil;
    Paradigm := ParadigmCollection.Item[j];
    i:=1;

    SrcAncodes := Paradigm.SrcAncode;
    while  i < Length(SrcAncodes) do
    begin
      OneAncode := Copy(SrcAncodes,i,2);
      strLemma:=Paradigm.Norm;
      strPartOfSpeach:=RusGramTab.GetPartOfSpeechStr( RusGramTab.GetPartOfSpeech(OneAncode));
      if not((strLemma='ЛИ')and (strPartOfSpeach='С'))
      and not((strLemma='МЕНЬ')and (strPartOfSpeach='С')) then
      begin
        Lemma.Add(strLemma);
        PartOfSpeach.Add(strPartOfSpeach);
        Grammems.Add(RusGramTab.GrammemsToStr( RusGramTab.GetGrammems(OneAncode) ));
      end;
      inc (i, 2);
    end;
  end;
  ParadigmCollection:=nil;
  Paradigm:=nil;
end ;

destructor TWordForm.Destroy();
begin
  dec(g_WordformCount);
  PartOfSpeach.Free;
  Grammems.Free;
  Lemma.Free;
  inherited;
end;

function TWordForm.GetNumberOfVariants():integer;
begin
  result:=PartOfSpeach.Count;
end;


//2. Лемматизация, присваивание граммем  (грамматических признаков)
// даже если одна словоформа опознается как принадлежащая нескольким исходным словам,
// (например, "мыла" <- мыть|мыло, они запоминаются все, узнаванию синтаксических структур
// это мешать не должно.
function Lemmatize(WordList:TStringList):TPhrase;
var i:integer;
    Phrase: TPhrase;

begin
  Phrase:= TPhrase.create;
  Phrase.OwnsObjects:= true;
  for i:=0 to WordList.Count-1 do
  begin

    Phrase.Add(TWordForm.Create(WordList[i]) );
  end;
  result:=Phrase;
end;

//Cобственно, список шаблонов, с которыми мы будем сравнивать нашу фразу.
//Откуда получать этот список, из файла, или из "порождающей грамматики",
//не столь важно.
// Как сократить его объем, предстоит подумать.
// Этот список должен быть упорядочен по "специфичности", самые длинные в начале.

//Сравнение цепочек  по специфичности.
// должно учитывается
//  1 - число элементов.
//  2 - число явно заданных словоформ
//  3 - полную фразу ([.]) следует предпочесть неполной ([...])
function ComparePatternLength(Item1, Item2: Pointer): Integer;
var Pattern1,Pattern2:TSyntaxPattern;
begin
  Pattern1:=TSyntaxPattern(Item1);
  Pattern2:=TSyntaxPattern(Item2);

  //сперва проверим длинну
  Result :=0;
  if Pattern1.ElementCount> Pattern2.ElementCount  then
    Result :=-1;
  if Pattern1.ElementCount< Pattern2.ElementCount  then
    Result :=1;

  //проверим число явно заданных словоформ
  if Result=0 then
  begin
    if Pattern1.ExplicitWordformCount> Pattern2.ExplicitWordformCount  then
      Result :=-1;
    if Pattern1.ExplicitWordformCount< Pattern2.ExplicitWordformCount  then
      Result :=1;
  end;

  if Result=0 then
  begin
    //Проверим полноту
    if (Pos('[.]',Pattern1.TrasformationFormula)<>0) and (Pos('[.]',Pattern2.TrasformationFormula)=0)   then
      Result :=-1;
    if (Pos('[.]',Pattern1.TrasformationFormula)=0) and (Pos('[.]',Pattern2.TrasformationFormula)<>0)  then
      Result :=1;
  end;
  Pattern1:=nil;
  Pattern2:=nil;
end;

//Получаем все правила - фразы и фразовые категории
function GetAllRuleList: TObjectList;
var
  xml:IXMLDocument;
  PhraseNode,ElementNode,VarNode:IXMLNode;
  PatternList: TObjectList;
  Pattern: TSyntaxPattern;
  i,j:integer;
  strWord,strPartOfSpeach,strGrammems,Formula:string;
  varName,varValue:string;
begin
  PatternList:=TObjectList.Create;
  PatternList.OwnsObjects:=True;

  xml:=LoadXMLDocument('d:\OpenAI\_SRC\grammar.xml');//
 // xml.LoadFromFile();
  xml.Active:=True;

  for i := 0 to xml.DocumentElement.ChildNodes.Count-1 do
  if (xml.DocumentElement.ChildNodes[i].NodeName='Phrase') or
     (xml.DocumentElement.ChildNodes[i].NodeName='PhraseCategory') then
  begin
    Pattern:= TSyntaxPattern.Create;
    PhraseNode:=xml.DocumentElement.ChildNodes[i];
    strWord:= PhraseNode.NodeName;
    for j := 0 to PhraseNode.ChildNodes['Pattern'].ChildNodes.Count-1 do
    begin
      ElementNode:=PhraseNode.ChildNodes['Pattern'].ChildNodes[j];
      strWord:= ElementNode.ChildNodes['WordForm'].Text ;
      strPartOfSpeach:= ElementNode.ChildNodes['Category'].Text ;
      strGrammems:=ElementNode.ChildNodes['Grammems'].Text ;
      Pattern.AddElement( strWord,strPartOfSpeach,strGrammems );
    end;

    Pattern.TrasformationFormula:=PhraseNode.ChildNodes['TrasformationFormula'].Text;
    if (PhraseNode.NodeName='PhraseCategory') then
    begin
      ElementNode:=PhraseNode.ChildNodes['Def'];
      strPartOfSpeach:= ElementNode.ChildNodes['Category'].Text ;
      strGrammems:=ElementNode.ChildNodes['Grammems'].Text ;
      Pattern.SetRootElement (strPartOfSpeach,strGrammems );
    end;
    //Разбор переменных
    for j := 0 to PhraseNode.ChildNodes['Vars'].ChildNodes.Count-1 do
    begin
      VarNode:=PhraseNode.ChildNodes['Vars'].ChildNodes[j];
      varName:=VarNode.Attributes['Name'];
      varValue:=VarNode.Attributes['Values'];
      Pattern.FVariables.Add(varName +'='+varValue  );
    end;


    PatternList.Add(Pattern);
  end;

  Result :=PatternList;
  xml:=nil;
  PhraseNode:=nil;
  ElementNode:=nil;
  VarNode:=nil;
 end;

//выборка элементов, соответсвующих "правой части" правила (RightElement).
//для фразы ("S") RightElement - nil

function GetRuleListSubset(RightElement:TSyntaxPatternElement; AllRules:TList): TObjectList;
var i:integer;
    PE:TSyntaxPatternElement;
begin
  Result:= TObjectList.Create();
  Result.OwnsObjects:=True;
  //Получаем список шаблонов соответствующих заданной фразовой категории
  for i:=0 to AllRules.Count-1 do
  begin
    PE:=TSyntaxPattern(AllRules[i]).FRootElement;
    if RightElement<>nil then
      begin
        if PE<>nil then
          if (PE.PartOfSpeach=RightElement.PartOfSpeach) and (RightElement.CompareGrammems( PE.Grammems))  then
            //В список добавляется копия(!) исходного элемента
            Result.Add(TSyntaxPattern.CreateCopy(AllRules[i]));
      end
    else
      begin
        if PE=nil then
          //В список добавляется копия(!) исходного элемента
          Result.Add(TSyntaxPattern.CreateCopy(AllRules[i]));
      end;
  end;
end;

//Формулу саму надо трансформировать. На место j-го элемента нужно поставить формулу
//Распространенного элемента, а то что справа - сдвинуть.
// strFormula  - формула фразы
// strElFormula - формула замененного элемента
// j - номер замененного элемента
function ModifyTransformationFormula(strFormula, strElFormula:String;N,M,j:integer):String;
var //lstFormula:TStringList;
    i:integer;
begin
  if strElFormula='#2-НЕ' then
    i:=0;

  //lstFormula:=Split(strFormula,' ');
  for i := N downto j+1 do
  begin
    strFormula:=StringReplace(strFormula, '#'+IntToStr(i)+' ', '#'+IntToStr(i+M-1)+' ', [rfReplaceAll]) ;
    strFormula:=StringReplace(strFormula, '#'+IntToStr(i)+'-', '#'+IntToStr(i+M-1)+'-', [rfReplaceAll]) ;
    strFormula:=StringReplace(strFormula, '#'+IntToStr(i)+'l', '#'+IntToStr(i+M-1)+'l', [rfReplaceAll]) ;
  end;

  strElFormula:=strElFormula+' ';
  //в распространенном элементе 1 становится j, поскольку вставляется в j-ю позицию
  for i := M downto 1 do
  begin
    strElFormula:=StringReplace(strElFormula, '#'+IntToStr(i)+' ', '#'+IntToStr(j+i-1)+' ', [rfReplaceAll]) ;
    strElFormula:=StringReplace(strElFormula, '#'+IntToStr(i)+'-', '#'+IntToStr(j+i-1)+'-', [rfReplaceAll]) ;
    strElFormula:=StringReplace(strElFormula, '#'+IntToStr(i)+'l', '#'+IntToStr(j+i-1)+'l', [rfReplaceAll]) ;
  end;
  strElFormula:=trim(strElFormula);

  strFormula:=StringReplace(strFormula, '#'+IntToStr(j), strElFormula,[rfReplaceAll]) ;
  result:=strFormula;


  //result:=Join(lstFormula,' ');
end;

Procedure ExpandVariables(PatternList: TObjectList);
var
  VarName:String;
  VarValue:TStringList;
  blnNonTerminalElementsExist:boolean;
  NewPattern,CurrentPattern: TSyntaxPattern;
  i,j:integer;
begin
  //Нужно конкретизировать переменные
  repeat
  blnNonTerminalElementsExist:=False;
  for i:=0 to PatternList.Count-1 do
  begin
    CurrentPattern:=TSyntaxPattern(PatternList[i]);
    if CurrentPattern.FVariables.Count<>0   then
    begin
      blnNonTerminalElementsExist:=True;
      //берем первую переменную
      //Для каждого значения переменной
       //создаем новый шаблон
       VarName:=CurrentPattern.FVariables.Names[0];
       VarValue:=Split(CurrentPattern.FVariables.Values[VarName],',');
       for j := 0 to VarValue.Count-1 do
       begin
         NewPattern:=TSyntaxPattern.CreateCopy(CurrentPattern);
         NewPattern.SetVariableValue(VarName,VarValue[j]);
         PatternList.Add(NewPattern);
       end;
       //себя же удаляем из списка
       PatternList.Delete(i);
       VarValue.Free;
    end;
  end;
  until Not blnNonTerminalElementsExist ;
end;

function GetPatternList(intMaxElements:integer): TObjectList;
var
  i,j,k,l:integer;
  AllRules:TObjectList;
  Expansions:TObjectList;
  NewPattern,CurrentPattern: TSyntaxPattern;
  blnNonTerminalElementsExist:boolean;
begin

  AllRules:= GetAllRuleList;
  ExpandVariables(AllRules);

  //Получаем список шаблонов (т.е. цепочек, разворачивающих "фразу": S->xxx)
  Result:=GetRuleListSubset(nil,AllRules);
  //Это мы получили список фраз (S->xxx), который содержит в основном,
  //нетерминальные цепочки



  //После этого мы должны развернуть нетерминальные цепочки в терминальные
  // алгоритм самый простой.
  //  проверяются все цепочки в списке. Если оказывается, что в цепочке есть
  //  нетерминальный элемент, эта цепочка удаляется, а вместо нее вставляется
  //  N вариантов замены данного элемента, вместе с неизменным концом цепочки.
  //  Процесс повторяется, пока все нетерминальные элементы не будут заменены
  //  на терминальные
  repeat

  blnNonTerminalElementsExist:=False;
  for i:=0 to Result.Count-1 do
  begin
    CurrentPattern:=TSyntaxPattern(Result[i]);
    for j:=0 to CurrentPattern.ElementCount-1  do
    begin
      //Если этот элемент составной
      if not CurrentPattern.Elements[j].isTerminalElement  then
      begin

        //Надо получить правила, соответствующие данной фразовой категории.
        Expansions:=GetRuleListSubset(TSyntaxPattern(Result[i]).Elements[j],AllRules);
        if Expansions.Count>0 then
        begin
        blnNonTerminalElementsExist:=True; //Это составной элемент, в котором возможна замена.
        for k := 0 to Expansions.Count-1  do
        begin
          NewPattern:=TSyntaxPattern.Create;
          for l := 0 to j-1 do
            NewPattern.AddElement(CurrentPattern[l].WordForm, CurrentPattern[l].PartOfSpeach, CurrentPattern[l].Grammems.text);

          for l := 0 to TSyntaxPattern(Expansions[k]).ElementCount-1  do
            NewPattern.AddElement(TSyntaxPattern(Expansions[k])[l].WordForm, TSyntaxPattern(Expansions[k])[l].PartOfSpeach, TSyntaxPattern(Expansions[k])[l].Grammems.text);


          for l := j+1 to CurrentPattern.ElementCount-1 do
            NewPattern.AddElement(CurrentPattern[l].WordForm,
                                  CurrentPattern[l].PartOfSpeach,
                                  CurrentPattern[l].Grammems.text);
          //Формулу саму надо трансформировать. На место j-го элемента нужно поставить формулу
          //Распространенного элемента, а то что справа - сдвинуть.
          NewPattern.TrasformationFormula:=
          ModifyTransformationFormula(CurrentPattern.TrasformationFormula,
                                      TSyntaxPattern(Expansions[k]).TrasformationFormula,
                                      CurrentPattern.ElementCount,
                                      TSyntaxPattern(Expansions[k]).ElementCount,
                                      j+1 ) ;
          Result.Add(NewPattern);
        end; //кц по вариантам развертывания данного элемента.

        //После развертывания элемента цепочка удаляется.
        Result.Delete(i);
        end;
        //варианты тоже больше не нужны
        Expansions.Free;
        //надо перейти к следующей цепочке
        break;
      end;//if нетерминальный элемент
    end;
  end;
  Result.Pack;
  //Вот здесь можно удалить из списка те цепочки, которые
  //(начальные терминальные элементы которых) не соответствуют заданной фразе.
    //Удалим слишком длинные цепочки
    // Это можно сделать потому что у нас грамматика не сокращающая
  for i:=Result.Count-1 downto 0  do
    if TSyntaxPattern(Result[i]).ElementCount>intMaxElements  then
     Result.Delete(i);


  Result.Pack;
  //следует повторить цикл, потому что в цепочках могут еще оставаться
  //нетерминальные элементы
  until Not blnNonTerminalElementsExist ;

  //Чистка
  AllRules.Free;
end;

//Синтаксический разбор.
// - Функция принимает фразу,
//   а возвращает заматченный шаблон и число совпавших слов с начала фразы.
function SyntaxAnalysis(Phrase: TPhrase; var intMatchedWords, intUnmatchedWords:integer): TSyntaxPattern;
var
//  Pattern,Pattern2,Pattern3: TSyntaxPattern;
  PatternList: TList;
  i:integer;
begin

  //Cоздаем некий список шаблонов, которые мы будем проверять
  //(сверху вниз)

  PatternList := GetPatternList(Phrase.Count);

  //Проверка в цикле, начиная от наиболее специфичных
  PatternList.sort(ComparePatternLength);
  Result:=nil;
  //Я хочу потестировать фразу на соответствие некому синтаксическому шаблону.
  for i:=0 to PatternList.Count-1 do
  begin
    if TSyntaxPattern(PatternList[i]).TestPhrase(Phrase, intMatchedWords, intUnmatchedWords) then
    begin
      //Cоответствие найдено
      Result:= PatternList[i];

      //Найденный шаблон удаляется из списка
      PatternList.Extract(Result);
      break;
    end
  end;

  //Чистка памяти
  PatternList.Free;
end;

//Несколько альтернативная идея
//Будем строить синтаксическую структуру снизу вверх, перебирая последовательно
// слова и применяя правила.
// Концепция
// перебирать слова в цикле.
// - если слово способно быть главным в синтаксической группе, начать новую группу.
// - если синтаксическая группа уже начата, проверять (пропущенные) слова слева,
//   нельзя ли распространить ими эту группу
// - если группа уже начата (т.е. находится слева), проверить, нельзя ли присоединть это слово
//   к группе.
// - если ничего из этого сделать нельзя, пропустить слово и перейти к следующему.
// т.е мы всегда присоединяем зависимое слово к главному.
 //Другая альтернативная идея, делить фразу на фрагметы по предлогам  и союзам "а"
  // и вопросительным словам.
  // (предлог так или иначе начинает предложно именную группу), фрагменты анализировать шаблонами,
  // а получившиеся после анализа синтаксические группы комбинировать (тоже шаблонами).
  // еще бы понять какую нужно получить структуру. :)

function SyntaxAnalysis2(Phrase: TPhrase; var intMatchedWords, intUnmatchedWords:integer): TSyntaxPattern;
var i:integer;
begin
  //здесь мы предполагаем, что грамматическая омонимия устранна, и мы имеем дело с неким
  //вариантом лемматизации.  Хотя может надо наоборот, и тогда нужен поиск и возврат.
  for i:=0 to Phrase.Count-1 do
  begin

  end;

end;


Function TransformString(strInput:String; log:TStringList):String;
  var SplittedPhrase:TStringList;
      i,j:integer;
      Phrase: TPhrase;

      MatchedPattern: TSyntaxPattern;

      tmp: String;
      intMatchedWords, intUnmatchedWords:integer;
begin
  result:='';
  // 2. разбиение на слова
  SplittedPhrase:= Split(strInput,' ');

  //3. присвоение словоформам грамматических признаков
// даже если одна словоформа опознается как принадлежащая нескольким исходным словам,
// (например, "мыла" <- мыть|мыло, они запоминаются все, узнаванию синтаксических структур
// это мешать не должно.
//На этом этапе нам нужно получить некую структуру (Phrase), в которой будут содержаться
//слова исходной фразы, вместе с соответствующими им грамеммами (грамматическими признаками )
// и леммами (исходными формами)

  Phrase:=Lemmatize(SplittedPhrase);

  if TWordForm(Phrase[0]).WordForm='а' then
    Phrase.Delete(0);

  if TWordForm(Phrase[0]).WordForm='ну' then
    Phrase.Delete(0);

  if TWordForm(Phrase[0]).WordForm='и' then
    Phrase.Delete(0);

  //4.
  //Синтаксический разбор. На этом этапе мы должны получить структуру данных, которая
  //отражает синтаксическую структуру предложения и содержит слова с приписанными
  //синтаксическими признаками (зависимостями?)

  //на текущий момент такой структурой является заматченный шаблон.

   MatchedPattern:=SyntaxAnalysis(Phrase, intMatchedWords, intUnmatchedWords);

  //5. Трансформация
   if MatchedPattern <> nil then
     result:= MatchedPattern.ProcessTrasformationFormula;

  //вывод того что получилось.
   log.Add(result);
   log.Add( IntToStr( intMatchedWords) +'/'+  IntToStr( intUnmatchedWords));


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
  SplittedPhrase.Free;
  Phrase.Free;

  MatchedPattern.Free;
End;

//' Главная функция
//' strInput - ввод собеседника в сыром виде
// возращает лог :)
function ProcessUserInput(strInput: String; log:TStringList):String;
//Идеальный алгоритм.
// 1. Берется ввод пользователя.
// 2. Делится на предложенися по знакам препинания. '.!?;'
// 3. Каждое из предложений обрабатывается:
//      - делается его синтаксический разбор, возможно разбивается дополнительно.
//      - на основе синтаксической структуры делается трансформация  каждого предложения
//          в язык внутреннего представления (ЯВП).
//4. Получившийся набор предложений на ЯВП проталкивается дальше (в алису :) )
var
  _SentenceSplitter:TStringTokenizer;
  i:integer;
  strPhrase:string;
begin
   // 1. разбиение на предложения
  _SentenceSplitter:=TStringTokenizer.Create('.!?;');

  strInput:=AnsiLowerCase(strInput);
  _SentenceSplitter.Tokenize(trim(strInput));
  //SplittedPhrase:= Tokenizer._tokens;
  result:='';
  for i:=0 to _SentenceSplitter._count-1 do
  begin
    strPhrase:=trim(_SentenceSplitter._tokens[i]);

    if strPhrase<>'' then
      result:=result+TransformString(strPhrase,log)+' ';
  end;
  result:=trim(result);
  _SentenceSplitter.Free;
end;

//Проверка существующих Aiml шаблонов
//В сущности, нас интересует только <pattern>...</pattern>
function TestAIML(FileName:String):String;
var
  CurrentFile : TStringList;
  MyXml : TXmlParser;
begin
  CurrentFile := TStringList.Create;

  MyXml := TXmlParser.Create;
  MyXml.LoadFromFile (FileName);

  MyXml.StartScan;
  WHILE MyXml.Scan DO
    CASE MyXml.CurPartType OF

      ptContent, ptCData:
        if AnsiLowerCase (MyXml.CurName) ='pattern' then
        begin
           CurrentFile.Add(MyXml.CurContent);
           ProcessUserInput(MyXml.CurContent, CurrentFile);
           CurrentFile.Add('');
        end;
    END;
  MyXml.Free;


  result:=   CurrentFile.Text;
end;

//Инициализация  лемматизатора.
var   hr :  HRESULT;
initialization
  g_WordformCount:=0;
  g_ElementCount:=0;
  g_PatternCount:=0;
try
   // hr := CoInitialize(nil);
    if (hr <> S_OK) then
    begin
       // writeln('cannot load Component Object Model(COM) library');
      //  halt(1);
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

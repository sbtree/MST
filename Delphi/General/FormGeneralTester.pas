unit FormGeneralTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataBuffer, StdCtrls, ExtCtrls, ComCtrls, GenUtils, QExp;

type
  TfrmGeneralTester = class(TForm)
    btnTest: TButton;
    imgTest: TImage;
    trvDevices: TTreeView;
    btnCreateTree: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCreateTreeClick(Sender: TObject);
  private
    { Private-Deklarationen }
    //t_expr: TQExprParser;
  public
    { Public-Deklarationen }
  end;

var
  frmGeneralTester: TfrmGeneralTester;

implementation
{$R *.dfm}
uses StringPairs, Math, StrUtils;


procedure TestCase_CNExp;
var
  AVar:TQVar;
  AExpr:TQExprParser;
begin
AExpr:=TQExprParser.Create;
AVar:=QExpGlobal.FunctionByName('IfThen').Copy(AExpr);
AVar.Name:='�ж�';
AExpr.Add(AVar);
AVar:=QExpGlobal.FunctionByName('While').Copy(AExpr);
AVar.Name:='ѭ��';
AExpr.Add(AVar);
AVar:=QExpGlobal.FunctionByName('Break').Copy(AExpr);
AVar.Name:='�ж�';
AExpr.Add(AVar);
//���ı��ʽ
AExpr.Parse(
  '������=0;'#13#10+
  'ѭ��(������<100,'#13#10+
  ' ������++);'#13#10
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  '������=0;'#13#10+
  'ѭ��(������<100,'#13#10+
  ' {'#13#10+
  ' �ж�(������==50,�ж�(),������++);'#13#10+
  ' }'#13#10+
  ' );'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=50);
//C13.Other
//C13.1:String.Length
AExpr.Parse('Length("1234����ʲô")/100.323');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,Length('1234����ʲô')/100.323));
//C13.2:String.SubString
AExpr.Parse('IfThen((13>=1)&&(13<=2),SubString("����ʲôѽ",5,4),"û��");');
AExpr.Calc;
assert(AExpr.Value.AsString='û��');
AExpr.Parse('IfThen(Pos("bc","abc",False)<>0,SubString("����ʲôѽ",5,4),"û��");');
AExpr.Calc;
assert(AExpr.Value.AsString='ѽ');
AExpr.Parse('��è="����";');
assert(AExpr.Value.AsString='����');

AExpr.Free;
end;

//��������
procedure QExp_TestCase;
var
  AExpr:TQExprParser; sExpr: string;
begin
AExpr:=TQExprParser.Create;
//���ʽ��������
AExpr.AddConst('answer', '0101:00000064:C513');
//sExpr := 'sexpr="0x" + SubString(answer, 10, 4); Eval("100+2**3+" + sexpr);';
sExpr := 'sexpr="0x" + SubString(answer, 10, 4); Eval("(0x40 & " + sexpr);';
sExpr := ReplaceStr(sExpr, 'answer', '"0101:00000064:C513"');
AExpr.Parse(sExpr);
AExpr.Calc;
//assert(AExpr.Value.AsInteger=208);
assert(AExpr.Value.AsInteger = $40);

//C1:�򵥸�ֵ

AExpr.Parse('a=100;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=100);

//C2:���������� +,-,*,/,**,%,\

AExpr.Parse('a=4+5;b=5-2;c=4*5;d=4/5;e=4.0/5;f=2**3;g=5%3;h=5.1\3');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=9);
assert(AExpr.VarByName('b').Value.AsInteger=3);
assert(AExpr.VarByName('c').Value.AsInteger=20);
assert(AExpr.VarByName('d').Value.AsInteger=0);
assert(SameValue(AExpr.VarByName('e').Value.AsFloat,0.8));
assert(SameValue(AExpr.VarByName('f').Value.AsFloat,8));
assert(AExpr.VarByName('g').Value.AsInteger=2);
assert(AExpr.VarByName('h').Value.AsInteger=1);
//C3:�������,++/--

AExpr.Parse('a=1;a++;a--;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=1);
AExpr.Parse('a=1;++a;--a;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=1);
//C4:�Ƚ������
//C4.1:<
AExpr.Parse('a=1;b=2;a<b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b<a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.2:<=
AExpr.Parse('a=1;b=2;a<=b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b<=a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.3:>
AExpr.Parse('a=1;b=2;b>a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;a>b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.4:>=
AExpr.Parse('a=1;b=2;a>=b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b>=a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C4.5:==
AExpr.Parse('a=1;b=2;a==b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=1;a==b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C5:λ�����
//C5.1:|
AExpr.Parse('a=1;b=2;c=a|b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=3);
//C5.2:&
AExpr.Parse('a=1;b=2;c=a&b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=0);
//C5.3:^
AExpr.Parse('a=1;b=2;c=a^b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=3);
//C5.5:~
AExpr.Parse('a=1;b=~a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=-2);
//C6:�߼�����
//C6.1:&&
AExpr.Parse('a=1;b=2;a&&b;');//�κη�0����ֵ������Ϊ��true������a&&b=true
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=2;a&&b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);

//C6.2:||
AExpr.Parse('a=1;b=2;a||b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=2;a||b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=0;a||b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C6.3:!
AExpr.Parse('a=1;!a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;!a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C7:��λ�����
//C7.1:<<
AExpr.Parse('a=1;a<<1;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=2);
//C7.2:>>
AExpr.Parse('a=2;a>>1;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=1);
//C8:�Ը�ֵ�����
//C8.1:+=,-=,*=,/=,\=,%=
AExpr.Parse('a=1;a+=1;b=2;b-=1;c=3;c*=2;d=4;d/=2;e=5.0;e\=2;f=5;f%=2;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=2);
assert(AExpr.VarByName('b').Value.AsInteger=1);
assert(AExpr.VarByName('c').Value.AsInteger=6);
assert(AExpr.VarByName('d').Value.AsInteger=2);
assert(AExpr.VarByName('e').Value.AsInteger=2);
assert(AExpr.VarByName('f').Value.AsInteger=1);
AExpr.Calc;
//C8.2:&=,|=,^=,<<=,>>=
AExpr.Parse('a=1;a&=2;b=1;b|=2;c=1;c^=2;d=1;d<<=1;e=2;e>>=1;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=0);
assert(AExpr.VarByName('b').Value.AsInteger=3);
assert(AExpr.VarByName('c').Value.AsInteger=3);
assert(AExpr.VarByName('d').Value.AsInteger=2);
assert(AExpr.VarByName('e').Value.AsInteger=1);
//C9:�����������ȼ�
AExpr.Parse('a=1;b=2;c=a+1*b;');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(AExpr.Value.AsInteger=3);
AExpr.Parse('a=1;b=2;c=(a+2)*b;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=6);
//C10:�����������ʽ
AExpr.Parse('a=2*3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=10);
AExpr.Parse('a=1+3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=8);
AExpr.Parse('a=1+3*4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=13);
AExpr.Parse('a=1+2*3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=11);
//C11:��������
//C11.1:�ɱ������������
AExpr.Parse('a=avg(1,3,5,7);');
AExpr.Calc;
assert(AExpr.Value.AsInteger=4);
//C11.2:�����к����ʽ����
AExpr.Parse('a=avg(1+3,5+7,9+11);');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,(4+12+20)/3));
//C12:��ת
//C12.1:Goto
AExpr.Parse(
  'a=0;'+
  'a++;'+
  'ifthen(a<100,Goto(1),Goto(3));'+
  'Result=a;'
  );
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=100);
//C12.2:Exit
AExpr.Parse(
  'a=0;' +
  'ifthen(a<50,exit(1),exit(2));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=1);
AExpr.Parse(
  'a=50;' +
  'ifthen(a<50,exit(1),exit(2));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=2);
//C13.�����ַ���
AExpr.Parse('a=True;b=False;c=TRUE;d=FALSE;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsBoolean);
assert(not AExpr.VarByName('b').Value.AsBoolean);
assert(AExpr.VarByName('c').Value.AsBoolean);
assert(not AExpr.VarByName('d').Value.AsBoolean);
//C13.3:Round
AExpr.Parse('round(1.245,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.25));
AExpr.Parse('round(467.85,-1)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,467.9));
AExpr.Parse('round(1.255,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.26));
AExpr.Parse('round(225.5,2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,200));
AExpr.Parse('round(1.3333,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.33));
//C13.4:Like
AExpr.Parse('Like("Hello,World","%ll_,%l%",True);');
AExpr.Calc;
assert(AExpr.Value.AsBoolean);
AExpr.Parse('Like("Hello,World","%ll_,%b%",True);');
AExpr.Calc;
assert(not AExpr.Value.AsBoolean);
//C13.4:Regex
{$IFDEF QEXP_REGEX}
//match
AExpr.Parse('RegexMatch("My home work is done by myself.","/my/i",1);');
AExpr.Calc;
assert(AExpr.Value.AsString='My');
//matchnext
AExpr.Parse('RegexMatch("My home work is done by myself.","/my/i",1);RegexMatchNext();');
AExpr.Calc;
assert(AExpr.Value.AsString='my');
//replace
AExpr.Parse('RegexReplace("My home work is done by myself.","/my/i","Your",1);');
AExpr.Calc;
assert(AExpr.Value.AsString='Your home work is done by myself.');
AExpr.Parse('RegexReplace("My home work is done by myself.","/my/i","Your",-1);');
AExpr.Calc;
assert(AExpr.Value.AsString='Your home work is done by Yourself.');
//split
AExpr.Parse('RegexSplit("My home is china.","\\s",-1,false,false);s1=RegexSplitText(0);s2=RegexSplitText(1);s3=RegexSplitText(2);s4=RegexSplitText(3);');
AExpr.Calc;
assert(AExpr.VarByName('s1').Value.AsString='My');
assert(AExpr.VarByName('s2').Value.AsString='home');
assert(AExpr.VarByName('s3').Value.AsString='is');
assert(AExpr.VarByName('s4').Value.AsString='china.');
{$ENDIF}
AExpr.Parse(
  'function Add(x,y)'#13#10+
  '{'#13#10+
  'Result=x+y;'#13#10+
  '}'#13#10+
  'a=100;'#13#10+
  'b=200;'#13#10+
  'Result=Add(a+1,b+2);'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=303);
AExpr.Parse(
  'IfThen(1>2,'#13#10+
  '{1+2;},'#13#10+
  '{1-2;}'#13#10+
  ');');
assert(AExpr.Value.AsInteger=-1);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' a++);'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' IfThen(a>50,Break(),a++));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=51);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' {'#13#10+
  ' b=0;'#13#10+
  ' while(b<50,Break());'#13#10+
  ' a++;'#13#10+
  ' });'#13#10
  );
AExpr.Calc;
//ShowMessage(AExpr.VarByName('a').Value.AsString);
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  'a=Eval("100+2**3");');
AExpr.Calc;
assert(AExpr.Value.AsInteger=108);
AExpr.Free;
//TestCase_CNExp;
end;

procedure TfrmGeneralTester.btnCreateTreeClick(Sender: TObject);
var
  MyTreeNode1, MyTreeNode2: TTreeNode;
begin
  with trvDevices.Items do
  begin
    Clear; { remove any existing nodes }
    MyTreeNode1 := Add(nil, 'RootTreeNode1'); { Add a root node }
    { Add a child node to the node just added }
    AddChild(MyTreeNode1,'ChildNode1');

    {Add another root node}
    MyTreeNode2 := Add(MyTreeNode1, 'RootTreeNode2');
    {Give MyTreeNode2 to a child }
    AddChild(MyTreeNode2,'ChildNode2');

    {Change MyTreeNode2 to ChildNode2 }
    { and add a child node to it}
    MyTreeNode2 := trvDevices.Items[3];
    AddChild(MyTreeNode2,'ChildNode2a');

    {Add another child to ChildNode2, after ChildNode2a }
    Add(MyTreeNode2,'ChildNode2b');

    {add another root node}
    Add(MyTreeNode1, 'RootTreeNode3');
  end;
end;

procedure TfrmGeneralTester.btnTestClick(Sender: TObject);
var chbuffer: TCharBuffer;  t: cardinal; n:integer; tPairs: TStringPairs;
    sNames, sValues, tComPorts: TStrings; v: variant; iInt64: int64; r: double;
begin
  //QExp_TestCase();
  ShowMessage(TGenUtils.ShowStrHex('ABCDE12345'));
  v := 1;
  v := '12.5';
  if (VarType(v) = varString) then v := TGenUtils.ReplaceDecimalSeparator(v);
  r := v;
  ShowMessage(format('iInt64 = %f',[r]));
  sNames := TStringList.Create();
  sValues := TStringList.Create();
  tPairs := TStringPairs.Create();
  tComPorts := TStringList.Create();

  sNames.Add('Test1=text1=1');
//  sNames.Add('Test1=text1+text1');
  n := sNames.IndexOfName('Test1');
  ShowMessage(format('sNames: count=%d; Test1: index=%d, value=%s',
              [sNames.Count, n, sNames.ValueFromIndex[n] ]));

  tPairs.AddPair('Test1', 'text1');
  tPairs.AddPair('Test[2]', 'text2');
  tPairs.AddPair('Test3', 'text3');
  tPairs.AddPair('Test4=');
  tPairs.AddPair('Test5');
  tPairs.AddPair('=text6');

  tPairs.GetPairNames(sNames);
  tPairs.GetPairValues(sValues);
  ShowMessage(format('all names: %s; all values: %s', [sNames.DelimitedText, sValues.DelimitedText]));

  tPairs.SetPairValue('Test_2', '');
  tPairs.GetPairNames(sNames);
  tPairs.GetPairValues(sValues);
  ShowMessage(format('all names: %s; all values: %s', [sNames.DelimitedText, sValues.DelimitedText]));

  tComPorts.Add('Test7=text7');
  tComPorts.Add('Test8=8');
  tPairs.AddPairs(tComPorts);
  tPairs.GetPairNames(sNames);
  tPairs.GetPairValues(sValues);
  ShowMessage(format('all names: %s; all values: %s', [sNames.DelimitedText, sValues.DelimitedText]));

 {
  //TGenUtils.EnumComPorts(tComPorts);
  //ShowMessage(tComPorts.DelimitedText);
  tComPorts.Add('Test1=');
  tComPorts.Add('Test2=text of test2');

  ShowMessage(format('Index of Test1 = %d, index of Test2 = %d', [tComPorts.IndexOf('Test1='), tComPorts.IndexOf('Test2=text of test2')]));
  n := tComPorts.IndexOfName('Test1');
  ShowMessage(format('Test1 (Name): Index = %d, value = "%s"; Test2 (Name): index = %d',
          [n, tComPorts.ValueFromIndex[n], tComPorts.IndexOfName('Test2')]));

  n := tComPorts.IndexOfName('Test2');
  tComPorts.ValueFromIndex[n] := '';
  ShowMessage(format('Index of Test2 = %d, index of Test2 (Name) = %d', [tComPorts.IndexOf('Test2=text of test2'), tComPorts.IndexOfName('Test2')]));


  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  //ShowMessage(format('imgTest.ClientRect=[%d, %d, %d, %d]',
  //          [imgTest.ClientRect.Left, imgTest.ClientRect.Top, imgTest.ClientRect.Right, imgTest.ClientRect.Bottom]));
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);

  chbuffer := TCharBuffer.Create();
  chbuffer.Resize(10);
  t:=GetTickCount;
  n:=chbuffer.WriteHex('19084091483758913478faf214f');
  //chbuffer.Resize(14);
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('2.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryHex=%s', [chbuffer.HistoryHex()]));
  t:=GetTickCount;
  n:=chbuffer.WriteStr('987654321');
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('2.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryStr=%s', [chbuffer.HistoryStr()]));


  for n := $30 to $39 do chbuffer.WriteChar(char(n));
  ShowMessage(format('ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('HistoryStr=%s',[chbuffer.HistoryStr()]));
  ShowMessage(format('HistoryHex=%s',[chbuffer.HistoryHex()]));

  FreeAndNil(chbuffer); }
  FreeAndNil(tPairs);
  FreeAndNil(sValues);
  FreeAndNil(sNames);
  FreeAndNil(tComPorts);
end;

procedure TfrmGeneralTester.FormCreate(Sender: TObject);
begin
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo_BuR.bmp');
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);
end;

end.

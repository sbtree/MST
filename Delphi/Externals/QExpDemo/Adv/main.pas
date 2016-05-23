unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,QExp, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnCreateButton: TButton;
    btnFuncAssign: TButton;
    btnJSFuncAssign: TButton;
    btnBlockAssign: TButton;
    btnCallScriptFunction: TButton;
    btnVarArgs: TButton;
    procedure btnCreateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnFuncAssignClick(Sender: TObject);
    procedure btnJSFuncAssignClick(Sender: TObject);
    procedure btnBlockAssignClick(Sender: TObject);
    procedure btnCallScriptFunctionClick(Sender: TObject);
    procedure btnVarArgsClick(Sender: TObject);
  private
    { Private declarations }
    FExpr:TQExprParser;
    procedure DoCreateButton(ASender:TObject);
    procedure DoSetCaption(ASender: TObject);
    procedure DoGetCaption(ASender:TObject);
    procedure DoButtonAssigned(ASender:TQExprParser;AVar:TQVar);
  public
    { Public declarations }
  end;
  
var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoSetCaption(ASender: TObject);
var
  AButton:TButton;
  AVar:TQVar;
begin
AVar:=ASender as TQVar;
AButton:=TButton(AVar.Parent.Value.AsInteger);
AButton.Caption:=AVar.Value.AsString;
end;

procedure TForm1.btnCreateButtonClick(Sender: TObject);
var
  AButton:TButton;
begin
FExpr.Parse(
  'AButton=Button();'#13#10+
  'AButton.Text="Test";'
  );
FExpr.Calc;
AButton:=TButton(FExpr.VarByName('AButton').Value.AsInteger);
ShowMessage(
  '[Script]'#13#10+FExpr.Text+
  #13#10'[Result]'#13#10'AButton with text "Test" displayed.'#13#10+
  '[Notice]'#13#10'After this message closed,the button create by script will free by delphi code.');
AButton.Free;
end;

procedure TForm1.btnFuncAssignClick(Sender: TObject);
begin
FExpr.Parse(
  'function Add(x,y)'#13#10+
  '{'#13#10+
  'Result=x+y;'#13#10+
  '}'#13#10+
  'a=Add;'#13#10+
  'a(1,2);'
  );
FExpr.Calc;
ShowMessage('[Script]'#13#10+FExpr.Text+#13#10#13#10'[Result]'#13#10+FExpr.Value.AsString);  
end;

procedure TForm1.btnJSFuncAssignClick(Sender: TObject);
begin
FExpr.Parse(
  'a=function (x,y)'#13#10+
  '{'#13#10+
  'Result=x+y;'#13#10+
  '};'#13#10+
  'a(1,2);'
  );
FExpr.Calc;
ShowMessage('[Script]'#13#10+FExpr.Text+#13#10#13#10'[Result]'#13#10+FExpr.Value.AsString);
end;

procedure TForm1.btnVarArgsClick(Sender: TObject);
begin
FExpr.Parse(
  'function DoSum(...)'#13#10+
  '{'#13#10+
  'AResult=0;'#13#10+
  'AIndex=0;'#13#10+
  'function Add(x)'#13#10+
  ' {'#13#10 +
  ' AResult=AResult+x;'#13#10 +
  ' AIndex++;'#13#10+
  ' Result=AResult;'#13#10+
  ' }'#13#10+
  'while(AIndex<ParamCount(),Add(Params(AIndex)));'#13#10+
  '}'#13#10+
  'Result=DoSum(1,3,4,5,6);'
  );
FExpr.Calc;
ShowMessage('[Script]'#13#10+FExpr.Text+#13#10#13#10'[Result]'#13#10+FExpr.Value.AsString);
end;

procedure TForm1.btnBlockAssignClick(Sender: TObject);
begin
FExpr.Parse(
  'a='#13#10+
  '{'#13#10+
  'Result=1+2;'#13#10+
  '};'
  );
FExpr.Calc;
ShowMessage('[Script]'#13#10+FExpr.Text+#13#10#13#10'[Result]'#13#10+FExpr.Value.AsString);
end;

procedure TForm1.btnCallScriptFunctionClick(Sender: TObject);
var
  AFunc:TQFunction;
begin
FExpr.Parse(
  'function Add(x,y)'#13#10+
  '{'#13#10+
  'Result=x+y;'#13#10+
  '}'
  );
AFunc:=FExpr.FunctionByName('Add');
AFunc.Parameters[0].Value.AsInteger:=100;
AFunc.Parameters[1].Value.AsInteger:=200;
AFunc.Call;
ShowMessage('Call Script function Add(100,200)='+AFunc.Value.AsString);  
end;

procedure TForm1.DoButtonAssigned(ASender: TQExprParser; AVar: TQVar);
var
  AChild:TQVar;
begin
AChild:=AVar.Add('Text',False);
AChild.OnValueChange:=DoSetCaption;
AChild.OnGetValue:=DoGetCaption;
end;

procedure TForm1.DoCreateButton(ASender: TObject);
var
  AButton:TButton;
  AFunc:TQFunction;
begin
AButton:=TButton.Create(Self);
AButton.Parent:=Panel1;
AButton.SetBounds(Random(ClientWidth),50+Random(ClientHeight),75,25);
AFunc:=ASender as TQFunction;
AFunc.Value.AsInteger:=Integer(AButton);
AFunc.Owner.SetAssignHelper(DoButtonAssigned);
end;

procedure TForm1.DoGetCaption(ASender: TObject);
var
  AButton:TButton;
  AVar:TQVar;
begin
AVar:=ASender as TQVar;
AButton:=TButton(AVar.Parent.Value.AsInteger);
AVar.Value.AsString:=AButton.Caption;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FExpr:=TQExprParser.Create;
FExpr.AddFunction('Button',DoCreateButton);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
FExpr.Free;
end;

end.

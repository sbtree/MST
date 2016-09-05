unit Main;
{Small program to compare the performance of ParseExpr with compiled
Delphi. Code: Arnulf Sortland}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons,
  ComCtrls, ParseExpr;

type
  TMainForm = class(TForm)
    Button2: TBitBtn;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    X: double;
    Y:double;
    ExprParser1: TExpressionParser;
    function TestTimeFuncFloat: double;
    function TestTimeFuncBool: boolean;
  public
    { Public declarations }
  end;

const
  TimesToEvaluate = 500000;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
uses Math;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  X := 1.2;
  Y:=0.55;
  ExprParser1 := TExpressionParser.Create;
  ExprParser1.ClearExpressions;
  ExprParser1.DefineVariable('x', @X);
  ExprParser1.DefineVariable('y', @Y);
  ExprParser1.AddExpression('sin(2*y)*cos(x)');
  //ExprParser1.AddExpression('sin(2*y) >  cos(x)');
  Label1.Caption := 'Function: ' + ExprParser1.Expression[0];
  Label2.Caption := 'Evaluate it ' + IntToStr(TimesToEvaluate) + ' times';
  Button2.Click;
end;

function TMainForm.TestTimeFuncFloat: double;
begin
  result := sin(2*y) * cos(x);
  //must be the same as Expression!!
end;

function TMainForm.TestTimeFuncBool: boolean;
begin
  result := (sin(2*y) >  cos(x));
  //must be the same as Expression!!
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ExprParser1.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  tick_now: array[0..2] of Longint;
  time_ms: array[0..1] of Longint;
  fE: array[0..1] of double;
  bE: array[0..1] of boolean;
  i: Longint;
begin
  Button2.Enabled := false;
  tick_now[0] := GetTickCount;
  for i := 1 to TimesToEvaluate do
    fE[0] := TestTimeFuncFloat;
    //bE[0] := TestTimeFuncBool;

  tick_now[1] := GetTickCount;
  for i := 1 to TimesToEvaluate do
    fE[1] := ExprParser1.EvaluateCurrent;
    //bE[1] := ExprParser1.AsBoolean[0];

  tick_now[2] := GetTickCount;

  time_ms[0] := tick_now[1] - tick_now[0];
  time_ms[1] := tick_now[2] - tick_now[1];

  Memo1.Lines.Add(Format('Compiled Delphi code :%10d ms', [time_ms[0]]));
  Memo1.Lines.Add(Format('ExprParser :%10d ms', [time_ms[1]]));
  Memo1.Lines.Add(Format('ExprParser - Delphi :%10d ', [time_ms[1] - time_ms[0]]));
  //Memo1.Lines.Add(Format('Result: ExprParser = Delphi? %s ', [BoolToStr(bE[0] = bE[1], true)]));
  Memo1.Lines.Add(Format('Result: ExprParser = Delphi? %s ', [BoolToStr(abs(fE[0] - fE[1]) <= MinDouble, true)]));
  Memo1.Lines.Add('');
  Button2.Enabled := true;
end;

end.


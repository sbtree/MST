// =============================================================================
// Module name  : $RCSfile: FuncMat.pas,v $
// description  : The classes of script functions which have only mathematical
//                functionality, are implemented in this unit.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncMat;

interface
uses Classes, FuncBase, TextMessage;
type
  EvaluateStr = class(TFunctionBase)
  public
    function LoadParameter(const par: string): boolean; override;
    function DoTask(): boolean; override;
  end;

implementation
uses SysUtils;

function EvaluateStr.LoadParameter(const par: string): boolean;
begin
  t_msgrimpl.AddMessage(format('todo->"LoadParameter" must be reimplemented in class "%s".', [self.ClassName]), ML_WARNING);
  result := true;
end;

function EvaluateStr.DoTask(): boolean;
begin
  t_msgrimpl.AddMessage(format('todo->"Execute" must be reimplemented in class "%s".', [self.ClassName]), ML_WARNING);
  result := true;
end;

initialization
  Classes.RegisterClass(EvaluateStr);

finalization
  Classes.UnregisterClass(EvaluateStr);

end.

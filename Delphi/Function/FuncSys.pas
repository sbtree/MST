// =============================================================================
// Module name  : $RCSfile: FuncSys.pas,v $
// description  : The classes of script functions which are relevant to the
//                system of this program (SYS), are implemented in this unit, e.g.
//                Jump-Function, Repeat-Controll etw.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncSys;

interface
uses Classes, FuncBase, TextMessage;

type

  RepeatIfTrue = class(TFunctionBase)

  end;

  RepeatIfFalse = class(TFunctionBase)

  end;

  JumpIfTrue = class(TFunctionBase)

  end;

  JumpIfFalse = class(TFunctionBase)

  end;

implementation
uses Contnrs;

initialization
  //Classes.RegisterClass(YourSubclass);

finalization
  //Classes.UnregisterClass(YourSubclass);
end.

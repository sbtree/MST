// =============================================================================
// Module name  : $RCSfile: FuncSys.pas,v $
// description  : The classes of script functions which are relevant to the
//                software system of this program (SYS), are implemented in this unit.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncSys;

interface
uses Classes, FuncBase, TextMessage;

type
  LoopBegin = class(TFunctionBase)

  end;

  LoopEnd = class(TFunctionBase)

  end;

  IfTrueJump = class(TFunctionBase)

  end;

  IfFalseJump = class(TFunctionBase)

  end;

implementation

initialization
  //Classes.RegisterClass(YourSubclass);

finalization
  //Classes.UnregisterClass(YourSubclass);
end.

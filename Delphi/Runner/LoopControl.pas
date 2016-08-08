unit LoopControl;

interface
uses Contnrs;

type
  TLoopEntry = class

  public
    constructor Create();
    destructor Destroy(); override;

  end;

  TLoopEntryStack = class(TObjectStack)

  end;

const
  CSTR_LOOP_BEGIN:  string = 'LoopBegin';
  CSTR_LOOP_END:    string = 'LoopEnd';
  
implementation

end.

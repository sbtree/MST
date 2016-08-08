unit LoopControl;

interface
uses Contnrs;

type
  TLoopEntry = class
  protected
    c_timeout:cardinal;
    s_endexp: string;
  public
    constructor Create();
    destructor Destroy(); override;

    property Timeout: cardinal read c_timeout write c_timeout;
    property EndCondition: string read s_endexp write s_endexp;


  end;

  TLoopEntryStack = class(TObjectStack)

  end;

const
  CSTR_LOOP_BEGIN:  string = 'LoopBegin';
  CSTR_LOOP_END:    string = 'LoopEnd';
  
implementation

end.

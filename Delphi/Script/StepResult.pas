unit StepResult;

interface
type
  TStepResult = class
  protected
    b_stepok:   boolean;
    s_result:   string;
    s_pattern:  string
  public
    constructor Create();
    destructor Destroy(); override;

    property ResultString: string read s_result write s_result;
    property ValuePattern: string read s_pattern write s_pattern;
    property Resulted: boolean read b_stepok write b_stepok;

    function ValueStr(const pattern: string = ''): string; virtual;
    function ValueInt(const pattern: string = ''): integer; virtual;
    function ValueWord(const pattern: string = ''): word; virtual;
    function ValueReal(const pattern: string = ''): real; virtual;
  end;

implementation
constructor TStepResult.Create();
begin
  inherited Create();
  b_stepok := false;
end;

destructor TStepResult.Destroy();
begin
  inherited Destroy();
end;

end.

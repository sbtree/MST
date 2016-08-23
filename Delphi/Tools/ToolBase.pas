unit ToolBase;

interface
uses StringPairs;
type
  TToolBase = class
  protected
    t_conf: TStringPairs;

  protected
    function GetCommandLine(): string; virtual;

  public
    constructor Create();
    destructor Destroy(); override;

    property CommandLine: string read GetCommandLine;

  end;
implementation

function TToolBase.GetCommandLine(): string;
begin
  result := '';
  //todo:
end;

constructor TToolBase.Create();
begin

end;

destructor TToolBase.Destroy();
begin

end;

end.

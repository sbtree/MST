unit ProtocolBase;

interface

type
  TProtBase = class
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TProtBase.Create;
begin
	inherited Create;
end;

destructor TProtBase.Destroy;
begin
	inherited Destroy;
end;

end.

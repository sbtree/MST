unit FuncFlashRunner;

interface
uses Classes, FunctionBase;

type
  FR_Set_DM = class(TFunctionBase)
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor FR_Set_DM.Create;
begin
	inherited Create;
end;

destructor FR_Set_DM.Destroy;
begin
	inherited Destroy;
end;

initialization
  Classes.RegisterClass(FR_Set_DM);

finalization
  Classes.UnregisterClass(FR_Set_DM);
end.

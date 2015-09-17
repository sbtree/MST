unit Multimeter;

interface
uses DeviceBase, IniFiles, Classes;

type
  ECurrentFlow = (
                  CF_DC, //derect current
                  CF_AC  //alternating current
                  );

  TMultimeter = class(TDeviceBase)
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function ConfigDevice(const ini: TMemIniFile): Boolean; override;
    function GetLastError(var msg: string): Integer; override;

    function CloseRelays(const relays: string): boolean;
    function OpenRelays(const relays: string): boolean;
    function MeasureR(): real;
    function MeasureI(const cf: ECurrentFlow): real;
    function MeasureU(const cf: ECurrentFlow): real;
    function MeasureF(): real;
  end;
implementation

constructor TMultimeter.Create(owner: TComponent);
begin
	inherited Create(owner);
end;

destructor TMultimeter.Destroy;
begin
	inherited Destroy;
end;

function TMultimeter.ConfigDevice(const ini: TMemIniFile): Boolean;
begin
  //todo
  result := false;
end;

function TMultimeter.GetLastError(var msg: string): Integer;
begin
  //todo
  result := 0;
end;

function TMultimeter.CloseRelays(const relays: string): boolean;
begin
  //todo
  result := false;
end;

function TMultimeter.OpenRelays(const relays: string): boolean;   
begin
  //todo
  result := false;
end;

function TMultimeter.MeasureR(): real; 
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureI(const cf: ECurrentFlow): real;
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureU(const cf: ECurrentFlow): real; 
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureF(): real;
begin
  //todo
  result := 0.0;
end;

end.

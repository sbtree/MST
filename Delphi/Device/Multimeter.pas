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
    constructor Create(owner: TComponent);
    destructor Destroy; override;

    function GetLastError(var msg: string): Integer; override;
    function ConfigDevice(const ini: TMemIniFile): Boolean; override;
    function FreeDevice(): Boolean; override;
    function Connect(): Boolean; override;
    function Disconnect: boolean; override;
    function SendStr(const sdata: string; const bans: boolean = true): Integer; override;
    function RecvStr(var sdata: string): Integer; override;

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

function TMultimeter.GetLastError(var msg: string): Integer;
begin

end;

function TMultimeter.ConfigDevice(const ini: TMemIniFile): Boolean;
begin

end;

function TMultimeter.FreeDevice(): Boolean;
begin

end;

function TMultimeter.Connect(): Boolean; 
begin

end;
function TMultimeter.Disconnect: boolean; 
begin

end;
function TMultimeter.SendStr(const sdata: string; const bans: boolean = true): Integer; 
begin

end;
function TMultimeter.RecvStr(var sdata: string): Integer; 
begin

end;

function TMultimeter.CloseRelays(const relays: string): boolean;
begin

end;
function TMultimeter.OpenRelays(const relays: string): boolean;   
begin

end;
function TMultimeter.MeasureR(): real; 
begin

end;
function TMultimeter.MeasureI(const cf: ECurrentFlow): real;
begin

end;
function TMultimeter.MeasureU(const cf: ECurrentFlow): real; 
begin

end;
function TMultimeter.MeasureF(): real;
begin

end;

end.

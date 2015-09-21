unit Multimeter;

interface
uses DeviceBase, IniFiles, Classes;

type
  ECurrentFlow = (
                  CF_DC, //direct current
                  CF_AC  //alternating current
                  );

  EContinueMode = (
                  CM_OFF,
                  CM_ON
                  );

  EMeasureAction = (
                    MA_RES,     //measure resistance
                    MA_DCV,     //measure direct voltage
                    MA_ACV,     //measure alternating voltage
                    MA_DCI,     //measure direct current
                    MA_ACI,     //measure alternating current
                    MA_FREQ,    //measure frequence
                    MA_PERI,    //measure period
                    MA_TEMP     //measure temperature
                    );
  TMultimeter = class(TDeviceBase)
  type Channels = 1..40;
  protected
    e_cont : EContinueMode; //indicate if continue mode is shut on
    w_cards: word; //count of installed model 7700 switching module

  protected
    function GetClosedRelays(): string; virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property ContinueMode : EContinueMode read e_cont write e_cont;
    property CountCards: word read w_cards;

    function ConfigDevice(const ini: TMemIniFile): Boolean; override;
    function GetLastError(var msg: string): Integer; override;

    function CloseRelays(const relays: string): boolean; virtual;
    function OpenRelays(const relays: string): boolean; virtual;
    function MeasureR(): real; virtual;
    function MeasureDCV(): real; virtual;
    function MeasureACV(): real; virtual;
    function MeasureDCI(): real; virtual;
    function MeasureACI(): real; virtual;
    function MeasureF(): real; virtual;
    function MeasureP(): real; virtual;
    function MeasureT(): real; virtual;
  end;
implementation

function TMultimeter.GetClosedRelays(): string;
begin
  //todo
  result := '';
end;

constructor TMultimeter.Create(owner: TComponent);
begin
	inherited Create(owner);
  w_cards := 0;
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

function TMultimeter.MeasureDCV(): real;
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureACV(): real;
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureDCI(): real;
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureACI(): real;
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

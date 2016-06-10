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
    function QueryRelays(): string; virtual;
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

const
  // Keithley-Multimert 2700
  C_MULTIMETER_BEEP_OFF       = 'SYST:BEEP 0';         // Beep off
  C_MULTIMETER_OPTIONEN       = '*OPT?';               // Optionen erfragen
  C_MULTIMETER_CLEAR_ERROR    = '*CLS';                // alle 'Event register' und 'error queue' loeschen
  C_MULTIMETER_SELF_TEST      = '*TST?';               // Eigentest
  C_MULTIMETER_OPEN_ALL       = 'ROUT:OPEN:ALL';       // alle Relais oeffnen
  C_MULTIMETER_OPEN           = 'ROUT:MULT:OPEN (@%s)'; //n relays to open. %s: relay numbers with separator ','
  C_MULTIMETER_CLOSE          = 'ROUT:MULT:CLOS (@%s)'; //n relays to clase. %s: relay numbers with separator ','
  C_MULTIMETER_CLOSE_ASK      = 'ROUT:CLOS?';           // geschlossene Relais erfragen
  C_MULTIMETER_FORMAT_ELEMENT = 'FORM:ELEM READ';       // Datenformat spezifizieren
  C_MULTIMETER_MEAS_ONE       = 'INIT:CONT OFF';        // one-shot measurement mode
  C_MULTIMETER_MEAS_READ      = 'READ?';                // Einzelmessung triggern
  C_MULTIMETER_MEAS_CONTINU   = 'INIT:CONT ON';         // continuouse measurement mode
  C_MULTIMETER_DATA_ASK       = 'DATA?';                // Daten lesen
  C_MULTIMETER_SET_FUNC       = 'FUNC "%s"';            // set function to messure
  C_MULTIMETER_FUNC_ASK       = 'FUNC?';                // ask for the current function of the messurement

  C_MESSURE_FUNC: array[EMeasureAction] of string = (
                  'RES',
                  'VOLT:DC',
                  'VOLT:AC',
                  'CURR:DC',
                  'CURR:AC',
                  'FREQ',
                  'PER',
                  'TEMP'
                  );

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

function TMultimeter.MeasureP(): real;
begin
  //todo
  result := 0.0;
end;

function TMultimeter.MeasureT(): real;
begin
  //todo
  result := 0.0;
end;

end.

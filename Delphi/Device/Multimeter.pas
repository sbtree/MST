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

  IMultimeter = interface
    function MeasureR(var val: real): boolean;
    function MeasureDCV(var val: real): boolean;
    function MeasureACV(var val: real): boolean;
    function MeasureDCI(var val: real): boolean;
    function MeasureACI(var val: real): boolean;
    function MeasureF(var val: real): boolean;
    function MeasureP(var val: real): boolean;
    function MeasureT(var val: real): boolean;
  end;

  TMultimeter = class(TDeviceBase, IMultimeter)
  protected
    e_cont :  EContinueMode;  //indicate if continue mode is shut on
    e_curma:  EMeasureAction; //current setting for the measurement
    w_cards:  word; //count of installed model 7700 switching module
  protected
    function InitFromFile(const sfile: string): boolean; virtual;
    function SwitchMeasurement(const meas: EMeasureAction): boolean; virtual;
    function ReadData(var val: real): boolean; virtual;
    procedure TriggerMesssure(); virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property ContinueMode : EContinueMode read e_cont write e_cont;
    property CountCards: word read w_cards;

    function MeasureR(var val: real): boolean;
    function MeasureDCV(var val: real): boolean;
    function MeasureACV(var val: real): boolean;
    function MeasureDCI(var val: real): boolean;
    function MeasureACI(var val: real): boolean;
    function MeasureF(var val: real): boolean;
    function MeasureP(var val: real): boolean;
    function MeasureT(var val: real): boolean;
  end;

  TMultimeterKeithley = class(TMultimeter)
  const
    // Keithley-Multimert 2700
    C_MULTIMETER_BEEP_OFF       = 'SYST:BEEP 0';          // Beep off
    C_MULTIMETER_OPTIONEN       = '*OPT?';                // Optionen erfragen
    C_MULTIMETER_CLEAR_ERROR    = '*CLS';                 // alle 'Event register' und 'error queue' loeschen
    C_MULTIMETER_SELF_TEST      = '*TST?';                // Eigentest
    C_MULTIMETER_OPEN_ALL       = 'ROUT:OPEN:ALL';        // alle Relais oeffnen
    C_MULTIMETER_OPEN           = 'ROUT:MULT:OPEN (@%s)'; //n relays to open. %s: relay numbers with separator ','
    C_MULTIMETER_CLOSE          = 'ROUT:MULT:CLOS (@%s)'; //n relays to clase. %s: relay numbers with separator ','
    C_MULTIMETER_CLOSE_ASK      = 'ROUT:CLOS?';           // geschlossene Relais abfragen
    C_MULTIMETER_FORMAT_ELEMENT = 'FORM:ELEM READ';       // Datenformat spezifizieren
    C_MULTIMETER_MEAS_ONE       = 'INIT:CONT OFF';        // one-shot measurement mode
    C_MULTIMETER_MEAS_CONTINU   = 'INIT:CONT ON';         // continuouse measurement mode
    C_MULTIMETER_MEAS_READ      = 'READ?';                // Einzelmessung triggern
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

  protected
    function SwitchMeasurement(const meas: EMeasureAction): boolean; virtual;
    function ReadData(var val: real): boolean; virtual;
    procedure TriggerMesssure(); virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
  end;
implementation
uses SysUtils, TextMessage;

function TMultimeter.InitFromFile(const sfile: string): boolean;
begin
  result := false;
  AddMessage(format('"%s.InitFromFile" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.SwitchMeasurement(const meas: EMeasureAction): boolean;
begin
  result := false;
  AddMessage(format('"%s.SwitchMeasurement" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.ReadData(var val: real): boolean;
begin
  result := false;
  AddMessage(format('"%s.ReadData" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

procedure TMultimeter.TriggerMesssure();
begin
  //todo: if it's neccessary
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

function TMultimeter.MeasureR(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_RES) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCV(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACV(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCI(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACI(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureF(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_FREQ) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureP(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_PERI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureT(var val: real): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_TEMP) then result := ReadData(val)
  else result := false;
end;

end.

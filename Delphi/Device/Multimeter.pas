unit Multimeter;

interface
uses DeviceBase, IniFiles, Classes, RelayControl, TextMessage, ConfigBase;

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
                    MA_ANY,     //undefinitive measurement
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
    function MeasureR(var val: double): boolean;
    function MeasureDCV(var val: double): boolean;
    function MeasureACV(var val: double): boolean;
    function MeasureDCI(var val: double): boolean;
    function MeasureACI(var val: double): boolean;
    function MeasureF(var val: double): boolean;
    function MeasureP(var val: double): boolean;
    function MeasureT(var val: double): boolean;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: double = 0.0);
  end;

  TMultimeter = class(TDeviceBase, IMultimeter)
  protected
    e_curma:  EMeasureAction;       //current setting for the measurement
  protected
    function InitFromFile(const sfile: string): boolean; virtual;
    function SwitchMeasurement(const meas: EMeasureAction): boolean; virtual;
    function ReadData(var val: double): boolean; virtual;
    procedure TriggerMesssure(); virtual;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function MeasureR(var val: double): boolean; virtual;
    function MeasureDCV(var val: double): boolean; virtual;
    function MeasureACV(var val: double): boolean; virtual;
    function MeasureDCI(var val: double): boolean; virtual;
    function MeasureACI(var val: double): boolean; virtual;
    function MeasureF(var val: double): boolean; virtual;
    function MeasureP(var val: double): boolean; virtual;
    function MeasureT(var val: double): boolean; virtual;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: double); virtual;
  end;

  TMultimeterKeithley = class(TMultimeter, IRelayControl)
  protected
    e_cont :  EContinueMode;        //indicate if continue mode is shut on
    s_idn:    string;
    t_relay:  TRelayKeithley;
  protected
    function SwitchMeasurement(const meas: EMeasureAction): boolean; override;
    function ReadingValue(const elem: string): string;
    function ReadData(var val: double): boolean; override;
    function GetRelayCards(): integer;
    //procedure TriggerMesssure(); virtual;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;

    property RelayControl: TRelayKeithley read t_relay implements IRelayControl;
    property ContinueMode : EContinueMode read e_cont write e_cont;
    property RelayCards: integer read GetRelayCards;

    function InitDevice(): boolean; override;
    function ReleaseDevice(): boolean; override;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: double); override;
  end;
implementation
uses SysUtils, GenUtils, StrUtils, RS232;

const
  // Keithley-Multimert 2700
  C_KEITHLEY_BEEP_OFF       = 'SYST:BEEP 0';    // Beep off
  C_KEITHLEY_CLEAR_ERROR    = '*CLS';           // alle 'Event register' und 'error queue' loeschen
  C_KEITHLEY_SELF_TEST      = '*TST?';          // Eigentest
  C_KEITHLEY_ID_QUERY       = '*IDN?';          // string for querying identifer of the device
  C_KEITHLEY_RESET          = '*RST';           // string for resetting the device
  C_KEITHLEY_FORMAT_ELEMENT = 'FORM:ELEM READ'; // Datenformat: element only read
  C_KEITHLEY_FORMAT_ASCII   = 'FORM:DATA ASC';  // Datenformat: ASCII
  C_KEITHLEY_MEAS_ONE       = 'INIT:CONT OFF';  // one-shot measurement mode
  C_KEITHLEY_MEAS_CONTINU   = 'INIT:CONT ON';   // continuouse measurement mode
  C_KEITHLEY_MEAS_READ      = 'READ?';          // Einzelmessung triggern
  C_KEITHLEY_DATA_ASK       = 'DATA?';          // Daten lesen
  C_KEITHLEY_SET_FUNC       = 'FUNC "%s"';      // set function to messure
  C_KEITHLEY_FUNC_ASK       = 'FUNC?';          // ask for the current function of the messurement
  C_KEITHLEY_RANGE_SET      = ':RANG %s';       // set range of the measurement, it must be used with measurement function together
  C_KEITHLEY_RANGE_AUTO_ON  = ':RANG:AUTO ON';  // turn on automatical range
  C_KEITHLEY_RANGE_AUTO_OFF = ':RANG:AUTO OFF'; // trun off automatical range
  C_KEITHLEY_OVERFLOW       = '+9.9E37';        // overflowing data

  C_KEITHLEY_FUNC: array[EMeasureAction] of string = (
                  'ANY',
                  'RES',
                  'VOLT:DC',
                  'VOLT:AC',
                  'CURR:DC',
                  'CURR:AC',
                  'FREQ',
                  'PER',
                  'TEMP'
                  );

  C_KEITHLEY_UNITS: array[EMeasureAction] of string = (
                  'ANY',
                  'OHM',
                  'VDC',
                  'VAC',
                  'ADC',
                  'AAC',
                  'HZ',
                  'SEC',
                  '°C'
                  );

  C_KEITHLEY_RANGE: array[EMeasureAction] of single = (
                  0.0,    //not in use
                  120.0e6,
                  1010.0,
                  757.5,
                  3.0,
                  3.0,
                  0.0,     //not in use
                  0.0,     //not in use
                  0.0      //not in use
                  );

function TMultimeter.InitFromFile(const sfile: string): boolean;
begin
  result := false;
  t_msgrimpl.AddMessage(format('"%s.InitFromFile" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.SwitchMeasurement(const meas: EMeasureAction): boolean;
begin
  result := (meas <> MA_ANY);
  //t_msgrimpl.AddMessage(format('"%s.SwitchMeasurement" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.ReadData(var val: double): boolean;
begin
  result := false;
  t_msgrimpl.AddMessage(format('"%s.ReadData" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

procedure TMultimeter.TriggerMesssure();
begin
  //todo: if it's neccessary
end;

constructor TMultimeter.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_curma := MA_ANY;
end;

destructor TMultimeter.Destroy;
begin
	inherited Destroy;
end;

function TMultimeter.MeasureR(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_RES) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCV(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACV(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCI(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACI(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureF(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_FREQ) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureP(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_PERI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureT(var val: double): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_TEMP) then result := ReadData(val)
  else result := false;
end;

procedure TMultimeter.SetMeasureRange(const meas:EMeasureAction; const range: double);
begin
  t_msgrimpl.AddMessage(format('"%s.SetMeasurementRange" must be reimplemented in its subclass.', [ClassName()]), ML_WARNING);
end;

function TMultimeterKeithley.SwitchMeasurement(const meas: EMeasureAction): boolean;
var s_sending, s_recv: string;
begin
  result := inherited SwitchMeasurement(meas);
  if result then begin
    e_curma := MA_ANY;
    //ask HW for current measurement
    result := t_curconn.SendStr(C_KEITHLEY_FUNC_ASK + Char(13));
    if result then begin
      t_curconn.ExpectStr(s_recv, '"' + Char(13), false); //wait for string like '..."..."#D'
      if ContainsText(s_recv, C_KEITHLEY_FUNC[meas]) then
        e_curma := meas
      else begin //switch measurement if current measurement is different from the needed measurement
        s_sending := format(C_KEITHLEY_SET_FUNC, [C_KEITHLEY_FUNC[meas]]) + Char(13);
        result := t_curconn.SendStr(s_sending);
        if result then begin
          TGenUtils.Delay(100); //wait for switching
          if t_curconn.SendStr(C_KEITHLEY_FUNC_ASK + Char(13)) then begin
            if t_curconn.ExpectStr(s_recv, '"' + C_KEITHLEY_FUNC[meas] + '"' + Char(13), false) then begin
              e_curma := meas;
              t_msgrimpl.AddMessage(format('Successful to switch to the measurement(%s).', [C_KEITHLEY_FUNC[meas]]));
            end else
              t_msgrimpl.AddMessage(format('Failed to switch to the measurement(%s).', [C_KEITHLEY_FUNC[meas]]), ML_ERROR);
          end;
        end;
      end;
    end;
  end;
  result := (e_curma = meas);
end;

//return the value string of keithley reading element
//NOTE: the measurement data is composed of several elements, e.g.
//+4.69781780E+00OHM,+8179.250SECS,+46802RDNG#,000,0000LIMITS
//READingUnit,TSTampSECS,RNUMberRDNG#,CHANnel,LIMitsLIMITS
//this function gives back the first value string without unit
function TMultimeterKeithley.ReadingValue(const elem: string): string;
var i_pos: integer;
begin
  i_pos := AnsiPos(',', elem);
  if (i_pos > 0) then result := trim(AnsiLeftStr(elem, i_pos - 1))
  else result := trim(elem);

  if EndsText(C_KEITHLEY_UNITS[e_curma], result) then result := AnsiLeftStr(result, length(result) - length(C_KEITHLEY_UNITS[e_curma]));
  //keithley multimeter sends data only with '.' as decimal separator
  //it has to be changed into the local format
  result := TGenUtils.ReplaceDecimalSeparator(result);
end;

function TMultimeterKeithley.ReadData(var val: double): boolean;
var s_recv: string; c_retrial: cardinal;
begin
  c_retrial := 0;
  repeat
    result := t_curconn.SendStr(C_KEITHLEY_MEAS_READ + Char(13));
    if result then begin
      result := t_curconn.ExpectStr(s_recv, Char(13), false);
      if result then begin
        if StartsText(C_KEITHLEY_OVERFLOW, s_recv) then begin
          TGenUtils.Delay();
          result := false;
          t_msgrimpl.AddMessage(format('The measurement data(%s) is overflowed.', [C_KEITHLEY_OVERFLOW]), ML_ERROR);
        end else begin
          s_recv := ReadingValue(s_recv); //trim(s_recv);
          result := TryStrToFloat(s_recv, val);
          if result then t_msgrimpl.AddMessage(format('Successful to convert data %0.5f %s.', [val, C_KEITHLEY_UNITS[e_curma]]))
          else t_msgrimpl.AddMessage(format('Failed to convert data %s', [s_recv]), ML_ERROR);
        end;
      end;
    end;
    inc(c_retrial);
  until (result or (c_retrial >= C_MEAS_RETRIAL));
end;

function TMultimeterKeithley.GetRelayCards(): integer;
begin
  result := t_relay.CardSlots;
end;

constructor TMultimeterKeithley.Create(owner: TComponent);
begin
  inherited Create(owner);
  t_relay := TRelayKeithley.Create();
end;

destructor TMultimeterKeithley.Destroy();
begin
  t_relay.Free();
  inherited Destroy();
end;

function TMultimeterKeithley.InitDevice(): boolean;
begin
  e_curma := MA_ANY; s_idn := '';
  if assigned(t_curconn) then begin
    result := t_curconn.Connect();
    t_relay.CurConnect := t_curconn;
  end else
    result := false;

  if result then begin //get identifer string from the device
    result := t_curconn.SendStr(C_KEITHLEY_ID_QUERY + Char(13));
    if result then begin
      result := t_curconn.ExpectStr(s_idn, Char(13), false);
      if result then result := ContainsText(s_idn, 'KEITHLEY');
      
      if result then begin
        s_idn := trim(s_idn);
        //t_curconn.SendStr(C_KEITHLEY_RESET + Char(13));
        t_curconn.SendStr(C_KEITHLEY_CLEAR_ERROR + Char(13));
        t_curconn.SendStr(C_KEITHLEY_BEEP_OFF + Char(13));
        t_curconn.SendStr(C_KEITHLEY_FORMAT_ELEMENT + Char(13));
        t_curconn.SendStr(C_KEITHLEY_FORMAT_ASCII + Char(13));
        t_curconn.SendStr(C_KEITHLEY_MEAS_ONE + Char(13));
      end;
    end;
  end;

  if result then
    t_msgrimpl.AddMessage('Successful to initialize device.')
  else
    t_msgrimpl.AddMessage('Failed to initialize device.', ML_ERROR);
end;

function TMultimeterKeithley.ReleaseDevice(): boolean;
begin
  if assigned(t_curconn) then FreeAndNil(t_curconn);
  result := inherited ReleaseDevice();
end;

procedure TMultimeterKeithley.SetMeasureRange(const meas:EMeasureAction; const range: double);
var s_range, s_sending: string;
begin
  if (meas in [MA_RES, MA_DCV, MA_ACV, MA_DCI, MA_ACI])  then begin
    if (range > 0.0)then begin //set range
      if range > C_KEITHLEY_RANGE[meas] then s_range := FloatToStr(C_KEITHLEY_RANGE[meas])
      else s_range := FloatToStr(range);
      //keithley multimeter accepts only '.'- decimal separator
      //the local format has to be changed into the keithley format
      //s_range := ReplaceStr(s_range, DecimalSeparator, '.');
      s_range := TGenUtils.ReplaceDecimalSeparator(s_range);
      s_sending := Format(C_KEITHLEY_FUNC[meas] + C_KEITHLEY_RANGE_SET + AnsiChar(13), [s_range]);
    end else //auto range
      s_sending := C_KEITHLEY_FUNC[meas] + C_KEITHLEY_RANGE_AUTO_ON + AnsiChar(13);

    t_curconn.SendStr(s_sending);
  end;
end;

end.

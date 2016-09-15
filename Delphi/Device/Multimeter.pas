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
    function MeasureR(var val: single): boolean;
    function MeasureDCV(var val: single): boolean;
    function MeasureACV(var val: single): boolean;
    function MeasureDCI(var val: single): boolean;
    function MeasureACI(var val: single): boolean;
    function MeasureF(var val: single): boolean;
    function MeasureP(var val: single): boolean;
    function MeasureT(var val: single): boolean;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: single = 0);
  end;

  TMultimeter = class(TDeviceBase, IMultimeter, ITextMessengerImpl)
  protected
    e_cont :  EContinueMode;        //indicate if continue mode is shut on
    e_curma:  EMeasureAction;       //current setting for the measurement
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface TTextMessengerImpl         l
  protected
    function InitFromFile(const sfile: string): boolean; virtual;
    function SwitchMeasurement(const meas: EMeasureAction): boolean; virtual;
    function ReadData(var val: single): boolean; virtual;
    procedure TriggerMesssure(); virtual;
    procedure SetMessengerReim(tmessenger: TTextMessenger); virtual;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    procedure ITextMessengerImpl.SetMessenger = SetMessengerReim;
    property ContinueMode : EContinueMode read e_cont write e_cont;

    function MeasureR(var val: single): boolean; virtual;
    function MeasureDCV(var val: single): boolean; virtual;
    function MeasureACV(var val: single): boolean; virtual;
    function MeasureDCI(var val: single): boolean; virtual;
    function MeasureACI(var val: single): boolean; virtual;
    function MeasureF(var val: single): boolean; virtual;
    function MeasureP(var val: single): boolean; virtual;
    function MeasureT(var val: single): boolean; virtual;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: single); virtual;
  end;

  TMultimeterKeithley = class(TMultimeter, IRelayControl)
  protected
    t_relay:  TRelayKeithley;
  protected
    function SwitchMeasurement(const meas: EMeasureAction): boolean; override;
    function ReadData(var val: single): boolean; override;
    //procedure TriggerMesssure(); virtual;
    procedure SetMessengerReim(tmessenger: TTextMessenger); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;

    property RelayControl: TRelayKeithley read t_relay implements IRelayControl;

    function InitDevice(const devconf: TConfigBase): boolean; override;
    procedure SetMeasureRange(const meas:EMeasureAction; const range: single); override;
  end;
implementation
uses SysUtils, GenUtils, StrUtils, RS232;

const
  // Keithley-Multimert 2700
  C_KEITHLEY_BEEP_OFF       = 'SYST:BEEP 0';    // Beep off
  C_KEITHLEY_CLEAR_ERROR    = '*CLS';           // alle 'Event register' und 'error queue' loeschen
  C_KEITHLEY_SELF_TEST      = '*TST?';          // Eigentest
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
  result := (Ord(meas) > Ord(MA_ANY)) and (Ord(meas) <= Ord(MA_TEMP));
  //t_msgrimpl.AddMessage(format('"%s.SwitchMeasurement" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.ReadData(var val: single): boolean;
begin
  result := false;
  t_msgrimpl.AddMessage(format('"%s.ReadData" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

procedure TMultimeter.TriggerMesssure();
begin
  //todo: if it's neccessary
end;

procedure TMultimeter.SetMessengerReim(tmessenger: TTextMessenger);
begin
  t_msgrimpl.Messenger := tmessenger;
  ITextMessengerImpl(t_curconn).Messenger := t_msgrimpl.Messenger;
end;

constructor TMultimeter.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_curma := MA_ANY;
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
  t_curconn := TMtxRS232.Create(self); //test
  t_curconn.Config('Port:5|baudrate:9600'); //test
  t_curconn.Connect();
end;

destructor TMultimeter.Destroy;
begin
  t_curconn.Free(); //test
  t_msgrimpl.Free();
	inherited Destroy;
end;

function TMultimeter.MeasureR(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_RES) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCV(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACV(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACV) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureDCI(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_DCI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureACI(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_ACI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureF(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_FREQ) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureP(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_PERI) then result := ReadData(val)
  else result := false;
end;

function TMultimeter.MeasureT(var val: single): boolean;
begin
  val := 0.0;
  if SwitchMeasurement(MA_TEMP) then result := ReadData(val)
  else result := false;
end;

procedure TMultimeter.SetMeasureRange(const meas:EMeasureAction; const range: single);
begin
  t_msgrimpl.AddMessage(format('"%s.SetMeasurementRange" must be reimplemented in its subclass.', [ClassName()]), ML_WARNING);
end;

procedure TMultimeterKeithley.SetMessengerReim(tmessenger: TTextMessenger);
begin
  inherited SetMessengerReim(tmessenger);
  ITextMessengerImpl(t_relay).Messenger := t_msgrimpl.Messenger;
end;

function TMultimeterKeithley.SwitchMeasurement(const meas: EMeasureAction): boolean;
var s_sending, s_recv: string;
begin
  result := inherited SwitchMeasurement(meas);
  if result then begin
    if (e_curma <> meas) then begin
      s_sending := format(C_KEITHLEY_SET_FUNC, [C_KEITHLEY_FUNC[meas]]) + Char(13);
      t_curconn.SendStr(s_sending);
    end;
    t_curconn.SendStr(C_KEITHLEY_FUNC_ASK + Char(13));
    if t_curconn.ExpectStr(s_recv, Char(13), false) then begin
      s_recv := TGenUtils.ClearQuotationMarks(trim(s_recv));
      result := SameText(s_recv, C_KEITHLEY_FUNC[meas]);
      if result then e_curma := meas
      else e_curma := MA_ANY;
    end else result := false;
  end;
  if result then
    t_msgrimpl.AddMessage(format('Successful to switch to the measurement(%s).', [C_KEITHLEY_FUNC[meas]]))
  else
    t_msgrimpl.AddMessage(format('Failed to switch to the measurement(%s).', [C_KEITHLEY_FUNC[meas]]), ML_ERROR);
end;

function TMultimeterKeithley.ReadData(var val: single): boolean;
var s_recv: string;
begin
  result := t_curconn.SendStr(C_KEITHLEY_MEAS_READ + Char(13));
  if result then begin
    result := t_curconn.ExpectStr(s_recv, Char(13), false);
    if result then begin
      s_recv := trim(s_recv);
      s_recv := ReplaceStr(s_recv, '.', DecimalSeparator);
      result := TryStrToFloat(s_recv, val);
      if result then t_msgrimpl.AddMessage(format('Successful to convert data %f.', [val]))
      else t_msgrimpl.AddMessage(format('Failed to convert data %s', [s_recv]));
    end;
  end;
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

function TMultimeterKeithley.InitDevice(const devconf: TConfigBase): boolean;
begin
  result := t_curconn.Connect;
  t_relay.CurConnect := t_curconn;
  if result then result := t_curconn.SendStr(C_KEITHLEY_CLEAR_ERROR + Char(13));
  if result then result := t_curconn.SendStr(C_KEITHLEY_BEEP_OFF + Char(13));
  if result then result := t_curconn.SendStr(C_KEITHLEY_FORMAT_ELEMENT + Char(13));
  if result then result := t_curconn.SendStr(C_KEITHLEY_FORMAT_ASCII + Char(13));
  if result then result := t_curconn.SendStr(C_KEITHLEY_MEAS_ONE + Char(13));
end;

procedure TMultimeterKeithley.SetMeasureRange(const meas:EMeasureAction; const range: single);
var s_range, s_sending: string;
begin
  if (meas in [MA_RES, MA_DCV, MA_ACV, MA_DCI, MA_ACI])  then begin
    if (range > 0)then begin //set range
      if range > C_KEITHLEY_RANGE[meas] then s_range := FloatToStr(C_KEITHLEY_RANGE[meas])
      else s_range := FloatToStr(range);
      s_range := ReplaceStr(s_range, DecimalSeparator, '.');
      s_sending := format(C_KEITHLEY_FUNC[meas] + C_KEITHLEY_RANGE_SET, [s_range]) + Char(13);
    end else //auto range
      s_sending := C_KEITHLEY_FUNC[meas] + C_KEITHLEY_RANGE_AUTO_ON + Char(13);

    t_curconn.SendStr(s_sending);
  end;
end;

end.

unit Multimeter;

interface
uses DeviceBase, IniFiles, Classes, RelayControl, TextMessage;

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

  TMultimeter = class(TDeviceBase, IMultimeter, ITextMessengerImpl)
  protected
    e_cont :  EContinueMode;        //indicate if continue mode is shut on
    e_curma:  EMeasureAction;       //current setting for the measurement
    t_msgrimpl: TTextMessengerImpl; //for delegation of interface TTextMessengerImpl         l
  protected
    function InitFromFile(const sfile: string): boolean; virtual;
    function SwitchMeasurement(const meas: EMeasureAction): boolean; virtual;
    function ReadData(var val: real): boolean; virtual;
    procedure TriggerMesssure(); virtual;
    procedure SetMessengerReim(tmessenger: TTextMessenger); virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    procedure ITextMessengerImpl.SetMessenger = SetMessengerReim;
    property ContinueMode : EContinueMode read e_cont write e_cont;

    function InitMeasurement(): boolean; virtual;
    function MeasureR(var val: real): boolean; virtual;
    function MeasureDCV(var val: real): boolean; virtual;
    function MeasureACV(var val: real): boolean; virtual;
    function MeasureDCI(var val: real): boolean; virtual;
    function MeasureACI(var val: real): boolean; virtual;
    function MeasureF(var val: real): boolean; virtual;
    function MeasureP(var val: real): boolean; virtual;
    function MeasureT(var val: real): boolean; virtual;
  end;

  TMultimeterKeithley = class(TMultimeter, IRelayControl)
  protected
    t_relay:  TRelayKeithley;
  protected
    function SwitchMeasurement(const meas: EMeasureAction): boolean; override;
    //function ReadData(var val: real): boolean; virtual;
    //procedure TriggerMesssure(); virtual;
    procedure SetMessengerReim(tmessenger: TTextMessenger); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy(); override;

    property RelayControl: TRelayKeithley read t_relay implements IRelayControl;

    function InitMeasurement(): boolean; override;
  end;
implementation
uses SysUtils;

const
  // Keithley-Multimert 2700
  C_KEITHLEY_BEEP_OFF       = 'SYST:BEEP 0';     // Beep off
  C_KEITHLEY_CLEAR_ERROR    = '*CLS';            // alle 'Event register' und 'error queue' loeschen
  C_KEITHLEY_SELF_TEST      = '*TST?';           // Eigentest
  C_KEITHLEY_FORMAT_ELEMENT = 'FORM:ELEM READ';  // Datenformat spezifizieren
  C_KEITHLEY_MEAS_ONE       = 'INIT:CONT OFF';   // one-shot measurement mode
  C_KEITHLEY_MEAS_CONTINU   = 'INIT:CONT ON';    // continuouse measurement mode
  C_KEITHLEY_MEAS_READ      = 'READ?';           // Einzelmessung triggern
  C_KEITHLEY_DATA_ASK       = 'DATA?';           // Daten lesen
  C_KEITHLEY_SET_FUNC       = 'FUNC "%s"';       // set function to messure
  C_KEITHLEY_FUNC_ASK       = 'FUNC?';           // ask for the current function of the messurement

  C_KEITHLEY_FUNC: array[EMeasureAction] of string = (
                  'RES',
                  'VOLT:DC',
                  'VOLT:AC',
                  'CURR:DC',
                  'CURR:AC',
                  'FREQ',
                  'PER',
                  'TEMP'
                  );

function TMultimeter.InitFromFile(const sfile: string): boolean;
begin
  result := false;
  t_msgrimpl.AddMessage(format('"%s.InitFromFile" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.SwitchMeasurement(const meas: EMeasureAction): boolean;
begin
  result := false;
  t_msgrimpl.AddMessage(format('"%s.SwitchMeasurement" must be reimplemented in its subclass.', [ClassName()]), ML_ERROR);
end;

function TMultimeter.ReadData(var val: real): boolean;
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
  t_msgrimpl := TTextMessengerImpl.Create();
end;

destructor TMultimeter.Destroy;
begin
  t_msgrimpl.Free();
	inherited Destroy;
end;

function TMultimeter.InitMeasurement(): boolean;
begin
  result := true;
  t_msgrimpl.AddMessage(format('Nothing is done in the default function "%s.InitMeasurement".', [ClassName()]), ML_WARNING);
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

procedure TMultimeterKeithley.SetMessengerReim(tmessenger: TTextMessenger);
begin
  inherited SetMessengerReim(tmessenger);
  ITextMessengerImpl(t_relay).Messenger := t_msgrimpl.Messenger;
end;

function TMultimeterKeithley.SwitchMeasurement(const meas: EMeasureAction): boolean;
var s_sending: string;
begin
  s_sending := format(C_KEITHLEY_SET_FUNC, [C_KEITHLEY_FUNC[meas]]) + Char(13);
  result := t_curconn.SendStr(s_sending);
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

function TMultimeterKeithley.InitMeasurement(): boolean;
begin
  result := false;
  //todo:
end;

end.

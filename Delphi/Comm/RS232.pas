unit RS232;

interface
uses  Serial3, ConnBase, DataBuffer, Classes, RegExpr;

type
  ERS232Settings = (
                    RS_PORT,
                    RS_BAUDRATE,
                    RS_PARITY,
                    RS_DATABITS,
                    RS_STOPBITS,
                    RS_HWFLOWCONTROL,
                    RS_SWFLOWCONTROL
                    );

  TConnRS232 = class(TConnBase)
  class function EnumSerialPort(var sports: TStringList): integer;
  protected
    t_ser : TSerial;
  protected
    function CheckPort(const sport: string; var port: integer): boolean;
    function CheckBaudrate(const sbaud: string; var baud: integer): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Config(const sconf: string): boolean;override;
    function IsConnected(): boolean; override;
    function Connect(): boolean;override;
    function Disconnect: boolean;override;
    function SendData(const data: TCharBuffer): Integer;override;
    function RecvData(var data: TCharBuffer): Integer;override;
    function GetLastError(var msg: string): Integer;override;
  end;
  PConnRS232 = ^TConnRS232;

const
  C_VALID_BAUD: array[0..14] of string = (
                '110','300','600','1200','2400','4800','9600','14400',
                '19200','38400','56000','57600','115200','128000','256000');
                
  C_RS232_KEYS: array[LOW(ERS232Settings)..HIGH(ERS232Settings)] of string = (
                '^PORT:(\d{1,3})$',
                '^BAUDRATE:(\d{3,6})$',
                '^PARITY(\d)$',
                '^DATABITS(\d)$',
                '^STOPBITS(\d)$',
                '^HWFLOWCONTROL(\d)$',
                '^SWFLOWCONTROL(\d)$'
                );

implementation
uses SysUtils, StrUtils, Windows, GenUtils, Registry;

// =============================================================================
// Class        : TConnRS232
// Function     : EnumSerialPort, class function, enumerates all serial ports
//                from Windows-Registry
// Parameter    : sports, an ouput string list in which all ports are found, e.g.
//                ('COM1', 'COM2' ...)
// Return       : integer, count of the found ports
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
class function TConnRS232.EnumSerialPort(var sports: TStringList): integer;
var t_registry: TRegistry; s_names: TStringList; s_value: string; i: integer;
begin
  sports.Clear;
  t_registry := TRegistry.Create;
  with t_registry do begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Hardware\DeviceMap\SerialComm') then begin
      s_names := TStringList.Create;
      GetValueNames(s_names);
      for i := 0 to s_names.Count - 1 do begin
        s_value := ReadString(s_names[i]);
        s_value := UpperCase(trim(s_value));
        sports.Add(s_value);
      end;
      s_names.Clear;
      FreeAndNil(s_names);
      CloseKey();
    end;
  end;
  FreeAndNil(t_registry);
  result := sports.Count;
end;

function TConnRS232.CheckPort(const sport: string; var port: integer): boolean;
var s_portname: string; t_ports: TStringList; t_regexp: TRegExpr;
begin
  result := false;
  t_regexp := TRegExpr.Create;
  t_regexp.Expression := C_RS232_KEYS[RS_PORT];
  if t_regexp.Exec(sport) then begin
    TryStrToInt(t_regexp.Match[1], port);
    s_portname := 'COM' + t_regexp.Match[1];
    t_ports := TStringList.Create;
    Self.EnumSerialPort(t_ports);
    result := (t_ports.IndexOf(s_portname) >= 0 );
    t_ports.Clear;
    FreeAndNil(t_ports);
  end;
  FreeAndNil(t_regexp);
end;

function TConnRS232.CheckBaudrate(const sbaud: string; var baud: integer): boolean;
var t_regexp: TRegExpr;
begin
  result := false;
  t_regexp := TRegExpr.Create;
  t_regexp.Expression := C_RS232_KEYS[RS_BAUDRATE];
  if t_regexp.Exec(sbaud) then  result := (IndexOfStr(C_VALID_BAUD, t_regexp.Match[1]) >= 0 );
  FreeAndNil(t_regexp);
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Constructor, creates t_ser
// Parameter    :
// Return       :
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
constructor TConnRS232.Create;
begin
  inherited Create();
  t_ser := TSerial.Create(Nil);
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Destructor, destroys t_ser
// Parameter    :
// Return       :
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
destructor TConnRS232.Destroy;
begin
  FreeAndNil(t_ser);
  inherited Destroy();
end;

// =============================================================================
// Class        : TConnRS232
// Function     : IsConnected
//                request, if the object of TSerial is actived
// Parameter    :
// Return       : value of s_ser.active (true or false)
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TConnRS232.IsConnected(): boolean;
begin
  result := t_ser.Active;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Config
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                'Port:8|Baudrate:115200'
// Return       : true, if the setting is available(At least PORT and BAUDRATE
//                      have to be set configured)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TConnRS232.Config(const sconf: string): boolean;
var s_conf: string;
begin
  result := false;
  s_conf := UpperCase(sconf);
end;

function TConnRS232.Connect(): boolean;
begin
  t_ser.Active := true;
  result := IsConnected();
end;

function TConnRS232.Disconnect: boolean;
begin
  t_ser.Active := true;
  result := (not IsConnected());
end;

function TConnRS232.SendData(const data: TCharBuffer): Integer;
begin
  result := 0;
end;

function TConnRS232.RecvData(var data: TCharBuffer): Integer;
begin
  result := 0;
end;

function TConnRS232.GetLastError(var msg: string): Integer;
begin
  result := i_lasterr;
end;


end.

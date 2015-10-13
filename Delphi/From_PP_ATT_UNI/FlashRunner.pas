// =============================================================================
// Module name  : $RCSfile: FlashRunner.pas,v $
// description  : This unit implements all methodes and properties of class
//                TFlashRunner for device FlashRunner
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit FlashRunner;

interface
uses Serial3, Classes, SysUtils, StrUtils, Windows, Forms, DeviceBase, IniFiles;

type

  PSerial = ^TSerial;

  ESerialProperty = (
                    SP_PORT,
                    SP_BAUDRATE,
                    SP_PARITY,
                    SP_DATABITS,
                    SP_STOPBITS,
                    SP_HWFLOWCONTROL,
                    SP_SWFLOWCONTROL
                    );

  EFlashRunnerIni = (
                    FRI_TIMEOUT,
                    FRI_CONNSTR
                    );

  // =============================================================================
  // Class        : TFlashRunner
  // Description  : Definition of class TFlashRunner for FlashRunner deriving
  //                from TDeviceBase
  // First author : 2015-08-14 /bsu/
  // History      :
  // =============================================================================
  TFlashRunner=class(TDeviceBase)
  protected
    t_ser: TSerial;
  protected
    procedure SetDefaults(); virtual;
    function CheckAnswer(const ans: string): boolean;
    function ConfigConnByStr(const conf: string): boolean;
    function SendStrTill(const data: string; const tend: cardinal): boolean;
    function RecvStrTill(var data: string; const tend: cardinal): integer;
  public
    constructor Create(owner: TComponent); override;
    destructor  Destroy; override;

    function ConfigDevice(const ini: TMemIniFile): Boolean; override;
    function FreeDevice(): Boolean; override;
    function Connect(): Boolean; override;
    function CheckComm(): boolean; override;
    function Disconnect: boolean; override;
    function SendStr(const data: string; const ans: boolean = true): boolean; override;
    function RecvStr(var data: string): integer; override;
    
    function IntToDMDataStr(const num: integer): string;
  end;

var t_flashrunner: TFlashRunner; //a global variable of the FlashRunner instance
const
  CSTR_FRI_SECT = 'FlashRunner';

  CSTR_FRI_KEYS: array[EFlashRunnerIni] of string = (
                'TIMEOUT',
                'CONN_RS232'
                );

  CSTR_VALID_BAUD: array[0..14] of string = (
                '110','300','600','1200','2400','4800','9600','14400',
                '19200','38400','56000','57600','115200','128000','256000');

  CSTR_RS232_KEYS: array[ESerialProperty] of string = (
                'PORT',
                'BAUDRATE',
                'PARITY',  //the value is not yet in use
                'DATABITS',  //the value is not yet in use
                'STOPBITS',  //the value is not yet in use
                'HWFLOWCONTROL', //the value is not yet in use
                'SWFLOWCONTROL' //the value is not yet in use
                );

implementation

uses  Registry, RegExpr;
type  TSetSerialProperty = function(pser: PSerial; const sval: string): boolean;
var   PSerialPropertyCalls: array [SP_PORT..SP_BAUDRATE] of TSetSerialProperty;

// =============================================================================
// Class        : --
// Function     : IndexOfStr
//                return index of a string in an array
// Parameter    : aArray, in which the string is searched
//                S, string to search
// Return       : index of string in the array. -1 is returnd, if not found
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function IndexOfStr(const aArray: array of string; const S: string): integer;
var I: Integer;
begin
  Result := -1;
  for I := Low(aArray) to High(aArray) do
    if SameText(S, aArray[I]) then begin
      Result := I;
      Break;
    end;
end;

// =============================================================================
// Class        : --
// Function     : EnumSerialPort, enumerates all serial ports
//                from Windows-Registry
// Parameter    : sports, an ouput string list in which all ports are found, e.g.
//                ('COM1', 'COM2' ...)
// Return       : integer, count of the found ports
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function EnumSerialPort(var sports: TStringList): integer;
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


// =============================================================================
// Class        : --
// Function     : checks the given string and sets the port of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for port to be set
// Return       : true, if the string is a valid number of the port on this comuputer
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetPort(pser: PSerial; const sval: string): boolean;
var t_ports: TStringList; s_in,s_portname: string; i_port: integer;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(s_in, i_port) then begin
    s_portname := 'COM' + sval;
    t_ports := TStringList.Create;
    EnumSerialPort(t_ports);
    result := (t_ports.IndexOf(s_portname) >= 0 );
    if result then pser^.Port := i_port;
    t_ports.Clear;
    FreeAndNil(t_ports);
  end;
end;

// =============================================================================
// Class        : --
// Function     : SetBaudrate
//                checks the given string and sets the baudrate of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for baudrate to be set
// Return       : true, if the string is a number of the valid baudrate (C_VALID_BAUD)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetBaudrate(pser: PSerial; const sval: string): boolean;
var i_baud: integer; s_in: string;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(sval, i_baud) then begin
    result := (IndexOfStr(CSTR_VALID_BAUD, sval) >= 0 );
    if result then pser^.Baudrate := i_baud;
  end;
end;

// =============================================================================
// Class        : --
// Function     : Delay
//                delay for a moment in millisecond
// Parameter    : msec, integer of milli seconds
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure Delay(const msec: cardinal);
var i_timeout: cardinal;
begin
  i_timeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() >= i_timeout);
end;

// =============================================================================
// Class        : TFlashRunner
// procedure    : SetDefaults, set default settings
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2015-10-12 /bsu/
// History      : 
// =============================================================================
procedure TFlashRunner.SetDefaults();
begin
  c_timeout := C_TIMEOUT_MSEC;
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
  t_ser.Port := 1;
  t_ser.Baudrate := 115200;
  t_ser.Name := CSTR_FRI_SECT;
end;


// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigConnByStr
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                'Port:8|Baudrate:115200'
// Return       : true, if the setting is available.
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-03 /bsu/ return true, only if PORT and BAUDRATE are both configured
// =============================================================================
function TFlashRunner.ConfigConnByStr(const conf: string): boolean;
var s_conf: string; i: ESerialProperty; t_regexp: TRegExpr;
    b_settings: array[SP_PORT..SP_BAUDRATE] of boolean;
begin
  s_conf := UpperCase(conf);
  t_regexp := TRegExpr.Create;
  for i := SP_PORT to SP_BAUDRATE do b_settings[i]:=false;
  for i := SP_PORT to SP_BAUDRATE do begin
    t_regexp.Expression := '(^|\|)[\t\ ]*' + CSTR_RS232_KEYS[i] + '\b[\t\ ]*:([^\|$]*)';
    if t_regexp.Exec(s_conf) then  begin
      result := (PSerialPropertyCalls[i](@t_ser, t_regexp.Match[2]));
      b_settings[i]:= result;
      if not result then begin
        s_lastmsg := t_regexp.Match[0];
        break;
      end;
    end else begin
      s_lastmsg := CSTR_RS232_KEYS[i] + ' NOT found';
      break;
    end;
  end;
  FreeAndNil(t_regexp);
  result := (b_settings[SP_PORT] and b_settings[SP_BAUDRATE]);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : IntToDMDataStr
//                convert an integer into a string with format of data
//                for FlashRunner-command DMSET in word-addressing,
//                e.g. 1050820084($3EA23DF4)->'$3E $00 $A2 $00 $3D $00 $F4 $00'
// Parameter    : num, integer to be converted
// Return       : string in format of DMSET-data
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.IntToDMDataStr(const num: integer): string;
var str_data: string;
begin
  str_data := RightStr(IntToHex(num, 8), 8);
  Insert(' $00 $', str_data, 7);
  Insert(' $00 $', str_data, 5);
  Insert(' $00 $', str_data, 3);
  result := '$' + str_data + ' $00';
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Create
//                constructor of the class, which initializes the instance with
//                default values
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
constructor TFlashRunner.Create(owner: TComponent);
begin
	inherited Create(owner);
  t_ser := TSerial.Create(self);
  SetDefaults();
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Destroy
//                destructor of the class, which finilizes the instance
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
destructor TFlashRunner.Destroy;
begin
  FreeDevice();
  FreeAndNil(t_ser);
	inherited Destroy;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Connect
//                connects to the device (FlashRunner)
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES
// =============================================================================
function TFlashRunner.Connect: boolean;
begin
  ClearError();
  if (e_state in C_DEV_INSTATES[DE_CONNECT]) then begin
    if not t_ser.Active then t_ser.Active := true;
    result := t_ser.Active;
    if not result then begin
      i_lasterr := C_DEV_ERR_CONN;
      s_lastmsg := 'failed to activate the connection';
    end;
    PostEvent(DE_CONNECT, result);
  end else begin
    result := (e_state in C_DEV_OUTSTATES[DE_CONNECT]);
    if not result then SetStateError();
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : CheckComm, reimplimentation of virtual function
//                check with query string 'SPING', if the communication is possible
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES, change function name
//                            call internal functions to send and receive string
// =============================================================================
function TFlashRunner.CheckComm(): boolean;
const C_STR_PING: string = 'SPING'; C_STR_PONG: string = 'PONG>'; C_TIMEOUT: cardinal = 3000;
var s_ans: string; c_time: cardinal;
begin
  result := false; ClearError();
  if (e_state in C_DEV_INSTATES[DE_CHECKCOMM]) then
  begin
    c_time := GetTickCount() + C_TIMEOUT;
    result := SendStrTill(C_STR_PING + Char(13), c_time);
    if result then begin
      s_ans := '';
      RecvStrTill(s_ans, c_time);
      result := (i_lasterr = C_DEV_ERR_OK);
      if result then begin
        //verify the received string
        s_ans := trim(s_ans);
        result := SameText(C_STR_PONG,s_ans);
        if not result then begin
          i_lasterr := C_DEV_ERR_ANSW;
          s_lastmsg := 'answer:' + s_ans + '; expected:' + C_STR_PING ;
        end;
      end;
    end;
    PostEvent(DE_CHECKCOMM, result);
  end else SetStateError();
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SendStrTill, an internal function
//                send string to device till GetTickCount is greater than the given time
// Parameter    : str, a string to send
//                tend, time point in millisecond to stop sending
// Return       : true, if data is sent successfully. Otherweise false
// Exceptions   : --
// First author : 2015-09-29 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SendStrTill(const data: string; const tend: cardinal): boolean;
var ch: char; b_timeout: boolean;
begin
  //clear read-buffer of t_ser
  repeat if (t_ser.ReadChar(ch) <> 1) then Delay(C_DELAY_ONCE);
  until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= tend));

  t_ser.WriteString(data);
  //wait til write-buffer is completely sent
  repeat
    if (t_ser.TxWaiting > 0) then Delay(C_DELAY_ONCE);
    b_timeout := (GetTickCount() > tend);
  until ((t_ser.TxWaiting <= 0) or b_timeout);
  result := ((t_ser.TxWaiting <= 0) and (not b_timeout));
  if b_timeout then begin
    i_lasterr := C_DEV_ERR_TIMEOUT;
    s_lastmsg := 'sending:'+ data;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RecvStrTill, an internal function
//                receives string from device till GetTickCount is greater than the given time
// Parameter    : str, a string to send
//                tend, time point in millisecond to stop sending
// Return       : integer, count of the received chars
// Exceptions   : --
// First author : 2015-09-29 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RecvStrTill(var data: string; const tend: cardinal): integer;
var ch: char; b_timeout: boolean;
begin
  result := 0;
  //wait til any data arrive and receive it
  while ((t_ser.RxWaiting <= 0) and (GetTickCount() < tend)) do Delay(C_DELAY_ONCE);
  //receive char for char from t_ser
  repeat
    ch := chr(0);
    if (t_ser.ReadChar(ch) = 1) then begin
      data := data + ch;
      inc(result);
    end else Delay(C_DELAY_ONCE);
    b_timeout := (GetTickCount() > tend);
  until ((t_ser.RxWaiting <= 0) or b_timeout);
  if b_timeout then begin
    i_lasterr := C_DEV_ERR_TIMEOUT;
    s_lastmsg := 'received:'+ data;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SendStr
//                send string to device
// Parameter    : str, a string to send
//                ans, indicates if an answer is demanded for this query 
// Return       : true, if data is sent successfully. Otherweise false
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES, return boolean value.
//                            call internal function to send string
// =============================================================================
function TFlashRunner.SendStr(const data: string; const ans: boolean): boolean;
var c_time: cardinal;
begin
  result := false; ClearError();
  if (e_state in C_DEV_INSTATES[DE_SEND]) then begin
    c_time := GetTickCount() + c_timeout;
    result := SendStrTill(data, c_time);
    if not result then i_lasterr := C_DEV_ERR_SEND;
    PostEvent(DE_SEND, result);
    if (not ans) then PostEvent(DE_RECV, true);
  end else SetStateError();
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RecvStr
//                receiv string from device
// Parameter    : str, a string for receiving
// Return       : integer, which counts char, which are received
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES and call internal
//                            function to receive string
// =============================================================================
function TFlashRunner.RecvStr(var data: string): integer;
var c_time: cardinal; b_ok: boolean;
begin
  result := 0; ClearError();
  if (e_state in C_DEV_INSTATES[DE_RECV]) then begin
    //prepare
    c_time := GetTickCount() + c_timeout;
    data := '';
    result := RecvStrTill(data, c_time);
    b_ok := (i_lasterr = C_DEV_ERR_OK);
    //verify result
    if b_ok then begin
      b_ok := CheckAnswer(data);
      if not b_ok then i_lasterr := C_DEV_ERR_ANSW;
    end;
    PostEvent(DE_RECV, b_ok);
  end else SetStateError();
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : CheckAnswer
//                check, if an error exists in the answer string from device
// Parameter    : ans, the received string
// Return       : integer, error number
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.CheckAnswer(const ans: string): boolean;
var s_in: string; i_len: integer;
begin
  result := false;
  s_in := trim(ans);
  i_len := length(s_in);
  if (i_len > 0) then begin
    if (s_in[i_len] = '>') then result := true //FlashRunner command executes successfully
    else s_lastmsg := 'answer:' + ans; //FlashRunner command generates an error
  end else s_lastmsg := 'empty answer';
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigDevice
//                config FlashRunner using settings from INI like:
//                [FlashRunner]
//                DESCRIPTION=A universal programmer for writing flash
//                PRODUCER=SMH Technologies
//                TYPE=FR01LAN
//                CONN_RS232=PORT:8|Baudrate:115200
//                TIMEOUT=30000
// Parameter    : ini, a instance of TMemIniFile, in which section [FlashRunner]
//                     is saved
// Return       : true, if the device is configured successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES
// =============================================================================
function TFlashRunner.ConfigDevice(const ini: TMemIniFile): boolean;
var s_inivalue: string; i: EFlashRunnerIni;
begin
  result := false; ClearError();
  if (e_state in C_DEV_INSTATES[DE_CONFIG]) then begin
    //settings from ini file
    if (ini.SectionExists(CSTR_FRI_SECT)) then begin
      for i := Low(EFlashRunnerIni) to High(EFlashRunnerIni) do begin
        if ini.ValueExists(CSTR_FRI_SECT, CSTR_FRI_KEYS[i]) then begin
          result := true;
          s_inivalue := ini.ReadString(CSTR_FRI_SECT, CSTR_FRI_KEYS[i], '');
          case i of
            FRI_TIMEOUT: result := SetTimeout(s_inivalue);
            FRI_CONNSTR: result := ConfigConnByStr(s_inivalue);
          end;
          if not result then s_lastmsg := CSTR_FRI_KEYS[i] + '=' + s_inivalue;
        end else begin
          result := false;
          s_lastmsg := CSTR_FRI_KEYS[i] + ' was NOT found';
        end;
        if not result then break;
      end;
    end else s_lastmsg := '''' + CSTR_FRI_SECT + ''' was NOT found';
    if not result then i_lasterr := C_DEV_ERR_CONF;
    PostEvent(DE_CONFIG, result);;
  end else SetStateError();
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Disconnect
//                disconnect from the device
// Parameter    : --
// Return       : true, if the device is disconnected successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Disconnect(): boolean;
begin
  ClearError();
  if (e_state in C_DEV_INSTATES[DE_DISCONNECT]) then begin
    t_ser.Active := false;
    result := (t_ser.Active = false);
    if not result then i_lasterr := C_DEV_ERR_DISC;
    PostEvent(DE_DISCONNECT, result);
  end else begin
    result := (e_state in C_DEV_OUTSTATES[DE_DISCONNECT]);
    if (not result) then SetStateError();
  end;
end;


// =============================================================================
// Class        : TFlashRunner
// Function     : FreeDevice
//                release device, undo what is done in ConfigDevice
// Parameter    : --
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.FreeDevice(): boolean;
begin
  ClearError();
  if (e_state in C_DEV_INSTATES[DE_FREE]) then begin
    result := Disconnect();
    if result then SetDefaults();
    PostEvent(DE_FREE, result);;
  end else begin
    result := (e_state in C_DEV_OUTSTATES[DE_FREE]);
    if (not result) then SetStateError();
  end;
end;

initialization
  PSerialPropertyCalls[SP_PORT]      := @SetPort;
  PSerialPropertyCalls[SP_BAUDRATE]  := @SetBaudrate;
end.

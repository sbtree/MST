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
uses Serial3, Classes, SysUtils, StrUtils, Windows, Forms,  IniFiles, ConnBase, RS232, DeviceBase;

type
// =============================================================================
// Class        : TFlashRunner
// Description  : Definition of class TFlashRunner for FlashRunner deriving
//                from TDeviceBase
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  TFlashRunner = class(TDeviceBase)
  protected
    //t_ser: TSerial;
  protected
    function CheckAnswer(): boolean; override;
    function Sync(): boolean; override;
    function IntToDMDataStr(const num: integer): string;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function GetLastError(var msg: string): Integer; override;
    function ConfigDevice(const ini: TMemIniFile): Boolean; override;

    function SetDynamicMem(const addr: word; const num: integer): boolean;
    function RunScript(const script: string; const msecs: integer = -1): boolean;
  end;

var t_flashrunner: TFlashRunner; //a global variable of the FlashRunner instance

implementation
const
  C_ERR_PORT      = $8001; //
  C_ERR_BAUDRATE  = $8002; //
  C_ERR_WRONG_ANS = $8003; //
  C_ERR_WRONG_STA = $8004; //
  C_ERR_TIMEOUT   = $8005; //

  CSTR_FR_SEC   : string = 'FlashRunner';

// =============================================================================
// Class        : --
// Function     : Delay
//                delay for a moment in milli second
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
var
  I: Integer;
begin
  Result := -1;
  for I := Low(aArray) to High(aArray) do
    if SameText(S, aArray[I]) then begin
      Result := I;
      Break;
    end;
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
  ReleaseDevice();
	inherited Destroy;
end;


// =============================================================================
// Class        : TFlashRunner
// Function     : Sync
//                coordinate with the device
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Sync(): boolean;
const C_STR_PING: string = 'SPING'; C_STR_PONG: string = 'PONG>';
var s_ans: string; ds_set : set of EDeviceState; i_len: integer;
begin
  result := false;
  ds_set := [DS_CONNECTED, DS_COMERROR];
  if (e_state in ds_set) then
  begin
    //clear receiving-buffer of t_ser
    t_conns[e_actconn].RecvData(t_rbuf, C_TIMEOUT_ONCE);
    t_rbuf.Clear;

    //send string and wait til write-buffer is completely sent
    i_len := t_wbuf.WriteStr(C_STR_PING + Char(13));
    if (t_conns[e_actconn].SendData(t_wbuf, C_TIMEOUT_ONCE) = i_len) then begin
      //receive string til read-buffer is empty or timeout
      i_len := t_conns[e_actconn].RecvData(t_rbuf, C_TIMEOUT_ONCE);
      if (i_len > 0) then begin
        //verify the received string
        s_ans := trim(t_rbuf.ReadStr());
        result := SameText(C_STR_PONG, s_ans);
        if result then e_state := DS_READY;
      end;
    end;
  end;
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
function TFlashRunner.CheckAnswer(): boolean;
const C_VALID_CHARS: set of char = ['>', '!'];
var s_ans: string; i_len: integer;
begin
  s_ans := trim(t_rbuf.ReadStr());
  i_len := length(s_ans);
  s_lastmsg := format('answer string: %s', [IfThen(i_len>0, s_ans, '[empty]')]);
  if (i_len > 0) then begin
    if (s_ans[i_len] = '>') then  i_lasterr := C_ERR_NOERROR //sucessful answer
    else if (s_ans[i_len] = '!') then begin //failure answer
      delete(s_ans, i_len, 1);
      TryStrToInt(s_ans, i_lasterr);
    end  else i_lasterr := C_ERR_UNKNOWN;
  end else  i_lasterr := C_ERR_WRONG_ANS;
  result := (i_lasterr = C_ERR_NOERROR);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : GetLastError
//                override function
// Parameter    : msg, output string
// Return       : integer, last error number and set msg with the last message
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.GetLastError(var msg: string): Integer;
begin
  //todo
  result := i_lasterr;
  case i_lasterr of
  0: msg := 'No error exist.';
  else msg := 'This error is not specified.';
  end;
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
// History      :
// =============================================================================
function TFlashRunner.ConfigDevice(const ini: TMemIniFile): boolean;
begin
  result := false;
  if (e_state in C_DEV_STATES[DE_CONFIG]) then
  begin
    //settings from ini file
    if (ini.SectionExists(CSTR_FR_SEC)) then
    begin
      c_timeout := ini.ReadInteger(CSTR_FR_SEC, CSTR_DEV_TIMEOUT, C_TIMEOUT_MSEC);
      result := (ConfigConnections(ini, CSTR_FR_SEC) > 0);
      if result then e_state := DS_CONFIGURED;
    end;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SetDynamicMem
//                builds a string for setting dynamic memory of flash runner and
//                sends it to flash runner, then receives the answer and checks it.
//                The dynamic memory has 512 bytes and is addressed from 0 on.
// Parameter    : addr, address of dynamic memory in flash runner
//                num, a integer which is being set in the dynamic memory
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SetDynamicMem(const addr: word; const num: integer): boolean;
var s_send, s_answer: string;
begin
  result := false;
  s_send := format('DMSET $%.4x 8 %s', [addr, IntToDMDataStr(num)]) + Char(13);
  if (SendStr(s_send) = length(s_send)) then begin
    RecvStr(s_answer);
    result := (State = DS_READY);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RunScript
//                executs a script file from the sd card of FlashRunner
// Parameter    : script, name of the script file on sd card
//                msecs, t
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RunScript(const script: string; const msecs: integer): boolean;
var s_send, s_answer: string; c_tmp: cardinal; 
begin
  result := false; c_tmp := Timeout;
  if (msecs > 0) then Timeout := msecs;
  
  s_send := format('RUN %s', [script]) + Char(13);
  if (SendStr(s_send) = length(s_send)) then begin
    RecvStr(s_answer);
    result := (State = DS_READY);
  end;
  Timeout := c_tmp;
end;

end.

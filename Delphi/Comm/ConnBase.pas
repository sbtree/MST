// =============================================================================
// Module name  : $RCSfile: ConnBase.pas,v $
// description  : This unit implements a basis methodes and properties of a class
//                for connection FlashRunner. The types of connection includes Jtag
//                RS232, USB, GPIB, CAN-bus and Profil-Bus.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit ConnBase;

interface
uses Classes, TextMessage, IniFiles, Serial3;
const
  C_BUFFER_SIZE_DEFAULT = 1024;

type
  EConnectType = (CT_UNKNOWN, //unknown connection
                  CT_RS232,   //rs232
                  CT_USB,     //usb
                  CT_GPIB,    //GPIB (IEEE 488)
                  CT_ETHERNET,//ethernet
                  CT_JTAG,    //jtag
                  CT_CAN,     //can-bus
                  CT_PROFIL   //profil-bus
                  );

  EConnectState = ( CS_UNKNOWN,   //unknown state
                    CS_CONFIGURED,//connection is configurated
                    CS_CONNECTED  //connection is connected and in use
                   );

  IConnInterf = interface
    function Config(const sconf: string): boolean; overload;
    function Config(const sconfs: TStrings): boolean; overload;
    function Connect(): boolean;
    function Disconnect: boolean;
    function SendBuf(const buf: PChar; const len: longword): boolean;
    function RecvBuf(var buf: PChar; const len: longword): integer;
    function SendStr(const str: string): boolean;
    function RecvStr(var str: string; const bwait: boolean): integer;
  end;

  TConnBase = class(TComponent, IConnInterf)
  class function GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
  class function GetConnectTypeName(const etype: EConnectType): string;
  class function GetConnectState(const estate: EConnectState): string;
  protected
    e_type:     EConnectType;
    t_connobj:  TObject;        //actural intance for communication
    e_state:    EConnectState;  //connection state
    c_timeout:  cardinal;       //timeout in milli seconds
    c_rinterval:cardinal;       //maximal interval by reading in milli seconds
    b_rinterval:boolean;        //indicates if interval is allowed by reading
    t_messenger:TTextMessenger; //for transfering messages
    ch_nullshow:Char;           //indicate a char to show null, if it is received
    i_cntnull:  integer;        //count of received nulls
    ba_rbuf:    array[0..C_BUFFER_SIZE_DEFAULT-1] of Char; //buffer for data received from device
    w_rlen:     word;     //actual length of the received data
  protected
    function GetTypeName(): string; virtual;
    function GetStateStr(): string; virtual;
    function WaitForReading(const tend: cardinal): boolean; virtual;
    function WaitForWriting(const tend: cardinal): boolean; virtual;
    function WaitForConnecting(const tend: cardinal): boolean; virtual;
    function IsConnected(): boolean; virtual;
    function IsReadable(): boolean; virtual; abstract;
    function IsWriteComplete(): boolean; virtual; abstract;
    procedure TryConnect(); virtual; abstract;
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO); virtual;
    procedure UpdateMessage(const text: string; const level: EMessageLevel = ML_INFO); virtual;
  public
    //constructor and destructor
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    //base properties
    property Connected: boolean read IsConnected;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property ConnectType: EConnectType read e_type;
    property ConnectTypeName: string read GetTypeName;
    property ConnectState: EConnectState read e_state;
    property Timeout: cardinal read c_timeout write c_timeout;
    property RxInterval: cardinal read c_rinterval write c_rinterval;
    property RxIntervalEnabled: boolean read b_rinterval write b_rinterval;
    property ShowNullChar: Char read ch_nullshow write ch_nullshow;

    //implementation of ICommInterf
    function Config(const sconf: string): boolean; overload; virtual;
    function Config(const sconfs: TStrings): boolean; overload; virtual; abstract;
    function Connect(): boolean; virtual;
    function Disconnect: boolean; virtual; abstract;
    function SendBuf(const buf: PChar; const len: longword): boolean; virtual; abstract;
    function RecvBuf(var buf: PChar; const len: longword): integer; virtual; abstract;
    function SendStr(const str: string): boolean; virtual;
    function RecvStr(var str: string; const bwait: boolean = false): integer; virtual; abstract;

    //additionnal functions
    function RecvStrTimeout(var str: string; const tend: cardinal): integer; virtual;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = 3000): integer; virtual;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; virtual;
  end;
  PConnBase = ^TConnBase;

const
  CSTR_CONN_KEYS : array[EConnectType] of string = (
                    'UNKNOWN',
                    'RS232',
                    'USB',
                    'GPIB',
                    'ETHERNET',
                    'JTAG',
                    'CAN',
                    'PROFIL'
                    );
  CSTR_CONN_STATES: array[EConnectState] of string = (
                    'unknown',
                    'configured',
                    'connected'
                    );
  CINT_TIMEOUT_DEFAULT = 1000;//default timeout in milliseconds
  CINT_INTERVAL_DEFAULT = 300;//default interval by reading in milliseconds
  CINT_RECV_INTERVAL = 50;    //milliseconds

implementation
uses Forms, SysUtils, StrUtils, Windows, Registry;

class function TConnBase.GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
var i_idx: integer;
begin
  i_idx := IndexText(conkey, CSTR_CONN_KEYS);
  if ((i_idx >= Ord(Low(EConnectType))) and (i_idx <= Ord(High(EConnectType)))) then begin
    val := EConnectType(i_idx);
    result := true;
  end else result := false;
end;

class function TConnBase.GetConnectTypeName(const etype: EConnectType): string;
begin
  result := CSTR_CONN_KEYS[etype];
end;

class function TConnBase.GetConnectState(const estate: EConnectState): string;
begin
  result := CSTR_CONN_STATES[estate];
end;

function  TConnBase.GetTypeName(): string;
begin
  result := TConnBase.GetConnectTypeName(e_type);
end;

function TConnBase.GetStateStr(): string;
begin
  result := TConnBase.GetConnectState(e_state);
end;

function TConnBase.WaitForReading(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait, b_read: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_read := IsReadable(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_read) and (c_tcur <= tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for reading: %ds';
    AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
        c_count := c_tcur;
      end;
      b_read := IsReadable();
      b_wait := ((not b_read) and (c_tcur <= tend));
    until (not b_wait);
  end;
  result := b_read;
end;

function TConnBase.WaitForWriting(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait, b_write: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_write := IsWriteComplete(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_write) and (c_tcur <= tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for completing write: %ds';
    AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
        c_count := c_tcur;
      end;
      b_write := IsWriteComplete();
      b_wait := ((not b_write) and (c_tcur <= tend));
    until (not b_wait);
  end;
  result := b_write;
end;

function TConnBase.WaitForConnecting(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_wait := ((not IsConnected()) and (c_tcur <= tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for connecting: %ds';
    AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      TryConnect();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
        c_count := c_tcur;
      end;
      b_wait := ((not IsConnected()) and (c_tcur <= tend));
    until (not b_wait);
  end;
  result := IsConnected();
end;

function TConnBase.IsConnected(): boolean;
begin
  result := (e_state = CS_CONNECTED);
end;

procedure TConnBase.AddMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.AddMessage(format('%s', [text]), ClassName(), level);
  end;
end;

procedure TConnBase.UpdateMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.UpdateMessage(format('%s', [text]), ClassName(), level);
  end;
end;

constructor TConnBase.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_UNKNOWN;
  t_connobj := nil;
  e_state := CS_UNKNOWN;
  c_timeout := CINT_TIMEOUT_DEFAULT;
  c_rinterval := CINT_INTERVAL_DEFAULT;
  b_rinterval := false;
  ch_nullshow := #13; //null is show as #13
  //SetLength(ba_rbuf, C_BUFFER_SIZE_DEFAULT);
  w_rlen := 0;
end;

destructor TConnBase.Destroy;
begin
  //todo:
  inherited Destroy();
end;

// =============================================================================
//    Description  : config device using a string, which is formatted as
//                   'PropertyName1:value1|PropertyName2:value2' or
//                   'PropertyName1=value1|PropertyName2=value2'
//    Parameter    : sconf, a formatted string. As empty parameter is not acceptable
//    Return       : true, if the sconf is valid and accepted by the connection
//    First author : 2016-06-17 /bsu/
//    History      :
// =============================================================================
function TConnBase.Config(const sconf: string): boolean;
var t_confs: TStrings; s_conf: string;
begin
  result := false;
  s_conf := ReplaceStr(sconf, ':', '=');
  t_confs := TStringList.Create();
  if (ExtractStrings(['|'], [' ', #9], PChar(s_conf), t_confs) > 0) then
    result := Config(t_confs);
  t_confs.Free();
end;

// =============================================================================
// Description  : make connection over calling TryConnect()
// Parameter    : --
// Return       : true, if t_ser is activated
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-12 /bsu/
// History      :
// =============================================================================
function TConnBase.Connect(): boolean;
begin
  result := false;
  if (e_state in [CS_CONFIGURED, CS_CONNECTED]) then begin
    TryConnect();
    result := WaitForConnecting(GetTickCount() + c_timeout);
    if result then begin
      e_state := CS_CONNECTED;
      AddMessage(format('Successful to make a connection(%s).', [GetTypeName()]));
    end else AddMessage(format('Successful to make a connection(%s)', [GetTypeName()]), ML_ERROR);
  end else AddMessage(format('The current state (%s) is not suitable for making a connection.', [GetStateStr()]), ML_WARNING);
end;

function TConnBase.SendStr(const str: string): boolean;
begin
  result := SendBuf(PChar(str), length(str));
end;

function TConnBase.RecvStrTimeout(var str: string; const tend: cardinal): integer;
var s_recv: string;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    result := result + RecvStr(s_recv);
    str := str + s_recv;
    Application.ProcessMessages();
  until (GetTickCount() >= tend);
end;

function TConnBase.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
var i_time: cardinal; s_recv: string; b_break: boolean;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    result := result + RecvStr(s_recv);
    str := str + s_recv;
    i_time := GetTickCount() + interv;
    if (i_time > tend) then i_time := tend;
    Application.ProcessMessages();
    b_break := WaitForReading(i_time);
  until (b_break or (GetTickCount() >= tend));
end;

function TConnBase.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean): integer;
var b_break: boolean; s_recv: string;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    result := result + RecvStr(s_recv);
    str := str + s_recv;
    if bcase then b_break := ContainsStr(str, exstr)
    else b_break := ContainsText(str, exstr);
  until (b_break or (GetTickCount() >= tend));
end;

end.

// =============================================================================
// Module name  : $RCSfile: ConnectBase.pas,v $
// description  : This unit implements a basis methodes and properties of a class
//                for connection FlashRunner. The types of connection includes Jtag
//                RS232, USB, CAN-bus and Profil-Bus.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit ConnBase;

interface
uses  Classes, Windows{, DataBuffer};
type

  EConnectionType = (
                    CT_RS232,   //rs232
                    CT_USB,     //usb
                    CT_GPIB,    //GPIB (IEEE 488)
                    CT_ETHERNET,//ethernet
                    CT_JTAG,    //jtag
                    CT_CAN,     //can-bus
                    CT_PROFIL   //profil-bus
                    );

  TConnBase = class(TComponent)
  protected
    e_type    : EConnectionType;
    i_lasterr : integer;
    s_lastmsg : string;

  strict private
    function GetLastErrorText(): string; virtual;
    
  protected
    function IsConnected(): boolean; virtual; abstract;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property ConnectionType : EConnectionType read e_type;
    property LastError : integer read i_lasterr;
    property LastErrorText : string read GetLastErrorText;
    property Connected : boolean read IsConnected;

    function Config(const sconf: string): boolean; virtual; abstract;
    function Connect(): boolean; virtual; abstract;
    function Disconnect: boolean; virtual; abstract;
    function SendData(const sbuf: array of char; const len: longword; const tend: cardinal): boolean; virtual; abstract;
    function RecvData(var rbuf: array of char; const tend: cardinal): longword; virtual; abstract;
    function RecvExpect(var rbuf: array of char; const expects: TStringList; const tend: cardinal): longword; virtual; abstract;
    //function SendData(const sbuf: TCharBuffer; const timeout: cardinal): integer; virtual; abstract;
    //function RecvData(var rbuf: TCharBuffer; const timeout: cardinal): Integer;virtual; abstract;
  end;
  PConnBase = ^TConnBase;

const
  CSTR_CONN_KEYS : array[EConnectionType] of string = (
                    'CONN_RS232',
                    'CONN_USB',
                    'CONN_GPIB',
                    'CONN_ETHERNET',
                    'CONN_JTAG',
                    'CONN_CAN',
                    'CONN_PROFIL'
                    );

  C_CONN_NO_ERR   =$0;
  C_CONN_ERR_CONF =$1;
  C_CONN_ERR_CONN =$2;
  C_CONN_ERR_DISC =$3;
  C_CONN_ERR_SEND =$4;
  C_CONN_ERR_RECV =$5;
  C_CONN_ERR_TOUT =$6;

implementation

function TConnBase.GetLastErrorText(): string;
begin
  case i_lasterr of
    C_CONN_NO_ERR:    result := 'no error exists';
    C_CONN_ERR_CONF:  result := 'an error exists in configuration';
    C_CONN_ERR_CONN:  result := 'failed to connect to device';
    C_CONN_ERR_DISC:  result := 'failed to disconnect from device';
    C_CONN_ERR_SEND:  result := 'failed to send data';
    C_CONN_ERR_RECV:  result := 'failed to receive data';
    C_CONN_ERR_TOUT:  result := 'time is out in last transmission';
  end;
  if s_lastmsg<>'' then result := ' [' + s_lastmsg + ']';
end;

constructor TConnBase.Create(owner: TComponent);
begin
	inherited Create(owner);
end;

destructor TConnBase.Destroy;
begin
	inherited Destroy;
end;

end.

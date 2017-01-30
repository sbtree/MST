// =============================================================================
// Module name  : $RCSfile: TekUSB.pas,v $
// description  : This unit defines a class, which implements methodes and
//                properties for communication with Oscilloscope of Teltronix
//                over USB-connection.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit TekUSB;

interface
uses Classes, ConnBase, DllLoader;

type
  TTekUsb = class(TCommBase, IDllLoader)
  protected
    t_dllldr:   TDllLoader;
  public
    //constructor Create(owner: TComponent); override;
    //destructor Destroy(); override;

    property DLLService: TDllLoader read t_dllldr implements IDllLoader;

  
  end;

implementation
type
  ViSession      = longword;
  ViStatus       = longword;
  ViRsrc         = AnsiString;
  ViAccessMode   = longword;
  ViUInt32       = longword;
  ViPSession     = pointer;
  ViObject       = longword;
  ViPstring      = PAnsiChar;
  ViBuf          = pointer;
  ViPFindList    = longword;
  ViPUInt32      = longword;
  ViPRsrc        = pointer;
  ViPBuf         = PAnsiChar;
  ViString       = AnsiString;

  ViOpenDefaultRM = function(var vi_session : ViSession): viStatus; stdcall;
  ViOpen = function(my_session: ViSession; s_descr: ViRsrc; lw_mode: ViAccessMode; lw_timeout: ViUInt32; p_handle: ViPSession): ViStatus;  stdcall;
  ViStatusDesc = function( my_Object : ViObject; my_status : viStatus; pc_desc: ViPstring ): ViStatus;  stdcall;
  ViWrite = function(lw_handle: ViSession; p: ViBuf; lw_length: ViUInt32; var lw_retcount:  ViUInt32): ViStatus;  stdcall;
  ViRead = function(lw_handle: ViSession; p: ViPBuf; lw_length: ViUInt32; var lw_retcount: ViPUInt32): ViStatus;  stdcall;
  ViClose = function(lw_session: ViSession ): ViStatus;  stdcall;
  VFindRsrc = function( lw_session: ViSession; s_template: ViString; var lw_list: ViPFindList; var lw_items: ViPUInt32; p_desc: ViPRsrc): ViStatus; stdcall;

end.

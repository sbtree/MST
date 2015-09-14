unit Serial3;
{
   Serial interface wrapper component
   RJC Jul '96

   Change History :

        2.00         Jul '96     Delphi 2 version
        2.01         11/08/96    Corrected problem in DCB Flags
                                 Included 115200 baudrate
        2.02         23/08/96    Re-open problem fixed
        2.10         30/08/96    Added control-line events & methods
                                 Set DTR by default on initialize
                                 Eliminated compile-warnings

        2.20         16/02/97    NT fixes. Uses overlapped I/O.
                                 Also defaults to RTS enabled.

        2.21         10/02/98    Explicitly use AnsiString in ReadString/WriteString
                                 and TRxMessageEvent
                                 Efficiently handle long strings
                                 Use computed timeout on WriteString
	2.22	     28/03/98	 Don't call event scripts when aborting
	2.23         10/04/98	 Added extra baudrates
	3.23	     10/04/98	 Delphi 3 version
        6.23         24/09/02    Delphi 6 Version
                                 In EnableEvents: Resume nur wenn Suspended
        6.24         01/12/04    Erweiterung: COM-Ports bis 255 unterstützt                         

}

{$undef SHAREWARE}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs,Windows;

type

  {-----------------------------------------------------------------------------}
  { TSerial type declarations }

  eParity = ( paNone, PaOdd, paEven, paMark, paSpace ) ;
  eDataBits = (d7bit, d8bit ) ;
  eStopBits = (st1bit, st2bit ) ;
  //sPorts = 1..9 ;
  sPorts = 1..256 ; // changed by Georgi Slavov
  eFlowControl = (fcNone, fcRTS_CTS, fcDTR_DSR, fcXON_XOF) ;
  eOpModes = (rxNormal, rxMessage) ;
  eMsgState = (msNone, msStarted, msEnded, msDone) ;
  eNotifyErr = (neDialog, neEvent, neNone) ;
  eModemInSignal = (msCTS, msDSR, msRLSD) ;
  eModemOutSignal = (msRTS, msDTR) ;

  TRxMessageEvent = procedure (Sender : TObject ; RxMessage : AnsiString) of object;
  TSignalChangeEvent = procedure (Sender : TObject ; Signal : eModemInSignal ; SignalState : Boolean) of object;
  TWriteCompletedEvent = procedure of object;

  {-----------------------------------------------------------------------------}
  { TSerial main class }

  TSerial = class(TComponent)
  private
    { Property field declarations }
    FPort : sPorts ;             { Port property }
    FBaudrate : Integer ;        { Baudrate property }
    FParity : eParity ;          { Parity property }
    FDataBits : eDataBits ;      { Data bits property }
    FStopBits : eStopBits ;      { Stop bits property }
    FActive : Boolean ;          { Active property }
    FRxQSize : Word ;            { Input queue size property }
    FTxQSize : Word ;            { Output queue size property }
    FFlowMode : eFlowControl ;   { Flow control mode }
    FCheckParity : Boolean ;     { Check parity flag }
    FRxEventMode : eOpModes ;    { Message trap mode }
    FMessageStartChar : Char ;   { Message start character }
    FMessageEndChar : Char ;     { Message end character }
    FMessageAppendCount : Word ; { Number of bytes to add after end character }
    FNotifyErrors : eNotifyErr ; { Error notification mode }
    FErrorCode : Integer ;       { Error code field }

    { Variables }
    FCID : THANDLE ;             { Comms id returned by CreateFile }
    FDCB : TDCB ;                { DCB for Windows API }
    FCTO : TCOMMTIMEOUTS ;       { Comms timeouts structure }
    FTS  : TCOMSTAT;             { Comms state structure }
    FMessageState : eMsgState;   { Message state flags }
    FMessageAppendLeft : Word;   { Message overrun count }
    FMessage : AnsiString ;      { Message captured }
    FThreadActive : Boolean ;    { Thread active flag }
    CTSSignalState : Boolean ;   { Stored signal state }
    DSRSignalState : Boolean ;   { Stored signal state }
    RLSDSignalState : Boolean ;  { Stored signal state }
    WrittenBytes : DWORD;        { Bytes written }
    OverlapBlock : TOverlapped;  { Overlapped I/O structure }
    AbortInProgress : Boolean;	 { Abort flag }
    FDestroying     : boolean;   { Destroying flag }

    { Event-method pointers }
    FOnRxData : TNotifyEvent;
    FOnTxEmpty : TNotifyEvent;
    FOnMessage : TRxMessageEvent;
    FOnError : TNotifyEvent;
    FOnBreak : TNotifyEvent;
    FOnRing : TNotifyEvent;
    FOnSignalChange : TSignalChangeEvent;
    FOnWriteCompleted : TWriteCompletedEvent;

    { Thread object pointer }
    FEventThread : TThread ;        { Event thread pointer }

    { Private procedures }
    procedure SetPort (nPort : sPorts);
    procedure SetBaud (Baud : Integer);
    procedure SetParity (Parity : eParity);
    procedure SetData (Data : eDatabits);
    procedure SetStop (Stop : eStopbits);
    procedure SetActive (bActive : Boolean);
    procedure SetRxQSize (qSize : Word);
    procedure SetTxQSize (qSize : Word);
    procedure SetFlowMode (mode : eFlowControl);
    procedure SetParityCheck (check : Boolean);

    function  GetRxWaiting : Integer;
    function  GetTxWaiting : Integer;

    procedure ReadErrorState;
    procedure SetupDCB;
    procedure ReOpenPort(toOpen : Boolean);
    procedure OpenErrorDlg (err : Integer);
    procedure EnableEvents;
    procedure ProcessError;

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create (aOwner : TComponent); override;
    destructor Destroy; override;

    procedure WriteChar (c : Char);
    procedure WriteString (s : AnsiString);
    function ReadChar (var c : Char) : Integer;
    function ReadString (var s : AnsiString) : Integer;
    procedure ZapTxQueue;
    procedure ZapRxQueue;
    procedure Break (time : Integer);
    procedure SetSignal (signal : eModemOutSignal; state : Boolean) ;

    { Public (run-time only) properties }
    property Active : Boolean read FActive write SetActive default False ;

  published
    { Published declarations }

    { Published data properties }
    property Port : sPorts read FPort write SetPort default 1;
    property Baudrate : Integer read FBaudrate write SetBaud default 9600;
    property Parity : eParity read FParity write SetParity default paNone ;
    property DataBits : eDataBits read FDataBits write SetData default d7bit ;
    property StopBits : eStopBits read FStopBits write SetStop default st1bit ;
    property RxQueueSize : Word read FRxQSize write SetRxQSize default 1024 ;
    property TxQueueSize : Word read FTxQSize write SetTxQSize default 1024 ;
    property FlowMode : eFlowControl read FFlowMode write SetFlowMode default fcNone ;
    property CheckParity : Boolean read FCheckParity write SetParityCheck default True ;
    property RxWaiting : Integer read GetRxWaiting ;
    property TxWaiting : Integer read GetTxWaiting ;
    property RxEventMode : eOpModes read FRxEventMode write FRxEventMode default rxNormal ;
    property MessageStartChar : Char read FMessageStartChar write FMessageStartChar default Chr(2) ;
    property MessageEndChar : Char read FMessageEndChar write FMessageEndChar default Chr(3) ;
    property MessageAppendCount : Word read FMessageAppendCount write FMessageAppendCount default 0;
    property NotifyErrors : eNotifyErr read FNotifyErrors write FNotifyErrors default neDialog;
    property ErrorCode : Integer read FErrorCode ;

    { Event-method properties }
    property OnRxData: TNotifyEvent read FOnRxData write FOnRxData;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
    property OnMessage: TRxMessageEvent read FOnMessage write FOnMessage;
    property OnError : TNotifyEvent read FOnError write FOnError;
    property OnBreak : TNotifyEvent read FOnBreak write FOnBreak;
    property OnRing : TNotifyEvent read FOnRing write FOnRing;
    property OnSignalChange : TSignalChangeEvent read FOnSignalChange write FOnSignalChange;
    property OnWriteCompleted :TWriteCompletedEvent read FOnWriteCompleted write FOnWriteCompleted;
  end;

  {-----------------------------------------------------------------------------}
  { Event handling thread subclass }
  TEventThread = class(TThread)
  private
    { Private declarations }
    FParentInstance : TSerial ;
    procedure DoTxEvent;
    procedure DoRxEvent;
    procedure DoMsgEvent;
    procedure DoCTSEvent;
    procedure DoDSREvent;
    procedure DoRLSDEvent;
    procedure DoBreakEvent;
    procedure DoRingEvent;

  protected
    procedure Execute; override;
  public
    constructor Create(parentTSerial : TSerial);
  end;

procedure Register;

const
    aBaudrates : Array [1..15] of Integer =
                  (110, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 38400, 56000, 57600, 115200, 128000, 256000);

    aDatabits : Array[edataBits] of Byte =
             (7, 8) ;

    aParity : Array[eParity] of Byte =
             (NOPARITY, ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY) ;

    aStopbits : Array[eStopBits] of Byte =
             (ONESTOPBIT, TWOSTOPBITS);

implementation

{-----------------------------------------------------}
{ Registration procedures }
procedure Register;
begin
  RegisterComponents('System', [TSerial]);
end;

{-----------------------------------------------------}
{ Constructor/destructor procedure }
constructor TSerial.Create (aOwner : TComponent);
begin
   inherited Create (aOwner);

   FDestroying := false;

   { Initialize default property values }
   FPort := 1;
   FBaudrate := 9600;
   FParity := paNone ;
   FDataBits := d7bit ;
   FStopBits := st1bit ;
   FActive := False ;
   FRxQSize := 1024 ;
   FTxQSize := 1024 ;
   FFlowMode := fcNone ;
   FCheckParity := True ;
   FRxEventMode := rxNormal ;
   FMessageStartChar := Chr(2) ;
   FMessageEndChar := Chr(3) ;
   FMessageAppendCount := 0 ;
   FMessageState := msNone ;
   FMessageAppendLeft := 0 ;
   FNotifyErrors := neDialog ;

   CTSSignalState := False ;
   DSRSignalState := False ;
   RLSDSignalState := False ;

   OverlapBlock.Offset := 0;
   OverlapBlock.OffsetHigh := 0;
   OverlapBlock.Internal := 0;
   OverlapBlock.InternalHigh := 0;
   OverlapBlock.hEvent :=  CreateEvent(nil, FALSE, TRUE, pChar('Event1')) ;

   { Make the event-thread object }
   FEventThread := TEventThread.Create(self) ;
   FThreadActive := False ;

   AbortInProgress := False ;

{$ifdef SHAREWARE}
   { Output shareware message }
   MessageDlg ('SERIAL component for Delphi 3-6 (c) R J Crowther 1995-1998'+Chr(13)+
               'Shareware version 6.23'+Chr(13)+
               'See help file for registration information', mtInformation, [mbOK], 0)
{$ENDIF}
end;

destructor TSerial.Destroy;
begin
   FDestroying := true;
   
   { Close the port if it is open at the moment }
   ReopenPort (False);

   FEventThread.Terminate ; { Destroys thread automatically }
   // 2013-02-27 /gsv/: Thread wieder anstarten, damit es terminiert wird.
   if ( FEventThread.Suspended ) then FEventThread.Resume();
   
   inherited Destroy;
end;

{-----------------------------------------------------}
{ Property access procedures }
procedure TSerial.SetPort (nPort : sPorts);
begin
   if FPort <> nPort then
   begin
      FPort := nPort ;
      ReOpenPort(FActive);
   end
end;

procedure TSerial.SetBaud (Baud : Integer);
var
   i : Integer ;
   baud_ok : Boolean;
begin
   baud_ok := False;
   for i:=Low(aBaudrates) to High(aBaudrates) do
   begin
        if Baud=aBaudrates[i] then baud_ok := True ;
   end;

   if baud_ok then
   begin
      if FBaudrate<>Baud then
      begin
         FBaudrate := Baud ;
         ReopenPort(FActive)
      end
   end
   else
      MessageDlg ('Baudrate is not valid', mtError, [mbOK], 0)
end;

procedure TSerial.SetParity (Parity : eParity);
begin
   if FParity <> Parity then
   begin
      FParity := Parity ;
      ReopenPort(FActive)
   end
end;

procedure TSerial.SetData (Data : eDatabits);
begin
   if FDataBits <> Data then
   begin
        FDataBits := Data ;
        ReopenPort(FActive)
   end
end;

procedure TSerial.SetStop (Stop : eStopbits);
begin
   if FStopBits <> Stop then
   begin
      FStopBits := Stop ;
      ReopenPort(FActive)
   end
end;

procedure TSerial.SetActive (bActive : Boolean);
begin
   ReOpenPort(bActive);
end;

procedure TSerial.SetRxQSize (qSize : Word);
begin
   FRxQSize := qSize ;
   ReOpenPort(FActive);
end;

procedure TSerial.SetTxQSize (qSize : Word);
begin
   FTxQSize := qSize ;
   ReOpenPort(FActive);
end;

procedure TSerial.SetFlowMode (mode : eFlowControl);
begin
   if FFlowMode <> mode then
   begin
      FFlowMode := mode ;
      ReopenPort(FActive)
   end
end;

procedure TSerial.SetParityCheck (check : Boolean);
begin
   if FCheckParity<>check then
   begin
      FCheckParity := check ;
      ReopenPort(FActive)
   end
end;

function TSerial.GetRxWaiting : Integer;
begin
   if active then
   begin
      ReadErrorState;
      GetRxWaiting := FTS.cbInQue
   end
   else
      GetRxWaiting := 0
end;

function TSerial.GetTxWaiting : Integer;
begin
   if active then
   begin
      ReadErrorState;
      GetTxWaiting := FTS.cbOutQue
   end
   else
      GetTxWaiting := 0
end;


{-----------------------------------------------------}
{ Internal procedures }
procedure TSerial.ReadErrorState;
var
   errorcode : DWORD;
begin
   if active then
      if not ClearCommError (FCID, errorcode, @FTS)
      then ProcessError
end;

procedure TSerial.SetupDCB;
begin

   FDCB.DCBLength := SizeOf (TDCB) ;
   FDCB.BaudRate := FBaudRate ;
   FDCB.Flags := $0001;          { Binary flag }

   if FCheckParity then FDCB.Flags := FDCB.Flags or $0002;

   case FFlowMode of
       fcRTS_CTS: FDCB.Flags := FDCB.Flags or (RTS_CONTROL_HANDSHAKE shl 12) or $0004;
       fcDTR_DSR: FDCB.Flags := FDCB.Flags or (DTR_CONTROL_HANDSHAKE shl 4) or $0008;
       fcXON_XOF: FDCB.Flags := FDCB.Flags or $0300;
   end;

   FDCB.XonLim := FRxQSize div 3;                { Send XON when 1/3 full }
   FDCB.XoffLim := FRxQSize div 3;               { Send XOF when 2/3 full }
   FDCB.ByteSize := aDatabits[FDataBits] ;
   FDCB.Parity := aParity[FParity] ;
   FDCB.StopBits := aStopbits[FStopBits] ;

   FDCB.XonChar := Chr(17);
   FDCB.XoffChar := Chr(19);
   FDCB.ErrorChar := '?';

   { Set the DCB parameters into the port }
   if FActive then
   begin
        if not SetCommState(FCID, FDCB)
        then ProcessError;

        { Set comms timeouts }
        FCTO.ReadIntervalTimeout := MAXDWORD ; { Non-blocking read }
        FCTO.ReadTotalTimeoutMultiplier := 0 ;
        FCTO.ReadTotalTimeoutConstant := 0 ;

        { Block writes only if flow control is active }
        if FFlowMode <> fcNone then
        begin
            FCTO.WriteTotalTimeoutMultiplier := 0 ;
            FCTO.WriteTotalTimeoutConstant := 10000 ;
        end;
        if not SetCommTimeouts (FCID, FCTO)
        then ProcessError
   end
end;

procedure TSerial.ReOpenPort(toOpen : Boolean);
var
   pname : Array[0..11] of char;
   ret :    THANDLE;
   count : Integer ;
   
begin
   if FActive then
   begin
        { Port active so close it }
        { Wipe TX and RX queue }
        if not PurgeComm (FCID, PURGE_TXABORT or PURGE_RXABORT)
        then ProcessError;

        { Set DTR & RTS inactive }
        SetSignal (msDTR, False) ;
        SetSignal (msRTS, False) ;

        { Set component inactive }
        FActive := False;
	AbortInProgress := True ;

        { Disable events }
        if not SetCommMask (FCID, 0) then
        ProcessError;

        { Wait for event task thread to terminate }
        //count := 100 ;
        //count := 10 ;            // changed by axel Hahlweg
        // 2013-08-15 /gsv/: Keine Ahnung, warum count hier auf 10 reduziert war.
        // Dies führte u.U. dazu, dass bei disconnect() der COM-Port nicht richtig
        // geschlossen wurde, weil die Wartezeit hier nicht ausgereicht hat.
        // Daher count wider erhöht.
        count := 50 ;
        // 2013-08-15 /gsv/: Beim Freigeben des Objekts muss hier die MessageQueue
        // abgearbeitet werden. Der Thread ruft Funktionen mittels Synchronize auf.
        // Dadurch wird der Funktionsaufruf im Hauptthread ausgeführt und der
        // WorkerThread angehalten. Hier wird das Ende des Threads abgewartet.
        // Die Variable FThreadActive wird nicht zurückgesetzt, weil der Thread
        // noch in der synchronized Funktion hängt, die noch in der MessageQueue
        // des Hauptthreads ist. Daher hier diese prozessieren, damit die synchronized
        // Funktion verlassen und der Thread normal weiter ausgeführt wird.
        if ( FDestroying ) then Application.ProcessMessages();

        while FThreadActive and (count > 0) do
        begin
           Sleep(75) ;
           count := count-1 ;
        end;

	AbortInProgress := False ;

        { Close handle to port }
        if not CloseHandle(FCID)
        then ProcessError;

   end;

   if (toOpen) then
   begin
      { Open the port using the specified settings }
      { Für COM-Ports > 9 '\\.\' muss am Anfang eingefügt werden.
        (Windows Misteries, not specified in the WinAPI!) }
      if ( FPort < 10 ) then StrPcopy(pname, Format('COM%d', [FPort]))
      else StrPcopy(pname, Format('\\.\COM%d', [FPort]));
      ret := CreateFile (pname, (GENERIC_READ or GENERIC_WRITE), 0, nil,
                          OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0) ;

      if ret <> INVALID_HANDLE_VALUE then
      begin
          { Store the file handle }
          FCID := ret;

          { Set comms buffer sizes }
          if not SetupComm (FCID, FRxQSize, FTxQSize)
          then ProcessError;

          { Set xon/xof interrupt points and set up the DCB options }
          FActive := True;
          SetupDCB;

          { Set DTR & RTS active by default }
          SetSignal(msDTR, TRUE);
          setSignal(msRTS, TRUE);

          { Enable event processing }
          EnableEvents;
      end
      else ProcessError;
   end
end;

procedure TSerial.EnableEvents;
begin
   if not SetCommMask (FCID, EV_RXCHAR or EV_TXEMPTY or EV_CTS or EV_DSR or EV_BREAK or EV_RLSD or EV_RING)
   then ProcessError;
   if (FEventThread.Suspended) then FEventThread.Resume ;            { Resume event thread }

end;


procedure TSerial.OpenErrorDlg (err : Integer);
begin
   { Locate message }
   MessageDlg (SysErrorMessage(err), mtError, [mbOK], 0)
end;

{ General error handler }
procedure TSerial.ProcessError;
begin
   FErrorCode := GetLastError() ;
   case FNotifyErrors of
      neDialog : OpenErrorDlg(FErrorCode);
      neEvent : if Assigned(FOnError) then FOnError(self) ;
   end

end;


{-----------------------------------------------------}
{ Method procedures }
procedure TSerial.WriteChar (c : char);
var
  p : array[0..1] of char ;
  Res : Integer;
begin
  StrPCopy(p,c) ;
  if not WriteFile (FCID, p, 1, WrittenBytes, @OverlapBlock) then
  begin
    Res := WaitForSingleObject(OverlapBlock.hEvent,1000);
    if  Res <> 0 then ProcessError
    else
    begin
      ResetEvent(OverlapBlock.hEvent);
      OverlapBlock.Offset := 0;
      OverlapBlock.OffsetHigh := 0;
    end;
  end;
end;


procedure TSerial.WriteString (s : AnsiString);
var
  Res, timeout : Integer;
begin
  { Timeout based on string length & baudrate * 2 }
  timeout := (20000 div baudrate) * Length(s)  ;
  if timeout < 500 then timeout := 500 ;

  if not WriteFile (FCID, s[1], Length(s), WrittenBytes, @OverlapBlock) then
  begin
    Res := WaitForSingleObject(OverlapBlock.hEvent,timeout);
    if  Res <> 0 then    ProcessError
    else
    begin
      ResetEvent(OverlapBlock.hEvent);
      OverlapBlock.Offset := 0;
      OverlapBlock.OffsetHigh := 0;
    end;
  end;
end;

function TSerial.ReadChar (var c: Char) : Integer;
var
  buf : Array[0..1] of Char ;
  Res : integer;
begin
  ReadChar := 0;
  if not ReadFile (FCID, buf, 1, WrittenBytes, @OverlapBlock) then
  begin
    Res := WaitForSingleObject(OverlapBlock.hEvent,100);
    case Res of
      WAIT_OBJECT_0 :
      begin
        ResetEvent(OverlapBlock.hEvent);
        OverlapBlock.Offset := 0;
        OverlapBlock.OffsetHigh := 0;

        // //2014-10 /bsu/ Bugfix: neue Zeile, WrittenBytes muss aktualisiert werden. Sie war Null manchmal
        GetOverlappedResult ( FCID, OverlapBlock, WrittenBytes, false );

        if (WrittenBytes=1) then c := buf[0] ;
        ReadChar := WrittenBytes;
      end;
      WAIT_TIMEOUT :
      begin
        ResetEvent(OverlapBlock.hEvent);
        OverlapBlock.Offset := 0;
        OverlapBlock.OffsetHigh := 0;
        WrittenBytes := 0;
        ReadChar := 0;
      end;
      else
      begin
        ProcessError;
        exit;
      end;
    end;
  end
  else
  begin
    ResetEvent(OverlapBlock.hEvent);
    OverlapBlock.Offset := 0;
    OverlapBlock.OffsetHigh := 0;
    if (WrittenBytes=1) then c := buf[0] ;
    ReadChar := WrittenBytes;
  end;
end;

function TSerial.ReadString (var s : AnsiString) : Integer;
var
  TestChar : char;
  i : integer;
  iCount : integer;
begin
  S := '';
  for i := 0 to FRXQSize do
  begin
    if self.ReadChar(TestChar) > 0 then S := S + TestChar
    else System.Break;
  end;
  iCount := Length(s);
  Result := iCount;
end;

procedure TSerial.ZapTxQueue;
begin
   if not PurgeComm (FCID, PURGE_TXABORT)
   then ProcessError
end;

procedure TSerial.ZapRxQueue;
begin
   if not PurgeComm (FCID, PURGE_RXABORT)
   then ProcessError
end;

procedure TSerial.Break (time : Integer);
begin
   if SetCommBreak(FCID) then
   begin
       Sleep(time);
       ClearCommBreak(FCID)
   end
end;

procedure TSerial.SetSignal (signal : eModemOutSignal; state : Boolean);
var
    func : DWORD;
begin
    func := 0;
    if signal=msRTS then
        if state then func := SETRTS
                 else func := CLRRTS
    else if signal=msDTR then
        if state then func := SETDTR
                 else func := CLRDTR;

    EscapeCommFunction (FCID, func) ;
end;


{-------------------------------------------------------------------------------}
{ Thread procedures for handling comm events }

{
  Thread main procedures
  Wait for the appropriate event and call the user
  event handler if appropriate.
  "Synchronize" allows the user event to access VCL
}
procedure TEventThread.Execute;
var
   event : DWORD ;
   cstate : DWORD ;
   c      : Char ;
begin
   with FParentInstance do

      while not Terminated do
      begin
         if Active then
         begin
            { Set active flag }
            FThreadActive := True ;

            { Wait for the comms event }
            if not WaitCommEvent (FCID, event, nil)
            then
            begin
                 if Active then Synchronize(ProcessError);
            end;

	    if not AbortInProgress then
	    begin

               if (event and EV_CTS) <> 0 then
               begin
                  GetCommModemStatus(FCID, cstate) ;
                  CTSSignalState := (cstate and MS_CTS_ON) <> 0 ;
                  Synchronize(DoCTSEvent) ;
               end;

               if (event and EV_DSR) <> 0 then
               begin
                  GetCommModemStatus(FCID, cstate) ;
                  DSRSignalState := (cstate and MS_DSR_ON) <> 0 ;
                  Synchronize(DoDSREvent) ;
               end;

               if (event and EV_RLSD) <> 0 then
               begin
                  GetCommModemStatus(FCID, cstate) ;
                  RLSDSignalState := (cstate and MS_RLSD_ON) <> 0 ;
                  Synchronize(DoRLSDEvent) ;
               end;

               if (event and EV_TXEMPTY) <> 0 then
                  Synchronize(DoTxEvent) ;

               if (event and EV_BREAK) <> 0 then
                  Synchronize(DoBreakEvent) ;

               if (event and EV_RING) <> 0 then
                  Synchronize(DoRingEvent) ;

               if (event and EV_RXCHAR) <> 0 then

                  case FRxEventMode of

                     rxNormal :
                        { Execute OnRxData event }
                        Synchronize(DoRxEvent);

                     rxMessage :
                        { Check for OnMessage event }
                        if Assigned(FOnMessage) then
                        begin
                           while ReadChar(c) > 0 do
                           begin
                              if FMessageState=msEnded then
                              begin
                                 FMessage := FMessage + c ;
                                 FMessageAppendLeft := FMessageAppendLeft - 1 ;
                                 if FMessageAppendLeft <= 0 then
                                 begin
                                    { Append count completed - call event }
                                    FMessageState := msDone ;
                                    Synchronize(DoMsgEvent);
                                 end
                              end
                              else
                              begin
                                 if c=FMessageStartChar then
                                 begin
                                    { Always set start of message }
                                    FMessageState := msStarted ;
                                    FMessage := c
                                 end
                                 else if c=FMessageEndChar then
                                 begin
                                    { Set ended if running }
                                    if FMessageState=msStarted then
                                    begin
                                       FMessage := FMessage + c ;
                                       FMessageState := msEnded ;

                                       if FMessageAppendCount=0 then
                                       begin
                                          FMessageState := msDone;
                                          Synchronize(DoMsgEvent);
                                       end
                                       else
                                       begin
                                          FMessageAppendLeft := FMessageAppendCount
                                       end
                                    end
                                 end
                                 else
                                 begin
                                    if FMessageState=msStarted then
                                    begin
                                       FMessage := FMessage + c
                                    end
                                 end
                              end
                           end
                        end
                     end
                  end
            end
         else  { Not active }
         begin
            { unset active flag }
            FThreadActive := False ;
            Sleep(100) ;
         end
      end
end;

{ Constructor for thread object }
constructor TEventThread.Create(parentTSerial : TSerial);
begin
  inherited Create(True);                  { Create the thread suspended }
  FreeOnTerminate := True ;
  FParentInstance := parentTSerial ;
end;

procedure TEventThread.DoRxEvent;
begin
    { Execute OnRxData event }
    with FParentInstance do
    if Assigned(FOnRxData) then FOnRxData(FParentInstance);
end;

procedure TEventThread.DoMsgEvent;
begin
    { Execute OnRxMessage event }
    with FParentInstance do
    if Assigned(FOnMessage) then FOnMessage(FParentInstance, Fmessage);
end;

procedure TEventThread.DoTxEvent;
begin
    { Execute OnTxEmpty event }
    with FParentInstance do
    if Assigned(FOnTxEmpty) then FOnTxEmpty(FParentInstance);
end;

procedure TEventThread.DoCTSEvent;
begin
    { Execute OnSignalChange event for CTS }
    with FParentInstance do
    if Assigned(FOnSignalChange) then FOnSignalChange(FParentInstance, msCTS, CTSSignalState);
end;

procedure TEventThread.DoDSREvent;
begin
    { Execute OnSignalChange event for DSR }
    with FParentInstance do
    if Assigned(FOnSignalChange) then FOnSignalChange(FParentInstance, msDSR, DSRSignalState);
end;

procedure TEventThread.DoRLSDEvent;
begin
    { Execute OnSignalChange event for RLSD }
    with FParentInstance do
    if Assigned(FOnSignalChange) then FOnSignalChange(FParentInstance, msRLSD, RLSDSignalState);
end;

procedure TEventThread.DoBreakEvent;
begin
    { Execute OnBreak event }
    with FParentInstance do
    if Assigned(FOnBreak) then FOnBreak(FParentInstance);
end;

procedure TEventThread.DoRingEvent;
begin
    { Execute OnRing event }
    with FParentInstance do
    if Assigned(FOnRing) then FOnRing(FParentInstance);
end;


end.


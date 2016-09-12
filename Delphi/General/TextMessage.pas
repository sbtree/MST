// =============================================================================
// Module name  : $RCSfile: TextMessage.pas,v $
// description  : This unit implements a methodes and properties of a class  for
//                text message.
// Compiler     : Delphi 2007
// Author       : 2015-11-20 /bsu/
// History      :
//==============================================================================
unit TextMessage;
{$M+}
interface
uses Classes;
type
  //define message level
  EMessageLevel = (
                    ML_INFO,    //general information
                    ML_WARNING, //warning message
                    ML_ERROR,   //error message
                    ML_FATAL    //fatal message
                  );
  TTextMessenger = class(TInterfacedObject)
  protected
    t_msgintern:  TStrings; //saves messages. it is created in constructor
    t_msgextern:  TStrings; //default pointing to t_msgintern, but it will be replaced by SetMessages
    e_threshold:  EMessageLevel; //indicates threshold of message level. The message which has greater level than this value, can be appended to t_msg
    b_tstamp:     boolean; //indicats, whether the time stamp should be prepended, default true
  protected
    procedure SetMessages(const msgs: TStrings);
    function GetLines(): integer;
    function GetLineText(idx: integer): string;
    function FormatMsg(const text: string; const sender: string; const level: EMessageLevel; var msg: string): boolean;
  public
    constructor Create();
    destructor  Destroy(); override;

    property  Messages: TStrings read t_msgextern write SetMessages;
    property  MessageThreshold: EMessageLevel read e_threshold write e_threshold;
    property  TimeStamp: boolean read b_tstamp write b_tstamp;
    property  CountLines: integer read GetLines;
    property  LineText[idx: integer]: string read GetLineText;

    procedure AddMessage(const text: string; const sender: string = ''; const level: EMessageLevel = ML_INFO);
    procedure AddEmptyLine();
    procedure UpdateMessage(const text: string; const sender: string = ''; const level: EMessageLevel = ML_INFO);
    procedure ExportMessages(const fname: string);
    procedure Clear(const bextern: boolean = false);
  end;

  ITextMessengerImpl = interface
    function GetMessenger(): TTextMessenger;
    procedure SetMessenger(tmessenger: TTextMessenger);
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure AddEmptyLine();
    procedure UpdateMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure SetMessageThreshold(const msglevel: EMessageLevel);
    property Messenger: TTextMessenger read GetMessenger write SetMessenger;
  end;

  TTextMessengerImpl = class(TInterfacedObject, ITextMessengerImpl)
  protected
    t_messenger:  TTextMessenger;
    s_ownername:  string;
  public
    function GetMessenger(): TTextMessenger;
    procedure SetMessenger(tmessenger: TTextMessenger);
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure AddEmptyLine();
    procedure UpdateMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure SetMessageThreshold(const msglevel: EMessageLevel);
    property Messenger: TTextMessenger read GetMessenger write SetMessenger;
    property OwnerName: string read s_ownername write s_ownername;
  end;

implementation
uses SysUtils;

//define constant strings for EMessageLevel
const CSTR_MLKEYS: array[EMessageLevel] of string = (
                   'info',
                   'warning',
                   'error',
                   'fatal'
                   );

constructor TTextMessenger.Create();
begin
  inherited Create();
  t_msgintern := TStringList.Create;
  t_msgextern := t_msgintern;
  e_threshold := ML_INFO;
  b_tstamp := true;
end;

destructor TTextMessenger.Destroy();
begin
  t_msgintern.Clear;
  t_msgintern.Free;
  inherited Destroy;
end;

procedure TTextMessenger.AddMessage(const text: string; const sender: string; const level: EMessageLevel);
var s_msg: string;
begin
  if (FormatMsg(text, sender, level, s_msg)) then begin
    t_msgintern.Add(s_msg);
    if (t_msgextern <> t_msgintern) then t_msgextern.Add(s_msg);
  end;
end;

procedure TTextMessenger.AddEmptyLine();
begin
    t_msgintern.Add('');
    if (t_msgextern <> t_msgintern) then t_msgextern.Add('');
end;

procedure TTextMessenger.UpdateMessage(const text: string; const sender: string; const level: EMessageLevel);
var s_msg: string;
begin
  if (FormatMsg(text, sender, level, s_msg)) then begin
    if (t_msgintern.Count > 0) then begin
      t_msgintern[t_msgintern.Count - 1] := s_msg;
      if ((t_msgextern <> t_msgintern) and (t_msgextern.Count > 0)) then t_msgextern[t_msgextern.Count - 1] := s_msg;
    end else begin
      t_msgintern.Add(s_msg);
      if (t_msgextern <> t_msgintern) then t_msgextern.Add(s_msg);
    end;
  end;
end;

procedure TTextMessenger.ExportMessages(const fname: string);
begin
  t_msgintern.SaveToFile(fname);
end;

procedure TTextMessenger.Clear(const bextern: boolean);
begin
  t_msgintern.Clear();
  if (bextern) then t_msgextern.Clear();
end;

procedure TTextMessenger.SetMessages(const msgs: TStrings);
begin
  if assigned(msgs) then t_msgextern := msgs
  else t_msgextern := t_msgintern;
end;

function TTextMessenger.GetLines(): integer;
begin
  result := t_msgintern.Count;
end;

function TTextMessenger.GetLineText(idx: integer): string;
begin
  result := '';
  if ((idx >= 0) and (idx < t_msgintern.Count)) then result := t_msgintern[idx];
end;

function TTextMessenger.FormatMsg(const text: string; const sender: string; const level: EMessageLevel; var msg: string): boolean;
begin
  if (level >= e_threshold) then begin
    result := true;
    if (sender <> '' ) then msg := '[' + sender + ']' + text
    else msg := text;
    msg := '[' + CSTR_MLKEYS[level] + ']' + msg;
    if b_tstamp then msg := '[' + DateTimeToStr(Now()) + ']: ' + msg;
  end else result := false;
end;

function TTextMessengerImpl.GetMessenger(): TTextMessenger;
begin
  result := t_messenger;
end;

procedure TTextMessengerImpl.SetMessenger(tmessenger: TTextMessenger);
begin
  t_messenger := tmessenger;
end;

// =============================================================================
// Description  : append a new message in t_messenger
// Parameter    : text, text of the message to update
//                level, message level, see the definition of EMessageLevel
// Exceptions   : --
// First author : 2016-08-26 /bsu/
// History      :
// =============================================================================
procedure TTextMessengerImpl.AddMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.AddMessage(format('%s', [text]), s_ownername, level);
  end;
end;

procedure TTextMessengerImpl.AddEmptyLine();
begin
  if assigned(t_messenger) then t_messenger.AddEmptyLine();
end;

// =============================================================================
// Description  : update the text of last message, which is appended through
//                function AddMessage
// Parameter    : text, text of the message to update
// Exceptions   : --
// First author : 2016-06-15 /bsu/
// History      :
// =============================================================================
procedure TTextMessengerImpl.UpdateMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.UpdateMessage(format('%s', [text]), s_ownername, level);
  end;
end;

procedure TTextMessengerImpl.SetMessageThreshold(const msglevel: EMessageLevel);
begin
  if assigned(t_messenger) then t_messenger.MessageThreshold := msglevel;
end;

end.

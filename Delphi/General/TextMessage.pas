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

  TTextMessenger = class
  protected
    t_msgintern:  TStrings; //saves messages. it is created in constructor
    t_msgextern:  TStrings; //default pointing to t_msgintern, but it will be replaced by SetMessages
    e_threshold:  EMessageLevel; //indicates threshold of message level. The message which has greater level than this value, can be appended to t_msg
    b_tstamp:     boolean; //indicats, whether the time stamp should be prepended, default true
  protected
    procedure SetMessages(const msgs: TStrings);
  public
    constructor Create();
    destructor  Destroy(); override;

    property  Messages: TStrings read t_msgextern write SetMessages;
    property  MessageThreshold: EMessageLevel read e_threshold write e_threshold;
    property  TimeStamp: boolean read b_tstamp write b_tstamp;

    procedure AddMessage(const text: string; const sender: string = ''; const level: EMessageLevel = ML_INFO);
    procedure ExportMessages(const fname: string);
    procedure Clear(const bextern: boolean = false);
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
  if ((level >= e_threshold) and (text <> '')) then begin
    if (sender <> '' ) then s_msg := '[' + sender + ']' + text
    else s_msg := text;
    s_msg := '[' + CSTR_MLKEYS[level] + ']' + s_msg;
    if b_tstamp then s_msg := '[' + DateTimeToStr(Now()) + ']: ' + s_msg;
    t_msgintern.Add(s_msg);
    if (t_msgextern <> t_msgintern) then t_msgextern.Add(s_msg);
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

end.

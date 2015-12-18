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

  TTextMessager = class
  protected
    t_msgextern, t_msgintern: TStrings; //saves messages. it is created in constructor and replaced by SetMessages
    e_threshold: EMessageLevel; //indicates threshold of message level. The message which has greater level than this value, can be appended to t_messages
  protected
    procedure SetMessages(const msg: TStrings);
  public
    constructor Create();
    destructor  Destroy(); override;

    property  Messages: TStrings read t_msgextern write SetMessages;
    property  MessageThreshold: EMessageLevel read e_threshold write e_threshold;
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
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

constructor TTextMessager.Create();
begin
  inherited Create();
  t_msgintern := TStringList.Create;
  t_msgextern := t_msgintern;
  e_threshold := ML_INFO;
end;

destructor TTextMessager.Destroy();
begin
  t_msgintern.Clear;
  t_msgintern.Free;
  inherited Destroy;
end;

procedure TTextMessager.AddMessage(const text: string; const level: EMessageLevel);
begin
  if ((level >= e_threshold) and (text <> '')) then
    t_msgextern.Add(format('[%s] %s: %s', [DateTimeToStr(Now()),CSTR_MLKEYS[level], text]));
end;

procedure TTextMessager.SetMessages(const msg: TStrings);
begin
  if assigned(msg) then t_msgextern := msg
  else t_msgextern := t_msgintern;
end;

end.

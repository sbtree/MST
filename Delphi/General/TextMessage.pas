// =============================================================================
// Module name  : $RCSfile: TextMessage.pas,v $
// description  : This unit implements a methodes and properties of a class  for
//                text message.
// Compiler     : Delphi 2007
// Author       : 2015-11-20 /bsu/
// History      :
//==============================================================================
unit TextMessage;

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

  TTextMessager = class(TComponent)
  protected
    t_messages: TStrings; //saves messages. it is created in constructor and replaced by SetMessages
    b_msgdel: boolean; //indicates if t_messages should be freed in destructor
    e_threshold: EMessageLevel; //indicates threshold of message level. The message which has greater level than this value, can be appended to t_messages
  protected
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure SetMessages(msg: TStrings);
  public
    constructor Create(owner: TComponent); override;
    destructor  Destroy; override;

    property Messages: TStrings read t_messages write SetMessages;
    property MessageThreshold: EMessageLevel read e_threshold write e_threshold;
  end;

implementation
uses SysUtils;

constructor TTextMessager.Create(owner: TComponent);
begin
  inherited Create(owner);
  t_messages := TStringList.Create;
  b_msgdel := true;
  e_threshold := ML_INFO;
end;

destructor TTextMessager.Destroy;
begin
  if b_msgdel then begin
    t_messages.Clear;
    FreeAndNil(t_messages);
  end;
  inherited Destroy;
end;

procedure TTextMessager.AddMessage(const text: string; const level: EMessageLevel);
begin
  if ((level >= e_threshold) and (text <> '')) then t_messages.Add(format('[%s]:%s', [DateTimeToStr(Now()), text]));
end;

procedure TTextMessager.SetMessages(msg: TStrings);
begin
  if assigned(msg) then begin
    msg.AddStrings(t_messages);
    t_messages.Clear;
    FreeAndNil(t_messages);
    b_msgdel := false;
  end;
end;

end.

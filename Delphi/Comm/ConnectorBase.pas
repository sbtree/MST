unit ConnectorBase;

interface
uses Classes, TextMessage;
type
  TConnectorBase = class(TTextMessager)
  protected
    s_name: string; //indicates the name of this connector
  protected
    function  IsConnected(): boolean; virtual;
  public
    property Name: string read s_name write s_name;
    property Connected : boolean read IsConnected;

    function Config(const sconf: string): boolean; virtual;
    function Connect(): boolean; virtual;
    function Disconnect: boolean; virtual;
    function SendPacket(const a: array of char): boolean; virtual;
    function RecvPacket(var a: array of char; const tend: cardinal): boolean; virtual;
    function SendStr(const str: string; const bprint: boolean = true): boolean; virtual;
    function RecvStr(var str: string; const bwait: boolean = true): integer; virtual;
    function RecvStrTimeout(var str: string; const tend: cardinal): integer; virtual;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = 3000): integer; virtual;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; virtual;
    function WaitForReading(const tend: cardinal): boolean; virtual;
  end;

implementation
function TConnectorBase.IsConnected(): boolean;
begin
  result := false;
  AddMessage('Virtual function ''IsConnected'' should be reimplemented.', ML_WARNING);
end;

function TConnectorBase.Config(const sconf: string): boolean;
begin
  result := false;
  AddMessage('Virtual function ''Config'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.Connect(): boolean;
begin
  result := false;
  AddMessage('Virtual function ''Connect'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.Disconnect: boolean;
begin
  result := false;
  AddMessage('Virtual function ''Disconnect'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.SendPacket(const a: array of char): boolean;
begin
  result := false;
  AddMessage('Virtual function ''SendPacket'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.RecvPacket(var a: array of char; const tend: cardinal): boolean;
begin
  result := false;
  AddMessage('Virtual function ''RecvPacket'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.SendStr(const str: string; const bprint: boolean = true): boolean;
begin
  result := false;
  AddMessage('Virtual function ''SendStr'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.RecvStr(var str: string; const bwait: boolean = true): integer;
begin
  result := 0;
  AddMessage('Virtual function ''RecvStr'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.RecvStrTimeout(var str: string; const tend: cardinal): integer;
begin
  result := 0;
  AddMessage('Virtual function ''RecvStrTimeout'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
begin
  result := 0;
  AddMessage('Virtual function ''RecvStrInterval'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean): integer;
begin
  result := 0;
  AddMessage('Virtual function ''RecvStrExpected'' should be reimplemented.', ML_WARNING);
end;


function TConnectorBase.WaitForReading(const tend: cardinal): boolean;
begin
  result := false;
  AddMessage('Virtual function ''WaitForReading'' should be reimplemented.', ML_WARNING);
end;



end.

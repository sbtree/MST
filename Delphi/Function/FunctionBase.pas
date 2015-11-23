unit FunctionBase;

interface
uses Classes, TextMessage;
type
  TStepResult = class
  protected
    b_result: boolean;
    t_value: Variant;
    s_message: string;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  EFunctionState = (
                    FS_UNDEFINED,
                    FS_PARAMETERIZED,
                    FS_INITIALIZED,
                    FS_STARTED,
                    FS_FINISHED,
                    FS_VALIDATED,
                    FS_FINALIZED
                    );

  EFunctionEvent = (
                    FE_PARAMETERIZE,
                    FE_INITIALIZE,
                    FE_START,
                    FE_FINISH,
                    FE_VALIDATE,
                    FE_FINALIZE
                    );

  TFunctionBase = class(TPersistent)
  protected
    e_fstate: EFunctionState;
    t_result: TStepResult;
    b_toexit: boolean;
    i_lasterr:integer;
    t_messager: TTextMessager;

    s_para, s_init, s_exec, s_vali, s_fina: string;
  protected
    procedure PostEvent(const event: EFunctionEvent; const ok: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetToExit();

    function Parameterize(const str: string): boolean; virtual;
    function Initilize(const str: string): boolean; virtual;
    function Execute(const str: string): boolean; virtual;
    function Validate(const str: string): boolean; virtual;
    function Finalize(const str: string): boolean; virtual;
    function GetLastError(var msg: string): integer; virtual;
    function GetResult(): TStepResult; virtual;
  end;
  TFunctionClass = class of TFunctionBase;

  StateSet= set of EFunctionState;
const
  C_FUNC_STATES: array[LOW(EFunctionEvent)..HIGH(EFunctionEvent)] of StateSet = (
                [FS_UNDEFINED, FS_PARAMETERIZED, FS_INITIALIZED, FS_FINISHED, FS_VALIDATED, FS_FINALIZED],
                [FS_PARAMETERIZED, FS_INITIALIZED, FS_FINISHED, FS_VALIDATED, FS_FINALIZED],
                [FS_INITIALIZED, FS_FINISHED, FS_VALIDATED],
                [FS_STARTED],
                [FS_FINISHED, FS_VALIDATED, FS_FINALIZED],
                [FS_INITIALIZED, FS_FINISHED, FS_VALIDATED, FS_FINALIZED]
                );

implementation
uses SysUtils;

constructor TStepResult.Create;
begin
	inherited Create;
end;

destructor TStepResult.Destroy;
begin
	inherited Destroy;
end;

procedure TFunctionBase.PostEvent(const event: EFunctionEvent; const ok: boolean);
begin
  if (e_fstate in C_FUNC_STATES[event]) then begin
    case event of
      FE_PARAMETERIZE: if ok then e_fstate := FS_PARAMETERIZED;
      FE_INITIALIZE: if ok then e_fstate := FS_INITIALIZED;
      FE_START: e_fstate := FS_STARTED;
      FE_FINISH: e_fstate := FS_FINISHED;
      FE_VALIDATE: if ok then e_fstate := FS_VALIDATED;
      FE_FINALIZE: if ok then e_fstate := FS_FINALIZED;
    end;
  end;
end;

constructor TFunctionBase.Create;
begin
	inherited Create;
  t_result := TStepResult.Create;
  b_toexit := false;
end;

destructor TFunctionBase.Destroy;
begin
  FreeAndNil(t_result);
	inherited Destroy;
end;

procedure TFunctionBase.SetToExit();
begin
  b_toexit := true;
end;

function TFunctionBase.Parameterize(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_PARAMETERIZE]);
  PostEvent(FE_PARAMETERIZE, result);
end;

function TFunctionBase.Initilize(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_INITIALIZE]);
  PostEvent(FE_INITIALIZE, result);
end;

function TFunctionBase.Execute(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_START]);
  if result then begin
    PostEvent(FE_START, true);
    PostEvent(FE_FINISH, result);
  end;

end;

function TFunctionBase.Validate(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_VALIDATE]);
  PostEvent(FE_VALIDATE, result);
end;

function TFunctionBase.Finalize(const str: string): boolean;
begin
  result := (e_fstate in C_FUNC_STATES[FE_FINALIZE]);
  PostEvent(FE_FINALIZE, result);
end;

function TFunctionBase.GetLastError(var msg: string): integer;
begin
  result := i_lasterr;
end;

function TFunctionBase.GetResult(): TStepResult;
begin
  result := t_result;
end;

end.

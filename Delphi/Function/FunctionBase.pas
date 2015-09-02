unit FunctionBase;

interface

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

  EFunctionState = (FS_READY,
                    FS_INITILIZED,
                    FS_PARAMETERIZED,
                    FS_STARTED,
                    FS_SUCCEED,
                    FS_FAILED,
                    FS_VALIDATED,
                    FS_FINALIZED
                    );

  EFunctionEvent = (FE_INITIALIZE,
                    FE_PARAMETERIZE,
                    FE_START,
                    FE_EXIT,
                    FE_VALIDATE,
                    FE_FINALIZE
                    );

  TFunctionBase = class
  protected
    e_fstate: EFunctionState;
    t_result: TStepResult;
    b_forceout: boolean;
  protected
    procedure PostEvent(const event: EFunctionEvent; const ok: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ForceToExit();

    function Initilize(const str: string): boolean; virtual;
    function Parameterize(const str: string): boolean; virtual;
    function Execute(const str: string): boolean; virtual;
    function Validate(const str: string): boolean; virtual;
    function Finalize(const str: string): boolean; virtual;
    function GetResult(): TStepResult; virtual;
  end;

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
var fe_set: set of EFunctionEvent;
begin
  case e_fstate of
    FS_READY: begin
      case event of
        FE_INITIALIZE: if ok then e_fstate := FS_INITILIZED;
        FE_PARAMETERIZE, FE_START, FE_EXIT, FE_VALIDATE, FE_FINALIZE: ;
      end;
    end;
    FS_INITILIZED: begin
      case event of
        FE_INITIALIZE:
          if ok then e_fstate := FS_INITILIZED
          else e_fstate := FS_READY;
        FE_PARAMETERIZE: if ok then e_fstate := FS_PARAMETERIZED;
        FE_START, FE_EXIT, FE_VALIDATE:;
        FE_FINALIZE: if ok then e_fstate := FS_FINALIZED;
      end;
    end;
    FS_PARAMETERIZED: begin
      case event of
        FE_INITIALIZE:
          if ok then e_fstate := FS_INITILIZED
          else e_fstate := FS_READY;
        FE_PARAMETERIZE:
          if ok then e_fstate := FS_PARAMETERIZED
          else e_fstate := FS_INITILIZED;
        FE_START: if ok then e_fstate := FS_STARTED;
        FE_EXIT, FE_VALIDATE:;
        FE_FINALIZE: if ok then e_fstate := FS_FINALIZED;
      end;
    end;
    FS_STARTED:  begin
      case event of
        FE_EXIT:
          if ok then e_fstate := FS_SUCCEED
          else e_fstate := FS_FAILED;
        FE_INITIALIZE, FE_PARAMETERIZE,FE_START,FE_VALIDATE, FE_FINALIZE: ;
      end;
    end;
    FS_SUCCEED, FS_FAILED, FS_VALIDATED, FS_VALIDATED: begin
      case event of
        FE_INITIALIZE: if ok then e_fstate := FS_INITILIZED;
        FE_PARAMETERIZE: if ok then e_fstate := FS_PARAMETERIZED;
        FE_START: if ok then e_fstate := FS_STARTED;
        FE_VALIDATE: if ok then e_fstate := FS_VALIDATED;
        FE_EXIT: ;
        FE_FINALIZE: if ok then e_fstate := FS_FINALIZED;
      end;
    end;
  end;
end;

constructor TFunctionBase.Create;
begin
	inherited Create;
  t_result := TStepResult.Create;
  b_forceout := false;
end;

destructor TFunctionBase.Destroy;
begin
  FreeAndNil(t_result);
	inherited Destroy;
end;

procedure TFunctionBase.ForceToExit();
begin
  b_forceout := true;
end;

function TFunctionBase.Initilize(const str: string): boolean;
begin
  result := true;
end;

function TFunctionBase.Parameterize(const str: string): boolean;
begin
  result := true;
end;

function TFunctionBase.Execute(const str: string): boolean;
begin
  b_forceout := false;
  result := true;
end;

function TFunctionBase.Validate(const str: string): boolean;
begin
  result := true;
end;

function TFunctionBase.Finalize(const str: string): boolean;
begin
  result := true;
end;

function TFunctionBase.GetResult(): TStepResult;
begin
  result := t_result;
end;

end.

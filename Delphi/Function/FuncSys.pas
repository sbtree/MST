// =============================================================================
// Module name  : $RCSfile: FuncSys.pas,v $
// description  : The classes of script functions which are relevant to the
//                system of this program (SYS), are implemented in this unit, e.g.
//                Jump-Function, Repeat-Controll etw.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncSys;

interface
uses Classes, FuncBase, TextMessage, StepGroup;

type
  IStepControlImpl = interface
    function GotoStep(const stepnr: string): boolean;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TStepControlHelper = class(TInterfacedObject, IStepControlImpl)
  protected
    t_group: TStepGroup;
  public
    function GotoStep(const stepnr: string): boolean;
    function GetStepGroup(): TStepGroup;
    procedure SetStepGroup(const group: TStepGroup);
    property CurStepGroup: TStepGroup read GetStepGroup write SetStepGroup;
  end;

  TConditionControl = class(TFunctionBase, IStepControlImpl)
  protected
    t_schelper: TStepControlHelper;
    s_curstepnr:  string;
    s_deststepnr: string;
  public
    constructor Create();
    destructor Destroy; override;

    function BooleanCondition(const expr: string): boolean;
    property StepControl: TStepControlHelper read t_schelper implements IStepControlImpl;
  end;

  JumpIfTrue = class(TConditionControl)
  end;

  JumpIfFalse = class(TConditionControl)

  end;

  RepeatIfTrue = class(TConditionControl)

  end;

  RepeatIfFalse = class(TConditionControl)

  end;

implementation
uses Contnrs;

function TStepControlHelper.GotoStep(const stepnr: string): boolean;
begin
  result := false;
  if assigned(t_group) then result := t_group.GotoStepNr(stepnr);
end;

function TStepControlHelper.GetStepGroup(): TStepGroup;
begin
  result := t_group;
end;

procedure TStepControlHelper.SetStepGroup(const group: TStepGroup);
begin
  t_group := group;
end;

constructor TConditionControl.Create();
begin
  inherited Create();
  t_schelper := TStepControlHelper.Create();
end;

destructor TConditionControl.Destroy;
begin
  t_schelper.Free();
  inherited Destroy();
end;

function TConditionControl.BooleanCondition(const expr: string): boolean;
begin
  //todo
  result := false;
end;

initialization
  //Classes.RegisterClass(YourSubclass);

finalization
  //Classes.UnregisterClass(YourSubclass);
end.

unit qexp_vcl;
interface
uses classes,qexp;
//This implementation a demo for vcl object function and property,not complete.
type
  TQExpVCLHelper=class(TQFunctions)
  protected
    procedure DoGetClassName(ASender:TObject);
    procedure DoFreeObject(ASender:TObject);
    procedure DoGetIsClass(ASender:TObject);
    procedure SetComponentName(ASender:TObject);
    procedure GetComponentName(ASender:TObject);
    procedure DoObjectAssigned(ASender:TQExprParser;AVar:TQVar);
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);override;
  end;
implementation

{ TQExpVCLHelper }

constructor TQExpVCLHelper.Create;
begin
inherited;
end;

destructor TQExpVCLHelper.Destroy;
begin

  inherited;
end;

procedure TQExpVCLHelper.DoFreeObject(ASender: TObject);
var
  AObj:TObject;
begin
AObj:=TObject(TQVar(ASender).Parent.Value.AsInteger);
if Assigned(AObj) then
  AObj.Free;
end;

procedure TQExpVCLHelper.DoGetClassName(ASender: TObject);
var
  AObj:TObject;
begin
AObj:=TObject(TQVar(ASender).Parent.Value.AsInteger);
if Assigned(AObj) then
  TQVar(ASender).Value.AsString:=AObj.ClassName;
end;

procedure TQExpVCLHelper.DoGetIsClass(ASender: TObject);
var
  AFunc:TQFunction;
  AObj:TObject;
  AClass:TClass;
  AClassName:WideString;
begin
AFunc:=TQFunction(ASender);
AObj:=TObject(AFunc.Parent.Value.AsInteger);
if Assigned(AObj) then
  begin
  AClass:=AObj.ClassType;
  AClassName:=AFunc.Parameters[0].Value.AsString;
  repeat
    if AClass.ClassName=AClassName then
      begin
      AFunc.Value.AsBoolean:=True;
      Break;
      end
    else
      AClass:=AClass.ClassParent;
  until not Assigned(AClass);
  end;
end;

procedure TQExpVCLHelper.DoObjectAssigned(ASender: TQExprParser; AVar: TQVar);
var
  AObj:TObject;
begin
AVar:=AVar.Add('ClassName',True);
AVar.OnGetValue:=DoGetClassName;
AVar.AddFunction('Free',DoFreeObject,'',False);
AVar.AddFunction('IsClass',DoGetIsClass,'AClassName',False);
AObj:=TObject(AVar.Value.AsInteger);
if AObj is TComponent then
  begin
  AVar:=AVar.Add('Name',True);
  AVar.OnGetValue:=GetComponentName;
  AVar.OnValueChange:=SetComponentName;
  end;
end;

procedure TQExpVCLHelper.GetComponentName(ASender: TObject);
var
  AComp:TComponent;
  AFunc:TQFunction;
begin
AFunc:=TQFunction(ASender);
AComp:=TComponent(AFunc.Parent.Value.AsInteger);
if Assigned(AComp) then
  begin
  AFunc.Value.AsString:=AComp.Name;
  end;
end;

procedure TQExpVCLHelper.Register(AParser: TQExprParser);
begin
//Nothing,Just a helper needed
end;

procedure TQExpVCLHelper.SetComponentName(ASender: TObject);
var
  AComp:TComponent;
  AFunc:TQFunction;
begin
AFunc:=TQFunction(ASender);
AComp:=TComponent(AFunc.Parent.Value.AsInteger);
if Assigned(AComp) then
  AComp.Name:=AFunc.Parameters[0].Value.AsString;
end;

end.

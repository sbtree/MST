unit fmx.modaldlg;

{ FMX.ModalDlg FMX 下 ShowModal 的增强和接口统一
  本单元统一了 FMX 和 TForm.ShowModal 的操作接口，原来 Delphi 自带的 ShowModal 的
  匿名函数版本传递的只是一个 ModalResult 参数，无法通过局部变量进一步控制。此版本
  参数改为窗体的实例，你可以访问其 ModalResult 以及各个相关的成员，以方便进一步控
  制。
  受平台限制，Android 下并不是实际的ShowModal(不过和 ShowModal 差不多，因为它都是
  全屏覆盖的，下层的窗口你也操作不了），而其它平台都是真正的模态窗口。
  2016.3.4
  ========
  + 1.0 版
}
interface

uses classes, sysutils,fmx.forms, uitypes;

type
  TFormModalProc = reference to procedure(F: TForm);
  TFormClass = class of TForm;
  /// <summary>显示一个模态窗口</summmary>
  /// <param name="F">窗口实例</param>
  /// <param name="OnResult">用户关闭窗口时的操作</param>
  /// <param name="ACloseAction">窗口关闭时的动作，默认caFree释放掉</param>
  /// <remarks>在 Windows/iOS/OSX 上是真正 ShowModal 出来，然后调用 OnResult，而在
  /// Android 上，是 Show 以后，在用户关闭或设置 ModalResult 时调用的 OnResult。
  /// 也就是说，Android 上受平台限制是模拟的 ShowModal 效果。</remarks>
procedure ModalDialog(F: TForm; OnResult: TFormModalProc;
  ACloseAction: TCloseAction = TCloseAction.caFree); overload;
/// <summary>显示一个模态窗口，并在关闭时释放</summmary>
/// <param name="F">窗口实例</param>
/// <param name="OnResult">用户关闭窗口时的操作</param>
/// <remarks>在 Windows/iOS/OSX 上是真正 ShowModal 出来，然后调用 OnResult，而在
/// Android 上，是 Show 以后，在用户关闭或设置 ModalResult 时调用的 OnResult。
/// 也就是说，Android 上受平台限制是模拟的 ShowModal 效果。</remarks>
procedure ModalDialog(AClass: TFormClass; OnResult: TFormModalProc); overload;

implementation

type
  TFormModalHook = class(TComponent)
  private
    FForm: TForm;
    FCloseAction: TCloseAction;
    FOldClose: TCloseEvent;
    FResultProc: TFormModalProc;
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowModal(AResult: TFormModalProc);
  end;

procedure ModalDialog(F: TForm; OnResult: TFormModalProc;
  ACloseAction: TCloseAction = TCloseAction.caFree);
var
  AHook: TFormModalHook;
begin
  AHook := TFormModalHook.Create(F);
  AHook.FCloseAction := ACloseAction;
  AHook.ShowModal(OnResult);
end;

procedure ModalDialog(AClass: TFormClass; OnResult: TFormModalProc); overload;
var
  F: TForm;
begin
  F := AClass.Create(Application);
  ModalDialog(F, OnResult, TCloseAction.caFree);
end;
{ TFormModalHook }

constructor TFormModalHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := AOwner as TForm;
  FOldClose := FForm.OnClose;
  FForm.OnClose := DoFormClose;
  FCloseAction := TCloseAction.caFree;
end;

procedure TFormModalHook.DoFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FForm.ModalResult = mrNone then
    FForm.ModalResult := mrCancel;
  Action := FCloseAction;
  if Assigned(FOldClose) then
    FOldClose(Sender, Action);
end;

procedure TFormModalHook.ShowModal(AResult: TFormModalProc);
begin
  FResultProc := AResult;
{$IFDEF ANDROID}
  FForm.ShowModal(
    procedure(AResult: TModalResult)
    var
      AHook: TFormModalHook;
      AForm: TForm;
      I: Integer;
      AChild: TComponent;
    begin
      if Screen.ActiveForm is TForm then // 关闭时，它是活动的窗口，如果不是，那就是出鬼了
        AForm := Screen.ActiveForm as TForm
      else
      begin
        raise Exception.Create('You should not in here.');
      end;
      if Assigned(AForm) then
      begin
        AForm.OnClose := FOldClose;
        for I := 0 to AForm.ComponentCount - 1 do
        begin
          AChild := FForm.Components[I];
          if AChild is TFormModalHook then
          begin
            (AChild as TFormModalHook).FResultProc(AForm);
            FreeAndNil(AChild);
            Break;
          end;
        end;
      end;
    end);
{$ELSE}
  FForm.ShowModal;
  FResultProc(FForm);
{$ENDIF}
end;

end.

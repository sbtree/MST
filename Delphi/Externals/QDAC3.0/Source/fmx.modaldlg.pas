unit fmx.modaldlg;

{ FMX.ModalDlg FMX �� ShowModal ����ǿ�ͽӿ�ͳһ
  ����Ԫͳһ�� FMX �� TForm.ShowModal �Ĳ����ӿڣ�ԭ�� Delphi �Դ��� ShowModal ��
  ���������汾���ݵ�ֻ��һ�� ModalResult �������޷�ͨ���ֲ�������һ�����ơ��˰汾
  ������Ϊ�����ʵ��������Է����� ModalResult �Լ�������صĳ�Ա���Է����һ����
  �ơ�
  ��ƽ̨���ƣ�Android �²�����ʵ�ʵ�ShowModal(������ ShowModal ��࣬��Ϊ������
  ȫ�����ǵģ��²�Ĵ�����Ҳ�������ˣ���������ƽ̨����������ģ̬���ڡ�
  2016.3.4
  ========
  + 1.0 ��
}
interface

uses classes, sysutils,fmx.forms, uitypes;

type
  TFormModalProc = reference to procedure(F: TForm);
  TFormClass = class of TForm;
  /// <summary>��ʾһ��ģ̬����</summmary>
  /// <param name="F">����ʵ��</param>
  /// <param name="OnResult">�û��رմ���ʱ�Ĳ���</param>
  /// <param name="ACloseAction">���ڹر�ʱ�Ķ�����Ĭ��caFree�ͷŵ�</param>
  /// <remarks>�� Windows/iOS/OSX �������� ShowModal ������Ȼ����� OnResult������
  /// Android �ϣ��� Show �Ժ����û��رջ����� ModalResult ʱ���õ� OnResult��
  /// Ҳ����˵��Android ����ƽ̨������ģ��� ShowModal Ч����</remarks>
procedure ModalDialog(F: TForm; OnResult: TFormModalProc;
  ACloseAction: TCloseAction = TCloseAction.caFree); overload;
/// <summary>��ʾһ��ģ̬���ڣ����ڹر�ʱ�ͷ�</summmary>
/// <param name="F">����ʵ��</param>
/// <param name="OnResult">�û��رմ���ʱ�Ĳ���</param>
/// <remarks>�� Windows/iOS/OSX �������� ShowModal ������Ȼ����� OnResult������
/// Android �ϣ��� Show �Ժ����û��رջ����� ModalResult ʱ���õ� OnResult��
/// Ҳ����˵��Android ����ƽ̨������ģ��� ShowModal Ч����</remarks>
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
      if Screen.ActiveForm is TForm then // �ر�ʱ�����ǻ�Ĵ��ڣ�������ǣ��Ǿ��ǳ�����
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

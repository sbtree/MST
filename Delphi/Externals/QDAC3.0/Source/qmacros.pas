unit qmacros;

{
  QMacrosʹ�û��ں��滻�ļ��������ģ�����ݵĿ����滻��QMacros����ɫ���ڣ�
  �� ֧�����滻ʱ�Զ�������ʼ�ͽ����ַ��������߿���һ����Ҳ���Բ�һ������һ��Ҫ��֤�궨���м䲻����ָ��ַ���
  �� ����ջʽ����ʹ��ʹ�ú������ջ��ֵ�滻������ʹ����ֵʱ�����Գ�ջ�ָ�ĩ�ε�ֵ��
  �� ֧�ֱ���㹦�ܣ��������ñ���㲢���������ջ�µĺ궨�壬ֻ��Ҫ�����ԭ������ı���㣬�Ϳ��Իָ����б���㱣��ʱ�ĺ궨��ԭ״��
  �� ֧�ִ�Сд���ֺͺ��Դ�Сд���ִ���ģʽ��
  �� ֧�ֶ�̬ȡֵ�궨�壨�൱�ڼ򵥵ĺ������������ú궨��ÿ���滻ʱ��Ӧ��ͬ��ֵ��
  �� ʹ�ö��ַ����Һ궨�����ƣ�����Ľ����滻�ٶȣ�
  �� ֧��ģ��Ԥ����
  ������Ϣ������ʹ�����QMacrosר��
}
interface

{
  ��Դ������QDAC��Ŀ����Ȩ��swish(QQ:109867294)���С�
  (1)��ʹ����ɼ�����
  ���������ɸ��ơ��ַ����޸ı�Դ�룬�������޸�Ӧ�÷��������ߣ������������ڱ�Ҫʱ��
  �ϲ�������Ŀ���Թ�ʹ�ã��ϲ����Դ��ͬ����ѭQDAC��Ȩ�������ơ�
  ���Ĳ�Ʒ�Ĺ����У�Ӧ�������µİ汾����:
  ����Ʒʹ�õĺ����������QDAC��Ŀ�е�QMacros����Ȩ���������С�
  (2)������֧��
  �м������⣬�����Լ���QDAC�ٷ�QQȺ250530692��ͬ̽�֡�
  (3)������
  ����������ʹ�ñ�Դ�������Ҫ֧���κη��á���������ñ�Դ������а�������������
  ������Ŀ����ǿ�ƣ�����ʹ���߲�Ϊ�������ȣ��и���ľ���Ϊ�����ָ��õ���Ʒ��
  ������ʽ��
  ֧������ guansonghuan@sina.com �����������
  �������У�
  �����������
  �˺ţ�4367 4209 4324 0179 731
  �����У��������г����ŷ索����
}

{ �޶���־
  2016.3.23
  ==========
  * ����������Ч�ַ�����ʱ�������λ���ַ�����βʱ���������(�ڰ�ɱ�ֱ��棩

  2016.3.22
  ==========
  * ������MRF_ENABLE_ESCAPE��ת���ض��ַ���֧�֣�����ͬJSON
  * ������MRF_IGNORE_MISSED��־λ��֧�ֺ���δ����ĺ�

  2015.12.17
  ==========
  + ������������֧�֣���л������
  2015.5.22
  ==========
  * ������ InternalCompile ��,CompareMemʱ LS ֵ���Ե�����
  2015.4.30
  ==========
  * ֧�ֺ���ָ�����ַ���ʼ������Ч�ı�־�ַ�����
  * ������Ϊ��ʱ���׳��쳣��ʾ������δָ��

  2015.2.26
  =========
  * ��������2007�µı�����󣨸�л�����ٷʣ�
  2014.12.15
  ==========
  *  �����˱���ΪAndriod����ʱ����InternalComplie�����������������ٷʱ��棩
  2014.12.12
  ==========
  * ��ʼ�汾

}
uses classes, sysutils, math, db, qstring, qjson{$IFDEF UNICODE},
  Generics.Collections{$ENDIF};

const
  MRF_IN_DBL_QUOTER = $01; // �Ƿ��滻˫�����м������
  MRF_IN_SINGLE_QUOTER = $02; // �Ƿ��滻�������м������
  MRF_DELAY_BINDING = $04; // �Ƿ��ӳٰ󶨺궨�壬����ǵĻ������ڵ�һ��Replace֮ǰ�����Ƿ�궼�Ѿ���
  MRF_END_WITH_INVALID_CHAR = $08; // ����ָ�����ַ���ʼ�����ԷǱ�־�ַ�����
  MRF_PARSE_PARAMS = $10; // ����������Ĳ���
  MRF_ENABLE_ESCAPE = $20; // ����ת��֧��
  MRF_IGNORE_MISSED = $40; // �����޷�ʶ��ĺ����ƣ�������Ϊ����ͨ���ַ���
  MFF_FILETYPE = $736F7263614D51; // �����ļ���־�����������������ļ�

{$HPPEMIT '#pragma link "qmacros"'}

type
  TQMacroManager = class;
  TQMacroItem = class;
  /// <summary>
  /// ���Ҷ�̬���ֵʱ��ͨ���ص�������ȡ��Ӧ��ֵ������ֱֵ�Ӹ���AMacro.Value.Value���ɡ�
  /// </summary>
  /// <param name="AMacro">��</param>
  /// <param name="AQuoter">�������ͣ�ֵ������#0(0)��Ӣ�ĵĵ����Ż�˫����</param>
  TQMacroValueFetchEvent = procedure(AMacro: TQMacroItem; const AQuoter: QCharW)
    of object;
  TQMacroValueFetchEventG = procedure(AMacro: TQMacroItem;
    const AQuoter: QCharW);
{$IFDEF UNICODE}
  TQMacroValueFetchEventA = reference to procedure(AMacro: TQMacroItem;
    const AQuoter: QCharW);
{$ENDIF}
  /// <summary>ָ����ָ���ĺ�ʱ�������¼�</summary>
  /// <param name="ASender">�������</param>
  /// <param name="AName">������</param>
  /// <param name="AQuoter">�������ͣ�ֵ������#0(\0)��Ӣ�ĵĵ����Ż�˫����</param>
  /// <param name="AHandled">����¼������˺겻������⣬������ΪTrue��������������</param>
  TQMacroMissEvent = procedure(ASender: TQMacroManager; AName: QStringW;
    const AQuoter: QCharW; var AHandled: Boolean) of object;
  TQMacroMissEventG = procedure(ASender: TQMacroManager; AName: QStringW;
    const AQuoter: QCharW; var AHandled: Boolean);
{$IFDEF UNICODE}
  TQMacroMissEventA = reference to procedure(ASender: TQMacroManager;
    AName: QStringW; const AQuoter: QCharW; var AHandled: Boolean);
{$ENDIF}
  TQMacroCharType = (mctChar, mctNameStart, mctNameEnd);
  TQMacroNameTestEvent = procedure(ASender: TQMacroManager; p: PQCharW;
    var ALen: Integer; var AType: TQMacroCharType) of object;
  TQMacroNameTestEventG = procedure(ASender: TQMacroManager; p: PQCharW;
    var ALen: Integer; var AType: TQMacroCharType);
{$IFDEF UNICODE}
  TQMacroNameTestEventA = reference to procedure(ASender: TQMacroManager;
    p: PQCharW; var ALen: Integer; var AType: TQMacroCharType);
{$ENDIF}
  /// <summary>
  /// �궨��ֵ���ȶ��Զ���
  /// </summary>
  /// <param name="mvImmutable">ֵ�ǹ̶�����ģ���һ�ֳ���״̬</param>
  /// <param name="mvStable">ֵ����Բ���ģ���һ���滻���������У��������״γ��ֵ�λ���⣬���Ƕ�������Ϊ��ȼ۷�mvImmutable</param>
  /// <param name="mvVolatile">ֵ���ױ�ģ�ÿ�λ�ȡ���ֵʱ�����ص�ֵ�������ǲ�ͬ��</param>
  TQMacroVolatile = (mvImmutable,
    /// ֵ�ǹ̶�����ģ���һ�ֳ���״̬
    mvStable,
    /// ֵ����Բ���ģ���һ���滻���������У��������״γ��ֵ�λ���⣬���Ƕ�������Ϊ��ȼ۷�mvImmutable
    mvVolatile
    /// ֵ���ױ�ģ�ÿ�λ�ȡ���ֵʱ�����ص�ֵ�������ǲ�ͬ��
    );

  // TQMacroValue���͵�ָ�����Ͷ���
  PQMacroValue = ^TQMacroValue;

  // ������ֵ����
  TQMacroValue = record
    Value: QStringW; // ��ǰֵ����ֵ̬�̶�ʱʹ����
    OnFetchValue: TQMacroValueFetchEvent; // ��̬��ȡֵʱʹ����
    Tag: IntPtr; // �û����ӱ�ǩ
    SavePoint: Integer; // �����
    ReplaceId: Integer; // �滻�ڲ���ǣ�������һ�λ����л���mvStable���͵�ֵ
    Volatile: TQMacroVolatile; // �ȶ���
    /// �����һ��ȡֵ
    /// ���ǰһ��ȡֵ
    Prior, Next: PQMacroValue; // ��������ʵ��ջʽ����
  end;

  // ��һ����Ŀ
  TQMacroItem = class
  protected
    FName: QStringW;
    FValue: PQMacroValue;
    FParams: TQJson;
    FOwner: TQMacroManager;
  public
    constructor Create(AOwner: TQMacroManager); overload;
    destructor Destroy; override;
    property Name: QStringW read FName; // ������
    property Value: PQMacroValue read FValue; // ��ֵ
    property Owner: TQMacroManager read FOwner; // ������
    property Params: TQJson read FParams; // ��ȡֵʱ��ʱ�ɱ���������������ֵ
  end;
{$IFDEF UNICODE}

  TMacroList = TList<TQMacroItem>; // ���б�
{$ELSE}
  TMacroList = TList; // ���б� <D2009
{$ENDIF}

  // �������ĵ����
  TQMacroCompliedItem = record
    Start: PWideChar; // ��ʼƫ��
    Length: Integer; // ���ȣ���λΪ�ַ�
    Quoter: QCharW; // �������
    IsMacro: Boolean; // �Ƿ��Ǻ궨��
    Macro: TQMacroItem; // ����Ǻ궨�壬���Ӧ����Ӧ�ĺ궨��
    Params: TQJson;
    Text: QStringW;
  end;

  // TQMacroCompliedItem��ָ�����Ͷ���
  PQMacroCompliedItem = ^TQMacroCompliedItem;
  // TQMacroCompliedItem��̬���鶨��
  TQCompliedArray = array of TQMacroCompliedItem;

  // TQMacroComplied���ڱ���������Ҫ�滻���ݵ���Ϣ���Լ��ٶ���滻
  TQMacroComplied = class
  protected
    FOwner: TQMacroManager; // ������
    FMinSize: Integer; // �����С��Ҫ������ڴ泤��
    FCount: Integer; // ��Ŀ����
    FVolatiles: Integer; // �ױ��������
    FDelayCount: Integer; // �ӳٰ󶨵ĺ�����
    FPushId: Integer; // ���ڼ�����ջ�仯���Ծ����Ƿ����滻ʱ���б����������
    FText: QStringW; // ԭʼҪ�滻���ı�
    FReplacedText: QStringW; // ĩ���滻����������Լ����滻����
    FItems: TQCompliedArray; // ������Ŀ
    FFlags: Integer;
    function MacroNeeded(const AName: QStringW): TQMacroItem;
  public
    constructor Create(AOwner: TQMacroManager); overload;
    destructor Destroy; override;
    /// <summary>�����м���Ԥ����ĺ��滻��Ϣ</summary>
    /// <param name="AStream">Դ������</param>
    procedure LoadFromStream(AStream: TStream);
    /// <summary>���ļ��м���Ԥ����ĺ��滻��Ϣ</summary>
    /// <param name="AFileName">Դ�ļ���</param>
    procedure LoadFromFile(const AFileName: QStringW);
    /// <summary>���浱ǰԤ����ĺ��滻��Ϣ��������</summary>
    /// <param name="AStream">Ŀ��������</param>
    procedure SaveToStream(AStream: TStream);
    /// <summary>���浱ǰԤ����ĺ��滻��Ϣ���ļ���</summary>
    /// <param name="AStream">Ŀ���ļ���</param>
    procedure SaveToFile(const AFileName: QStringW);
    /// <summary>ִ��һ���滻����</summary>
    /// <returns>�����滻����ַ���</returns>
    function Replace: QStringW;
    /// <summary>ʹ�ú궨���滻ָ�����ı�</summary>
    /// <param name="AText">Ҫ���滻���ı�</param>
    /// <param name="AMacroStart">�궨����ʼ�ַ�������������ַ���ͬ</param>
    /// <param name="AMacroEnd">�궨������ַ�</param>
    /// <param name="AFlags">��־λ����ȡ MRF_IN_DBL_QUOTER �� MRF_IN_SINGLE_QUOTER �����</param>
    /// <returns>����ɹ�������True��ʧ�ܣ�����False</returns>
    function Complie(AText: QStringW; AMacroStart, AMacroEnd: QStringW;
      const AFlags: Integer = MRF_ENABLE_ESCAPE): Boolean;
    /// <summary>ö���õ��ĺ�����</summary>
    /// <param name="AList">�������������Ƶ��б�</param>
    /// <returns>����ʹ�õĺ������</returns>
    function EnumUsedMacros(AList: TStrings): Integer;
    /// <summary>����������õĺ궨���Ƿ���Ч</summary>
    /// <remarks>��������⣬���׳��쳣ָ��������ĺ궨��</returns>
    procedure CheckMacros;
    property MinSize: Integer read FMinSize; // �滻ʱ���滻�����С�Ĵ�С���ַ�����
    property Count: Integer read FCount; // ���������Ŀ��
    property Items: TQCompliedArray read FItems; // ����������
    property Volatiles: Integer read FVolatiles; // ��̬�仯�ĺ궨������
    property Owner: TQMacroManager read FOwner; // ������
    property Flags: Integer read FFlags; // ����ʱ���õı�־λ
  end;

  /// <summary>
  /// TQMacroManager���ڹ�����֪�ĺ궨�壬���ṩ���ֻ����Ĳ���֧�֡�
  /// </summary>
  TQMacroManager = class
  private
    FIgnoreCase: Boolean;
    FOnMacroMissed: TQMacroMissEvent;
    FOnTestNameStart: TQMacroNameTestEvent;
    FOnTestNameEnd: TQMacroNameTestEvent;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TQMacroItem;
    function GetValues(AName: QStringW): QStringW;
    procedure SetOnTestNameEnd(const Value: TQMacroNameTestEvent); overload;
    procedure SetOnTestNameStart(const Value: TQMacroNameTestEvent); overload;
    procedure SetOnMacroMissed(AHandler: TQMacroMissEvent); overload;
  protected
    FMacroes: TMacroList;
    FSavePoint: Integer;
    FReplaceId: Integer;
    FStableCount: Integer;
    FVolatileCount: Integer;
    FLastPushId: Integer;
    FBooleanAsInt: Boolean;
    function InternalMacroValue(const AName: QStringW; const AQuoter: QCharW;
      var AValue: QStringW): Integer;
    function MacroValue(const AName: QStringW; const AQuoter: QCharW)
      : QStringW; overload;
    function DateTimeToStr(ADateTime: TDateTime): String;
    procedure DoFetchFieldValue(AMacro: TQMacroItem; const AQuoter: QCharW);
    procedure DoFetchFieldQuotedValue(AMacro: TQMacroItem;
      const AQuoter: QCharW);
    function IncW(p: PQCharW; ALen: Integer): PQCharW; inline;
    procedure InternalPush(AMacro: TQMacroItem; const AValue: QStringW;
      AOnFetch: TQMacroValueFetchEvent; AStable: TQMacroVolatile; ATag: IntPtr);
    procedure InternalComplie(var AResult: TQMacroComplied; AText: QStringW;
      AMacroStart, AMacroEnd: QStringW; const AFlags: Integer);
    function DoMacroMissed(AName: QStringW; const AQuoter: QCharW): Boolean;
    procedure DoFetchValue(AMacro: TQMacroItem; const AQuoter: QCharW);
    function CharUnescape(var p: PQCharW): QCharW;
    function Unescape(const S: QStringW): QStringW; overload;
    function Unescape(p: PQCharW; ALen: Integer): QStringW; overload;
  public
    /// <summary>���캯��</summary>
    constructor Create; overload;
    /// <summary>��������</summary>
    destructor Destroy; override;
    /// <summary>��ջָ�����ƺ�ֵ�ĺ궨��</summary>
    /// <param name="AName">������</param>
    /// <param name="AValue">���Ӧ�ľ����ַ���ֵ</param>
    procedure Push(const AName, AValue: QStringW); overload;
    /// <summary>��ջָ�����ƺ�ֵ�ĺ궨��</summary>
    /// <param name="AName">������</param>
    /// <param name="AOnFetchValue">��ȡ���Ӧ��ֵʱ���õĻص�����</param>
    /// <param name="AVolatile">��������ֵ���ȶ���</param>
    /// <param name="ATag">�û����ӵı�ǩ����</param>
    procedure Push(const AName: QStringW; AOnFetchValue: TQMacroValueFetchEvent;
      AVolatile: TQMacroVolatile = mvVolatile; ATag: IntPtr = 0); overload;

    /// <summary>��ջָ�����ƺ�ֵ�ĺ궨��</summary>
    /// <param name="AName">������</param>
    /// <param name="AOnFetchValue">��ȡ���Ӧ��ֵʱ���õĻص�����</param>
    /// <param name="AVolatile">��������ֵ���ȶ���</param>
    /// <param name="ATag">�û����ӵı�ǩ����</param>
    procedure Push(const AName: QStringW;
      AOnFetchValue: TQMacroValueFetchEventG;
      AVolatile: TQMacroVolatile = mvVolatile; ATag: IntPtr = 0); overload;
{$IFDEF UNICODE}
    /// <summary>��ջָ�����ƺ�ֵ�ĺ궨��</summary>
    /// <param name="AName">������</param>
    /// <param name="AOnFetchValue">��ȡ���Ӧ��ֵʱ���õĻص�����</param>
    /// <param name="AVolatile">��������ֵ���ȶ���</param>
    /// <param name="ATag">�û����ӵı�ǩ����</param>
    procedure Push(const AName: QStringW;
      AOnFetchValue: TQMacroValueFetchEventA;
      AVolatile: TQMacroVolatile = mvVolatile; ATag: IntPtr = 0); overload;
{$ENDIF}
    /// <summary>��ջָ�����ݼ��������ֶ�Ϊ�궨��</summary>
    /// <param name="ADataSet">���ݼ�����</param>
    /// <param name="ANameSpace">����ǰ׺�������Ϊ�գ���Ϊǰ׺.�ֶ����ĺ궨������</param>

    procedure Push(ADataSet: TDataSet; const ANameSpace: QStringW); overload;
    /// <summary>��ջָ�����Ƶĺ궨��</summary>
    /// <param name="AName">Ҫ��ջ�ĺ궨������</param>
    procedure Pop(const AName: QStringW); overload;
    /// <summary>��ջָ�����ݼ����ϴ���ջ�����к궨��</summary>
    /// <param name="ADataSet">���ݼ�����</param>
    /// <param name="ANameSpace">����ǰ׺�������Ϊ�գ���Ϊǰ׺.�ֶ����ĺ궨������</param>
    procedure Pop(ADataSet: TDataSet; const ANameSpace: QStringW); overload;
    /// <summary>����һ������㣬�Ա����ʹ��Restore��ֱ�ӻָ�</summary>
    /// <returns>���ر������</returns>
    /// <remarks>SavePoint��Restore�����ʹ�õģ����صı�������ڻָ�ʱ��Ϊ����ʹ��</remarks>
    function SavePoint: Integer;
    /// <summary>�ָ���ָ���ı����</summary>
    /// <param name="ASavePoint">Ҫ����ı������</param>
    /// <remarks>��������֮����ջ�����к궨�嶼������ջ</remarks>
    procedure Restore(ASavePoint: Integer);
    /// <summary>����ָ�����Ƶĺ궨�������</summary>
    /// <param name="AName">������</param>
    /// <returns>�ҵ�������ָ����������ʧ�ܣ�����-1</returns>
    function IndexOf(const AName: QStringW): Integer;
    /// <summary>����ָ�����Ƶĺ궨��</summary>
    /// <param name="AName">������</param>
    /// <param name="AIndex">���ڽ��շ��ص�����</param>
    /// <returns>�ҵ�������true��ʧ�ܣ�����false</returns>
    /// <remarks>���ʧ�ܣ�AIndex���ص���ָ����ֵӦ�ó��ֵ�λ��</remarks>
    function Find(const AName: QStringW; var AIndex: Integer): Boolean;
    /// <summary>������к궨��</summary>
    /// <remarks>ע�⣺�������ɾ�����еĺ궨�壬�������Ǳ����λ��</remarks>
    procedure Clear;
    /// <summary>ʹ�ú궨���滻ָ�����ı�</summary>
    /// <param name="AText">Ҫ���滻���ı�</param>
    /// <param name="AMacroStart">�궨����ʼ�ַ�������������ַ���ͬ</param>
    /// <param name="AMacroEnd">�궨������ַ�</param>
    /// <param name="AFlags">��־λ����ȡ MRF_IN_DBL_QUOTER �� MRF_IN_SINGLE_QUOTER �����</param>
    /// <returns>����ɹ������ر����м��������ʧ�ܣ����ؿգ����ص�TQComplied����ֱ����Free�ͷ�</returns>
    function Complie(AText: QStringW; AMacroStart, AMacroEnd: QStringW;
      const AFlags: Integer = MRF_ENABLE_ESCAPE): TQMacroComplied;
    /// <summary>ʹ��ָ���ı�����ִ��һ���滻����</summary>
    /// <param name="AHandle">ʹ��Complie����������м���</param>
    /// <returns>�����滻���</returns>

    function Replace(AHandle: TQMacroComplied): QStringW; overload;
    /// <summary>ʹ�ú궨���滻ָ�����ı�</summary>
    /// <param name="AText">Ҫ���滻���ı�</param>
    /// <param name="AMacroStart">�궨����ʼ�ַ�������������ַ���ͬ</param>
    /// <param name="AMacroEnd">�궨������ַ�</param>
    /// <param name="AFlags">��־λ����ȡ MRF_IN_DBL_QUOTER �� MRF_IN_SINGLE_QUOTER �����</param>
    /// <returns>�����滻��ɵĽ���ַ���</returns>
    function Replace(const AText: QStringW; AMacroStart, AMacroEnd: QStringW;
      const AFlags: Integer = MRF_ENABLE_ESCAPE): QStringW; overload;
    /// <summary>��ָָ�����Ƶĺ�ĵ�ǰֵ�����δ�ҵ����׳��쳣</summary>
    /// <param name="AName">������</param>
    /// <returns>����ָ���ĺ�ĵ�ǰֵ</returns>
    function MacroValue(const AName: QStringW): QStringW; overload;
    procedure SetTestNameEnd(const Value: TQMacroNameTestEventG); overload;
    procedure SetTestNameStart(const Value: TQMacroNameTestEventG); overload;
    procedure SetMacroMissed(AHandler: TQMacroMissEventG); overload;
{$IFDEF UNICODE}
    procedure SetTestNameEnd(const Value: TQMacroNameTestEventA); overload;
    procedure SetTestNameStart(const Value: TQMacroNameTestEventA); overload;
    procedure SetMacroMissed(AHandler: TQMacroMissEventA); overload;
{$ENDIF}
    /// <summary>�����</summary>
    property Count: Integer read GetCount;
    /// <summary>���б�</summary>
    property Items[AIndex: Integer]: TQMacroItem read GetItems; default;
    /// <summary>�����Ƿ����ִ�Сд</summary>
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    /// <summary>ָ�����ƺ��ֵ����������ڣ����ؿ��ַ���</summary>
    property Values[AName: QStringW]: QStringW read GetValues;
    /// <summary>�ں�δ�ҵ��������¼���������ڸ��¼���Ϊ�丳Ĭ��ֵ</summary>
    property OnMacroMissed: TQMacroMissEvent read FOnMacroMissed
      write SetOnMacroMissed;
    property OnTestNameStart: TQMacroNameTestEvent read FOnTestNameStart
      write SetOnTestNameStart;
    property OnTestNameEnd: TQMacroNameTestEvent read FOnTestNameEnd
      write SetOnTestNameEnd;
    property BooleanAsInt: Boolean read FBooleanAsInt write FBooleanAsInt;
  end;

implementation

resourcestring
  SMacroValueUnknown = 'ָ���ĺ� %s ��ֵδ���塣';
  SMacroNotFound = 'ָ���ĺ� %s δ���塣';
  STagNotClosed = 'ָ���ĺ� %s �Ľ�����ǩδ�ҵ���';
  SMacroStartNeeded = 'ֻ�����˺�����ַ�����δָ���궨�忪ʼ�ַ�����';
  SBadFileFormat = '��Ч��QMacrosԤ���뻺�����ݡ�';
  STagNameMissed = '�� %d �� %d �к�����δָ��:'#13#10'%s'#13#10;
  SCharNeeded = '��ǰλ��Ӧ���� "%s" �������� "%s"��';

  { TQMacroManager }
type
  TQMacroCompliedFileHeader = record
    Flags: Int64;
    Version: Integer; // �汾��
    Count: Integer; // �������
    MinSize: Integer; //
    Volatiles: Integer;
    TextLength: Integer;
  end;

  TQMacoCompliedFileItem = record
    Start: Integer;
    Length: Integer;
    Quoter: QCharW;
    IsMacro: Boolean;
  end;

function EventEqual(AHandler1, AHandler2: TMethod): Boolean;
begin
  Result := (AHandler1.Code = AHandler2.Code) and
    (AHandler1.Data = AHandler2.Data);
end;

function TQMacroManager.CharUnescape(var p: PQCharW): QCharW;
  function DecodeOrd: Integer;
  var
    C: Integer;
  begin
    Result := 0;
    C := 0;
    while (p^ <> #0) and (C < 4) do
    begin
      if IsHexChar(p^) then
        Result := (Result shl 4) + HexValue(p^)
      else
        Break;
      Inc(p);
      Inc(C);
    end
  end;

begin
  if p^ = #0 then
  begin
    Result := #0;
    Exit;
  end;
  if p^ <> '\' then
  begin
    Result := p^;
    Inc(p);
    Exit;
  end;
  Inc(p);
  case p^ of
    'b':
      begin
        Result := #7;
        Inc(p);
      end;
    't':
      begin
        Result := #9;
        Inc(p);
      end;
    'n':
      begin
        Result := #10;
        Inc(p);
      end;
    'f':
      begin
        Result := #12;
        Inc(p);
      end;
    'r':
      begin
        Result := #13;
        Inc(p);
      end;
    '\':
      begin
        Result := '\';
        Inc(p);
      end;
    '''':
      begin
        Result := '''';
        Inc(p);
      end;
    '"':
      begin
        Result := '"';
        Inc(p);
      end;
    'u':
      begin
        // \uXXXX
        if IsHexChar(p[1]) and IsHexChar(p[2]) and IsHexChar(p[3]) and
          IsHexChar(p[4]) then
        begin
          Result := WideChar((HexValue(p[1]) shl 12) or (HexValue(p[2]) shl 8)
            or (HexValue(p[3]) shl 4) or HexValue(p[4]));
          Inc(p, 5);
        end
        else
          raise Exception.CreateFmt(SCharNeeded,
            ['0-9A-Fa-f', StrDupW(p, 0, 4)]);
      end;
    '/':
      begin
        Result := '/';
        Inc(p);
      end
  else
    begin
      Result := p^;
      Inc(p);
    end;
  end;
end;

procedure TQMacroManager.Clear;
var
  I: Integer;
begin
  for I := 0 to FMacroes.Count - 1 do
    FreeObject(FMacroes[I]);
  FMacroes.Clear;
  FSavePoint := 0;
end;

const
  QMERR_TAG_BEGIN_NEEDED = 1;
  QMERR_TAG_NOT_CLOSED = 2;
  QMERR_MACRO_NOT_FOUND = 3;

function TQMacroManager.Complie(AText: QStringW;
  AMacroStart, AMacroEnd: QStringW; const AFlags: Integer): TQMacroComplied;
begin
  Result := nil;
  InternalComplie(Result, AText, AMacroStart, AMacroEnd, AFlags);
end;

constructor TQMacroManager.Create;
begin
  inherited Create;
  FMacroes := TMacroList.Create;
end;

function TQMacroManager.DateTimeToStr(ADateTime: TDateTime): String;
var
  T: Integer;
begin
  T := Trunc(ADateTime);
  if T = 0 then
    Result := FormatDateTime('hh:nn:ss.zzz', ADateTime)
  else if IsZero(ADateTime - T) then
    Result := FormatDateTime('yyyy-mm-dd', ADateTime)
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADateTime);
end;

destructor TQMacroManager.Destroy;
begin
  Clear;
  FreeObject(FMacroes);
{$IFDEF UNICODE}
  if TMethod(OnMacroMissed).Data = Pointer(-1) then
    TQMacroMissEventA(TMethod(FOnMacroMissed).Code) := nil;
{$ENDIF}
  inherited;
end;

procedure TQMacroManager.DoFetchFieldQuotedValue(AMacro: TQMacroItem;
  const AQuoter: QCharW);
var
  AField: TField;
begin
  AField := TField(AMacro.Value.Tag);
  if AField <> nil then
  begin
    if AField.IsNull then
      AMacro.Value.Value := 'NULL'
    else if AField is TDateTimeField then
      AMacro.Value.Value := '''' + DateTimeToStr(AField.AsDateTime) + ''''
    else
    begin
      if BooleanAsInt and (AField is TBooleanField) then
      begin
        if AField.AsBoolean then
          AMacro.Value.Value := '''1'''
        else
          AMacro.Value.Value := '''0''';
      end
      else
        AMacro.Value.Value := QuotedStrW(AField.AsString, '''');
    end;
  end;
end;

procedure TQMacroManager.DoFetchFieldValue(AMacro: TQMacroItem;
  const AQuoter: QCharW);
var
  AField: TField;
begin
  AField := TField(AMacro.Value.Tag);
  if AField <> nil then
  begin
    if AField.IsNull then
      AMacro.Value.Value := 'NULL'
    else if AField is TDateTimeField then
      AMacro.Value.Value := DateTimeToStr(AField.AsDateTime)
    else
    begin
      if BooleanAsInt and (AField is TBooleanField) then
      begin
        if AField.AsBoolean then
          AMacro.Value.Value := '1'
        else
          AMacro.Value.Value := '0';
      end
      else
        AMacro.Value.Value := AField.AsString;
    end;
  end;
end;

procedure TQMacroManager.DoFetchValue(AMacro: TQMacroItem;
  const AQuoter: QCharW);
begin
  if TMethod(AMacro.Value.OnFetchValue).Data = nil then
    TQMacroValueFetchEventG(TMethod(AMacro.Value.OnFetchValue).Code)
      (AMacro, AQuoter)
{$IFDEF UNICODE}
  else if TMethod(AMacro.Value.OnFetchValue).Data = Pointer(-1) then
    TQMacroValueFetchEventA(TMethod(AMacro.Value.OnFetchValue).Code)
      (AMacro, AQuoter)
{$ENDIF}
  else
    AMacro.Value.OnFetchValue(AMacro, AQuoter);
end;

function TQMacroManager.DoMacroMissed(AName: QStringW;
  const AQuoter: QCharW): Boolean;
begin
  Result := False;
  if Assigned(FOnMacroMissed) then
  begin
    if TMethod(FOnMacroMissed).Data = nil then
      TQMacroMissEventG(TMethod(FOnMacroMissed).Code)
        (Self, AName, AQuoter, Result)
{$IFDEF UNICODE}
    else if IntPtr(TMethod(FOnMacroMissed).Data) = -1 then
      TQMacroMissEventA(TMethod(FOnMacroMissed).Code)
        (Self, AName, AQuoter, Result)
{$ENDIF}
    else
      FOnMacroMissed(Self, AName, AQuoter, Result);
  end;
end;

function TQMacroManager.Find(const AName: QStringW;
  var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FMacroes.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := StrCmpW(PQCharW(Items[I].Name), PQCharW(AName), IgnoreCase);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  AIndex := L;
end;

function TQMacroManager.GetCount: Integer;
begin
  Result := FMacroes.Count;
end;

function TQMacroManager.GetItems(AIndex: Integer): TQMacroItem;
begin
  Result := FMacroes[AIndex];
end;

function TQMacroManager.GetValues(AName: QStringW): QStringW;
begin
  if InternalMacroValue(AName, #0, Result) <> 0 then
    Result := '';
end;

function TQMacroManager.IncW(p: PQCharW; ALen: Integer): PQCharW;
begin
  Inc(p, ALen);
  Result := p;
end;

function TQMacroManager.IndexOf(const AName: QStringW): Integer;
begin
  if not Find(AName, Result) then
    Result := -1;
end;

procedure TQMacroManager.InternalComplie(var AResult: TQMacroComplied;
  AText, AMacroStart, AMacroEnd: QStringW; const AFlags: Integer);
var
  LS, LE, AIndex: Integer;
  prs, pts, p, ps, pms, pme, pl: PQCharW;
  AItem: PQMacroCompliedItem;
  AQuoter: QCharW;
  // AMacro: TQMacroItem;
  AName: QStringW;
  AIsNewResult: Boolean;
  ANameStarted: Boolean;
  function IsMacroEndChar: Boolean;
  begin
    if (AFlags and MRF_END_WITH_INVALID_CHAR) <> 0 then
      Result := not(((p^ >= '0') and (p^ <= '9')) or
        ((p^ >= 'A') and (p^ <= 'Z')) or ((p^ >= 'a') and (p^ <= 'z')) or
        (p^ > #$7F))
    else
      Result := StartWithW(p, pme, False);
  end;

  function MacroCharType(var ALen: Integer): TQMacroCharType;
  begin
    Result := mctChar;
    if ANameStarted then
    begin
      if Assigned(OnTestNameEnd) then
      begin
        if TMethod(OnTestNameEnd).Data = nil then
          TQMacroNameTestEventG(TMethod(OnTestNameEnd).Code)
            (Self, p, ALen, Result)
{$IFDEF UNICODE}
        else if TMethod(OnTestNameEnd).Data = Pointer(-1) then
          TQMacroNameTestEventA(TMethod(OnTestNameEnd).Code)
            (Self, p, ALen, Result)
{$ENDIF}
        else
          OnTestNameEnd(Self, p, ALen, Result);
      end
      else if IsMacroEndChar then
        Result := mctNameEnd;
    end
    else
    begin
      if Assigned(OnTestNameStart) then
        OnTestNameStart(Self, p, ALen, Result)
      else if CompareMem(p, pms, LS shl 1) then
        Result := mctNameStart;
    end;
    case Result of
      mctNameStart:
        ANameStarted := True;
      mctNameEnd:
        ANameStarted := False;
    end;
  end;

  procedure TestMacro;
  var
    pq: PQCharW;
    ALine, ACol: Integer;
    AParams: QStringW;
  begin
    if MacroCharType(LS) = mctNameStart then //
    begin
      if ps <> p then
      begin
        AItem := @AResult.Items[AResult.Count];
        AItem.Start := prs;
        Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
        AItem.Length := (IntPtr(p) - IntPtr(ps)) shr 1;
        AItem.Quoter := #0;
        AItem.Macro := nil;
        AItem.IsMacro := False;
        AItem.Text := Unescape(AItem.Start, AItem.Length);
        Inc(AResult.FCount);
        Inc(AResult.FMinSize, AItem.Length);
      end;
      pl := p;
      Inc(p, LS);
      ps := p;
      while (p^ <> #0) and (MacroCharType(LE) <> mctNameEnd) do
      begin
        if (p^ = '''') or (p^ = '"') then
        begin
          pq := p;
          Inc(p);
          while p^ <> #0 do
          begin
            if p^ = pq^ then
            begin
              Inc(p);
              Break;
            end
            else
              Inc(p);
          end;
        end
        else if (p^ = '\') and ((AFlags and MRF_ENABLE_ESCAPE) <> 0) then
        // ����ת��֧��
        begin
          Inc(p);
          if p^ <> #0 then
            Inc(p);
        end
        else
          Inc(p);
      end;
      if (p^ = #0) and ((AFlags and MRF_END_WITH_INVALID_CHAR)=0) then
        raise Exception.CreateFmt(STagNotClosed, [StrDupW(ps, 0, 20) + '...'])
      else if p = ps then
      begin
        ps := StrPosW(pts, p, ACol, ALine);
        AName := DecodeLineW(ps);
        raise Exception.CreateFmt(STagNameMissed, [ALine, ACol, AName]);
      end
      else
      begin
        AName := Unescape(ps, (IntPtr(p) - IntPtr(ps)) shr 1);
        if (AFlags and MRF_PARSE_PARAMS) <> 0 then
        begin
          AParams := ValueOfW(AName, '(');
          if EndWithW(AParams, ')', True) then // OK
          begin
            AName := NameOfW(AName, '(');
            SetLength(AParams, Length(AParams) - 1);
            AParams := '[' + AParams + ']'; // JSON����ģʽ
          end
          else
            SetLength(AParams, 0);
        end;
        if not Find(AName, AIndex) then
        begin
          if (AFlags and MRF_IGNORE_MISSED) <> 0 then
          begin
            ps := pl;
            Inc(p, LE);
            Exit;
          end
          else if not DoMacroMissed(AName, AQuoter) then
          begin
            if (AFlags and MRF_DELAY_BINDING) <> 0 then
              AIndex := -1
            else
              raise Exception.CreateFmt(SMacroNotFound, [AName]);
          end;
        end;
        AItem := @AResult.Items[AResult.Count];
        AItem.Start := prs;
        Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
        AItem.Length := Length(AName);
        AItem.IsMacro := True;
        AItem.Quoter := AQuoter;
        AItem.Text := AName;
        if Length(AParams) > 0 then
        begin
          AItem.Params := TQJson.Create;
          AItem.Params.TryParse(AParams);
        end;
        if AIndex <> -1 then
        begin
          AItem.Macro := Items[AIndex];
          if AItem.Macro.Value.Volatile = mvImmutable then
            Inc(AResult.FMinSize, Length(AItem.Macro.Value.Value))
          else
            Inc(AResult.FVolatiles);
        end
        else
        begin
          AItem.Macro := nil;
          Inc(AResult.FDelayCount);
          Inc(AResult.FVolatiles); // �����ӳٰ󶨵���Ŀ�޷�Ԥ��ȷ������
        end;
        Inc(AResult.FCount);
        Inc(p, LE);
        ps := p;
      end;
    end // End Macro Found
    else if p^ = '\' then
      CharUnescape(p)
    else
      Inc(p);
  end;
  procedure ParseInQuoter;
  begin
    AQuoter := p^;
    Inc(p);
    while p^ <> #0 do
    begin
      if p^ = AQuoter then
      begin
        Inc(p);
        AQuoter := #0;
        Break;
      end
      else
        TestMacro;
    end;
  end;
  procedure ParseWithBoundary;
  begin
    while p^ <> #0 do
    begin
      if (p^ = '''') then
      begin
        if (AFlags and MRF_IN_SINGLE_QUOTER) = 0 then
        begin
          Inc(p);
          while (p^ <> #0) and (p^ <> '''') do
            Inc(p);
          if p^ = '''' then
            Inc(p);
        end
        else
          ParseInQuoter;
      end
      else if p^ = '"' then
      begin
        if (AFlags and MRF_IN_DBL_QUOTER) = 0 then
        begin
          Inc(p);
          while (p^ <> #0) and (p^ <> '"') do
            Inc(p);
          if p^ = '"' then
            Inc(p);
        end
        else
          ParseInQuoter;
      end
      else // No Quoted
        TestMacro;
    end;
    if p <> ps then
    begin
      AItem := @AResult.Items[AResult.Count];
      AItem.Start := prs;
      Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
      AItem.Length := (IntPtr(p) - IntPtr(ps)) shr 1;
      AItem.Quoter := #0;
      AItem.Macro := nil;
      AItem.IsMacro := False;
      AItem.Text := Unescape(AItem.Start, AItem.Length);
      Inc(AResult.FCount);
      Inc(AResult.FMinSize, AItem.Length);
    end;
  end;
  function MacroStartWith: TQMacroItem;
  var
    L, H, I, C: Integer;
  begin
    L := 0;
    H := FMacroes.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      Result := Items[I];
      C := StrNCmpW(PQCharW(Result.Name), p, IgnoreCase, Length(Result.Name));
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
          Exit;
      end;
    end;
    Result := nil;
  end;

// û�зָ���������£�ֻ�ܲ��ö��ַ����ң���������
  procedure ParseWithNoBoundary;
  var
    AMacro: TQMacroItem;
  begin
    while p^ <> #0 do
    begin
      AMacro := MacroStartWith;
      if Assigned(AMacro) then
      begin
        if p <> ps then
        begin
          AItem := @AResult.Items[AResult.Count];
          AItem.Start := prs;
          Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
          AItem.Length := (IntPtr(p) - IntPtr(ps)) shr 1;
          AItem.Text := Unescape(AItem.Start, AItem.Length);
          AItem.Quoter := #0;
          AItem.Macro := nil;
          AItem.IsMacro := False;
          Inc(AResult.FCount);
          Inc(AResult.FMinSize, AItem.Length);
        end;
        AItem := @AResult.Items[AResult.Count];
        AItem.Start := prs;
        Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
        AItem.Length := Length(AMacro.Name);
        AItem.Macro := AMacro;
        AItem.IsMacro := True;
        AItem.Quoter := AQuoter;
        AItem.Text := AMacro.Name;
        if AMacro.Value.Volatile = mvImmutable then
          Inc(AResult.FMinSize, Length(AItem.Macro.Value.Value))
        else
          Inc(AResult.FVolatiles);
        Inc(AResult.FCount);
        Inc(p, Length(AMacro.Name));
        ps := p;
      end
      else
        Inc(p);
    end;
    if p <> ps then
    begin
      AItem := @AResult.Items[AResult.Count];
      AItem.Start := prs;
      Inc(AItem.Start, (IntPtr(ps) - IntPtr(pts)) shr 1);
      AItem.Length := (IntPtr(p) - IntPtr(ps)) shr 1;
      AItem.Quoter := #0;
      AItem.Macro := nil;
      AItem.IsMacro := False;
      AItem.Text := Unescape(AItem.Start, AItem.Length);
      Inc(AResult.FCount);
      Inc(AResult.FMinSize, AItem.Length);
    end;
  end;

begin
  LS := Length(AMacroStart);
  ANameStarted := False;
  if (AFlags and MRF_END_WITH_INVALID_CHAR) = 0 then
  begin
    LE := Length(AMacroEnd);
    if LE = 0 then
    begin
      AMacroEnd := AMacroStart;
      LE := LS;
    end;
  end
  else
    LE := 0;
  if LS = 0 then
  begin
    if LE <> 0 then
      raise Exception.Create(SMacroStartNeeded);
  end;
  if not Assigned(AResult) then
  begin
    AResult := TQMacroComplied.Create(Self);
    AResult.FText := AText;
    AResult.FMinSize := 0;
    AResult.FCount := 0;
    AResult.FVolatiles := 0;
    AIsNewResult := True;
  end
  else
    AIsNewResult := False;
  AResult.FPushId := FLastPushId;
  if Length(AText) = 0 then
    SetLength(AResult.FItems, 0)
  else
  begin
    try
      if IgnoreCase then
      begin
        AText := UpperCase(AText);
        AMacroStart := UpperCase(AMacroStart);
        AMacroEnd := UpperCase(AMacroEnd);
      end;
      SetLength(AResult.FItems, 4096);
      // ������Ҫ4096��
      ps := PQCharW(AText);
      p := ps;
      pts := ps;
      AQuoter := #0;
      prs := PQCharW(AResult.FText);
      if LS = 0 then
        ParseWithNoBoundary
      else
      begin
        pms := PQCharW(AMacroStart);
        pme := PQCharW(AMacroEnd);
        ParseWithBoundary;
      end;
    except
      if AIsNewResult then
        FreeObject(AResult);
      raise;
    end;
  end;
  AResult.FFlags := AFlags;
  SetLength(AResult.FItems, AResult.Count);
end;

function TQMacroManager.InternalMacroValue(const AName: QStringW;
  const AQuoter: QCharW; var AValue: QStringW): Integer;
var
  AIdx: Integer;
  AItem: TQMacroItem;
  function HandleMissed: Boolean;
  begin
    Result := False;
    if Assigned(FOnMacroMissed) then
      FOnMacroMissed(Self, AName, AQuoter, Result);
  end;

begin
  if Find(AName, AIdx) then
  begin
    AItem := Items[AIdx];
    if not Assigned(AItem.Value) then
      Result := 1
    else
    begin
      Result := 0;
      if AItem.Value.Volatile = mvImmutable then
        AValue := AItem.Value.Value
      else if Assigned(AItem.Value.OnFetchValue) then
      begin
        DoFetchValue(AItem, AQuoter);
        AValue := AItem.Value.Value;
      end
      else
        Result := 1;
    end
  end
  else
  begin
    if not HandleMissed then
      Result := 2
    else
      Result := InternalMacroValue(AName, AQuoter, AValue);
  end;
end;

procedure TQMacroManager.InternalPush(AMacro: TQMacroItem;
  const AValue: QStringW; AOnFetch: TQMacroValueFetchEvent;
  AStable: TQMacroVolatile; ATag: IntPtr);
var
  ALast: PQMacroValue;
begin
  New(ALast);
  ALast.Value := AValue;
  ALast.OnFetchValue := AOnFetch;
  ALast.Prior := AMacro.FValue;
  ALast.Next := nil;
  ALast.SavePoint := FSavePoint;
  ALast.Volatile := AStable;
  ALast.Tag := ATag;
  if Assigned(AMacro.FValue) then
    AMacro.FValue.Next := ALast;
  AMacro.FValue := ALast;
  if (AStable = mvImmutable) and Assigned(AOnFetch) then
    AOnFetch(AMacro, #0);
  // �̶���ֵ�ǳ�����ʼ�ղ���
  Inc(FLastPushId);
end;

function TQMacroManager.MacroValue(const AName: QStringW): QStringW;
begin
  Result := MacroValue(AName, QCharW(#0));
end;

function TQMacroManager.MacroValue(const AName: QStringW; const AQuoter: QCharW)
  : QStringW;
begin
  case InternalMacroValue(AName, AQuoter, Result) of
    1: //
      raise Exception.CreateFmt(SMacroValueUnknown, [AName]);
    2:
      raise Exception.CreateFmt(SMacroNotFound, [AName]);
  end;
end;

procedure TQMacroManager.Pop(const AName: QStringW);
var
  AIndex: Integer;
  AItem: TQMacroItem;
  AValue: PQMacroValue;
begin
  if Find(AName, AIndex) then
  begin
    AItem := Items[AIndex];
    if Assigned(AItem.Value.Prior) then
    begin
      AValue := AItem.Value;
      case AValue.Volatile of
        mvStable:
          Dec(FStableCount);
        mvVolatile:
          Dec(FVolatileCount);
      end;
      AItem.FValue := AItem.Value.Prior;
      Dispose(AValue);
      if AItem.FValue = nil then
        FMacroes.Delete(AIndex);
    end;
    Inc(FLastPushId);
  end;
end;

procedure TQMacroManager.Push(const AName: QStringW;
  AOnFetchValue: TQMacroValueFetchEvent; AVolatile: TQMacroVolatile;
  ATag: IntPtr);
var
  AIndex: Integer;
  AItem: TQMacroItem;
begin
  if not Find(AName, AIndex) then
  begin
    AItem := TQMacroItem.Create(Self);
    AItem.FName := AName;
    InternalPush(AItem, '', AOnFetchValue, AVolatile, ATag);
    FMacroes.Insert(AIndex, AItem);
  end
  else
    InternalPush(Items[AIndex], '', AOnFetchValue, AVolatile, ATag);
  Inc(FLastPushId);
end;

procedure TQMacroManager.Push(const AName, AValue: QStringW);
var
  AIndex: Integer;
  AItem: TQMacroItem;
begin
  if not Find(AName, AIndex) then
  begin
    AItem := TQMacroItem.Create(Self);
    AItem.FName := AName;
    InternalPush(AItem, AValue, nil, mvImmutable, 0);
    FMacroes.Insert(AIndex, AItem);
  end
  else
    InternalPush(Items[AIndex], AValue, nil, mvImmutable, 0);
end;

function TQMacroManager.Replace(const AText: QStringW;
  AMacroStart, AMacroEnd: QStringW; const AFlags: Integer): QStringW;
var
  ATemp: TQMacroComplied;
begin
  ATemp := Complie(AText, AMacroStart, AMacroEnd, AFlags);
  try
    Result := Replace(ATemp);
  finally
    FreeObject(ATemp);
  end;
end;

function TQMacroManager.Replace(AHandle: TQMacroComplied): QStringW;
  procedure SimpleReplace;
  var
    I, L: Integer;
    pd: PQCharW;
    AItem: PQMacroCompliedItem;
  begin
    if Length(AHandle.FReplacedText) <> 0 then
      Result := AHandle.FReplacedText
    else
    begin
      SetLength(Result, AHandle.MinSize);
      I := 0;
      pd := PQCharW(Result);
      while I < AHandle.Count do
      begin
        AItem := @AHandle.Items[I];
        if AItem.Macro = nil then
        begin
          L := Length(AItem.Text);
          Move(PQCharW(AItem.Text)^, pd^, L shl 1);
          Inc(pd, L);
        end
        else
        begin
          L := Length(AItem.Macro.Value.Value);
          Move(PQCharW(AItem.Macro.Value.Value)^, pd^, L shl 1);
          Inc(pd, L);
        end;
        Inc(I);
      end;
      AHandle.FReplacedText := Result;
    end;
  end;

  procedure LowReplace;
  var
    I, L, AOffset, ASize: Integer;
    pd: PQCharW;
    AItem: PQMacroCompliedItem;
  begin
    L := AHandle.MinSize shl 1;
    if L < 4096 then
      L := 4096;
    SetLength(Result, L);
    I := 0;
    AOffset := 0;
    pd := PQCharW(Result);
    while I < AHandle.Count do
    begin
      AItem := @AHandle.Items[I];
      if AItem.IsMacro then
      begin
        case AItem.Macro.Value.Volatile of
          mvStable:
            begin
              if AItem.Macro.Value.ReplaceId <> FReplaceId then
              begin
                DoFetchValue(AItem.Macro, AItem.Quoter);
                AItem.Macro.Value.ReplaceId := FReplaceId;
              end;
            end;
          mvVolatile:
            begin
              AItem.Macro.FParams := AItem.Params;
              DoFetchValue(AItem.Macro, AItem.Quoter);
            end;
        end;
        ASize := Length(AItem.Macro.Value.Value);
        if AOffset + ASize > L then
        begin
          Inc(L, ((ASize div 4096) + 1) * 4096);
          SetLength(Result, L);
          pd := PQCharW(Result);
          Inc(pd, AOffset);
        end;
        Move(PQCharW(AItem.Macro.Value.Value)^, pd^, ASize shl 1);
        Inc(AOffset, ASize);
        Inc(pd, ASize);
      end
      else
      begin
        ASize := Length(AItem.Text);
        if AOffset + ASize > L then
        begin
          Inc(L, ((ASize div 4096) + 1) * 4096);
          SetLength(Result, L);
          pd := PQCharW(Result);
          Inc(pd, AOffset);
        end;
        Move(PQCharW(AItem.Text)^, pd^, ASize shl 1);
        Inc(AOffset, ASize);
        Inc(pd, ASize);
      end;
      Inc(I);
    end;
    SetLength(Result, AOffset);
  end;

begin
  Inc(FReplaceId);
  if AHandle.Count = 0 then
    SetLength(Result, 0)
  else
  begin
    if (AHandle.FPushId <> FLastPushId) or (AHandle.FDelayCount > 0) then
    begin
      AHandle.CheckMacros;
      AHandle.FPushId := FLastPushId;
    end;
    if AHandle.Volatiles = 0 then
      SimpleReplace
    else
      LowReplace;
  end;
end;

procedure TQMacroManager.Restore(ASavePoint: Integer);
var
  I, APopCount: Integer;
  AMacro: TQMacroItem;
  AValue: PQMacroValue;
begin
  if ASavePoint >= 0 then
  begin
    I := 0;
    APopCount := 0;
    while I < FMacroes.Count do
    begin
      AMacro := Items[I];
      while (AMacro.Value <> nil) and (AMacro.Value.SavePoint > ASavePoint) do
      begin
        AValue := AMacro.Value.Prior;
        Dispose(AMacro.Value);
        AMacro.FValue := AValue;
        Inc(APopCount);
      end;
      if AMacro.FValue = nil then // ȫ����ջ��
        FMacroes.Delete(I)
      else
        Inc(I);
    end;
    FSavePoint := ASavePoint;
    if APopCount > 0 then
      Inc(FLastPushId);
  end;
end;

function TQMacroManager.SavePoint: Integer;
begin
  Result := FSavePoint;
  Inc(FSavePoint);
end;

procedure TQMacroManager.SetOnMacroMissed(AHandler: TQMacroMissEvent);
begin
  if not EventEqual(TMethod(AHandler), TMethod(FOnMacroMissed)) then
  begin
{$IFDEF UNICODE}
    if TMethod(FOnMacroMissed).Data = Pointer(-1) then
      TQMacroMissEventA(TMethod(FOnMacroMissed).Code) := nil;
{$ENDIF}
    FOnMacroMissed := AHandler;
  end;
end;

procedure TQMacroManager.SetOnTestNameEnd(const Value: TQMacroNameTestEvent);
begin
  if not EventEqual(TMethod(Value), TMethod(FOnTestNameEnd)) then
  begin
{$IFDEF UNICODE}
    if TMethod(FOnTestNameEnd).Data = Pointer(-1) then
      TQMacroNameTestEventA(TMethod(FOnTestNameEnd).Code) := nil;
{$ENDIF}
    FOnTestNameEnd := Value;
  end;
end;

procedure TQMacroManager.SetOnTestNameStart(const Value: TQMacroNameTestEvent);
begin
  if not EventEqual(TMethod(Value), TMethod(FOnTestNameStart)) then
  begin
{$IFDEF UNICODE}
    if TMethod(FOnTestNameStart).Data = Pointer(-1) then
      TQMacroNameTestEventA(TMethod(FOnTestNameStart).Code) := nil;
{$ENDIF}
    FOnTestNameStart := Value;
  end;
end;

procedure TQMacroManager.Pop(ADataSet: TDataSet; const ANameSpace: QStringW);
var
  I: Integer;
begin
  if Length(ANameSpace) = 0 then
  begin
    for I := 0 to ADataSet.FieldCount - 1 do
      Pop(ADataSet.Fields[I].FieldName);
  end
  else
  begin
    for I := 0 to ADataSet.FieldCount - 1 do
      Pop(ANameSpace + '.' + ADataSet.Fields[I].FieldName);
  end;
end;

procedure TQMacroManager.Push(ADataSet: TDataSet; const ANameSpace: QStringW);
var
  I: Integer;
  AField: TField;
begin
  if Length(ANameSpace) = 0 then
  begin
    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      AField := ADataSet.Fields[I];
      Push(AField.FieldName, DoFetchFieldValue, mvVolatile, IntPtr(AField));
      Push(AField.FieldName + '.Quoted', DoFetchFieldQuotedValue, mvVolatile,
        IntPtr(AField));
    end;
  end
  else
  begin
    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      AField := ADataSet.Fields[I];
      Push(ANameSpace + '.' + AField.FieldName, DoFetchFieldValue, mvVolatile,
        IntPtr(AField));
      Push(ANameSpace + AField.FieldName + '.Quoted', DoFetchFieldQuotedValue,
        mvVolatile, IntPtr(AField));
    end;
  end;
end;

procedure TQMacroManager.Push(const AName: QStringW;
  AOnFetchValue: TQMacroValueFetchEventG; AVolatile: TQMacroVolatile;
  ATag: IntPtr);
var
  AHandler: TQMacroValueFetchEvent;
begin
  if Assigned(AOnFetchValue) then
  begin
    TMethod(AHandler).Code := @AOnFetchValue;
    TMethod(AHandler).Data := nil;
    Push(AName, AHandler, AVolatile, ATag);
  end
  else
    Push(AName, nil, AVolatile, ATag);
end;
{$IFDEF UNICODE}

procedure TQMacroManager.Push(const AName: QStringW;
  AOnFetchValue: TQMacroValueFetchEventA; AVolatile: TQMacroVolatile;
  ATag: IntPtr);
var
  AHandler: TQMacroValueFetchEvent;
begin
  if Assigned(AOnFetchValue) then
  begin
    AHandler := nil;
    TQMacroValueFetchEventA(TMethod(AHandler).Code) := AOnFetchValue;
    TMethod(AHandler).Data := Pointer(-1);
    Push(AName, AHandler, AVolatile, ATag);
  end
  else
    Push(AName, nil, AVolatile, ATag);
end;

procedure TQMacroManager.SetMacroMissed(AHandler: TQMacroMissEventA);
var
  AEvent: TQMacroMissEvent;
begin
  TMethod(AEvent).Data := Pointer(-1);
  TMethod(AEvent).Code := nil;
  TQMacroMissEventA(TMethod(AEvent).Code) := AHandler;
  SetOnMacroMissed(AEvent);
end;
{$ENDIF}

procedure TQMacroManager.SetMacroMissed(AHandler: TQMacroMissEventG);
var
  AEvent: TQMacroMissEvent;
begin
  TMethod(AEvent).Data := nil;
  TQMacroMissEventG(TMethod(AEvent).Code) := AHandler;
  SetOnMacroMissed(AEvent);
end;
{$IFDEF UNICODE}

procedure TQMacroManager.SetTestNameEnd(const Value: TQMacroNameTestEventA);
var
  AEvent: TQMacroNameTestEvent;
begin
  TMethod(AEvent).Data := Pointer(-1);
  TMethod(AEvent).Code := nil;
  TQMacroNameTestEventA(TMethod(AEvent).Code) := Value;
  SetOnTestNameEnd(AEvent);
end;
{$ENDIF}

procedure TQMacroManager.SetTestNameEnd(const Value: TQMacroNameTestEventG);
var
  AEvent: TQMacroNameTestEvent;
begin
  TMethod(AEvent).Data := Pointer(-1);
  TQMacroNameTestEventG(TMethod(AEvent).Code) := Value;
  SetOnTestNameEnd(AEvent);
end;

procedure TQMacroManager.SetTestNameStart(const Value: TQMacroNameTestEventG);
var
  AEvent: TQMacroNameTestEvent;
begin
  TMethod(AEvent).Data := Pointer(-1);
  TQMacroNameTestEventG(TMethod(AEvent).Code) := Value;
  SetOnTestNameStart(AEvent);
end;
{$IFDEF UNICODE}

procedure TQMacroManager.SetTestNameStart(const Value: TQMacroNameTestEventA);
var
  AEvent: TQMacroNameTestEvent;
begin
  TMethod(AEvent).Data := Pointer(-1);
  TMethod(AEvent).Code := nil;
  TQMacroNameTestEventA(TMethod(AEvent).Code) := Value;
  SetOnTestNameStart(AEvent);
end;

{$ENDIF}


function TQMacroManager.Unescape(p: PQCharW; ALen: Integer): QStringW;
var
  pe, pd: PQCharW;
begin
  SetLength(Result, ALen);
  if ALen > 0 then
  begin
    pd := PQCharW(Result);
    pe := p;
    Inc(pe, ALen);
    while p < pe do
    begin
      pd^ := CharUnescape(p);
      Inc(pd);
    end;
    SetLength(Result, (IntPtr(pd) - IntPtr(PQCharW(Result))) shr 1);
  end;
end;

function TQMacroManager.Unescape(const S: QStringW): QStringW;
begin
  Result := Unescape(PQCharW(S), Length(S));
end;
{ TQMacroItem }

constructor TQMacroItem.Create(AOwner: TQMacroManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TQMacroItem.Destroy;
var
  AValue: PQMacroValue;
begin
  while FValue <> nil do
  begin
    AValue := FValue;
    FValue := FValue.Prior;
{$IFDEF UNICODE}
    if TMethod(AValue.OnFetchValue).Data = Pointer(-1) then
      TQMacroValueFetchEventA(TMethod(AValue.OnFetchValue).Code) := nil;
{$ENDIF}
    Dispose(AValue);
  end;
  inherited;
end;

{ TQMacroComplied }

procedure TQMacroComplied.CheckMacros;
var
  I: Integer;
  AItem: PQMacroCompliedItem;
begin
  SetLength(FReplacedText, 0);
  I := 0;
  FVolatiles := 0;
  FMinSize := 0;
  while I < FCount do
  begin
    AItem := @FItems[I];
    if AItem.IsMacro then
    begin
      if not Assigned(AItem.Macro) then
        Dec(FDelayCount);
      AItem.Macro := MacroNeeded(AItem.Text);
      if AItem.Macro.Value.Volatile <> mvImmutable then
        Inc(FVolatiles)
      else
        Inc(FMinSize, Length(AItem.Macro.Value.Value));
    end
    else
      Inc(FMinSize, AItem.Length);
    Inc(I);
  end;
end;

function TQMacroComplied.Complie(AText, AMacroStart, AMacroEnd: QStringW;
  const AFlags: Integer): Boolean;
var
  AResult: TQMacroComplied;
begin
  try
    AResult := Self;
    FOwner.InternalComplie(AResult, AText, AMacroStart, AMacroEnd, AFlags);
    Result := True;
  except
    Result := False;
  end;
end;

constructor TQMacroComplied.Create(AOwner: TQMacroManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TQMacroComplied.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FItems) do
  begin
    if Assigned(Items[I].Params) then
      FreeAndNil(Items[I].Params);
  end;
  inherited;
end;

function TQMacroComplied.EnumUsedMacros(AList: TStrings): Integer;
var
  T: TStringList;
  I, J: Integer;
  S: String;
begin
  T := TStringList.Create;
  try
    T.Sorted := True;
    for I := 0 to High(Items) do
    begin
      if Items[I].IsMacro then
      begin
        if Items[I].Macro <> nil then
          S := Items[I].Macro.Name
        else
          S := Items[I].Text;
        if not T.Find(S, J) then
          T.Add(S);
      end;
    end;
    AList.AddStrings(T);
    Result := T.Count;
  finally
    FreeObject(T);
  end;
end;

procedure TQMacroComplied.LoadFromFile(const AFileName: QStringW);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    LoadFromStream(AStream);
  finally
    FreeObject(AStream);
  end;
end;

procedure TQMacroComplied.LoadFromStream(AStream: TStream);
var
  AHeader: TQMacroCompliedFileHeader;
  AItemHeader: TQMacoCompliedFileItem;
  I: Integer;
  ps: PQCharW;
  AItem: PQMacroCompliedItem;
begin
  AStream.ReadBuffer(AHeader, SizeOf(AHeader));
  if AHeader.Flags = MFF_FILETYPE then
  begin
    SetLength(FText, AHeader.TextLength);
    AStream.ReadBuffer(PQCharW(FText)^, AHeader.TextLength shl 1);
    FMinSize := AHeader.MinSize;
    FCount := AHeader.Count;
    FVolatiles := AHeader.Volatiles;
    SetLength(FReplacedText, 0);
    ps := PQCharW(FText);
    SetLength(FItems, FCount);
    for I := 0 to FCount - 1 do
    begin
      AStream.ReadBuffer(AItemHeader, SizeOf(AItemHeader));
      AItem := @FItems[I];
      AItem^.Start := ps + AItemHeader.Start;
      AItem^.Length := AItemHeader.Length;
      AItem^.Quoter := AItemHeader.Quoter;
      AItem^.IsMacro := AItemHeader.IsMacro;
      AItem^.Text := FOwner.Unescape(AItem^.Start, AItem^.Length);
      if AItemHeader.IsMacro then
        FItems[I].Macro := MacroNeeded(AItem^.Text);
    end;
    FPushId := FOwner.FLastPushId;
  end
  else
    raise Exception.Create(SBadFileFormat);
end;

function TQMacroComplied.MacroNeeded(const AName: QStringW): TQMacroItem;
var
  AIndex: Integer;
begin
  if FOwner.Find(AName, AIndex) then
    Result := FOwner.Items[AIndex]
  else
    raise Exception.CreateFmt(SMacroNotFound, [AName]);
end;

function TQMacroComplied.Replace: QStringW;
begin
  Result := FOwner.Replace(Self);
end;

procedure TQMacroComplied.SaveToFile(const AFileName: QStringW);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    AStream.SaveToFile(AFileName);
  finally
    FreeObject(AStream);
  end;
end;

procedure TQMacroComplied.SaveToStream(AStream: TStream);
var
  AHeader: TQMacroCompliedFileHeader;
  AItemHeader: TQMacoCompliedFileItem;
  I: Integer;
  ps: PQCharW;
begin
  AHeader.Flags := MFF_FILETYPE;
  AHeader.Version := 1;
  AHeader.Count := FCount;
  AHeader.MinSize := FMinSize;
  AHeader.Volatiles := FVolatiles;
  AHeader.TextLength := Length(FText);
  AStream.WriteBuffer(AHeader, SizeOf(AHeader));
  AStream.WriteBuffer(PQCharW(FText)^, AHeader.TextLength shl 1);
  ps := PQCharW(FText);
  for I := 0 to FCount - 1 do
  begin
    AItemHeader.Start := FItems[I].Start - ps;
    AItemHeader.Length := FItems[I].Length;
    AItemHeader.Quoter := FItems[I].Quoter;
    AItemHeader.IsMacro := FItems[I].IsMacro;
    AStream.WriteBuffer(AItemHeader, SizeOf(AItemHeader));
  end;
end;

end.

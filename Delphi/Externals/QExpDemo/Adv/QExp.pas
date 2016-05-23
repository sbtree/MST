unit QExp;

interface
//要单独使用本单元，注释掉下面的包含语句
//{$I qdac.inc}
//要在独立使用时，启用正则表达式支持，去掉行前的注释
{$WARN SYMBOL_DEPRECATED OFF}
//{$DEFINE QEXP_REGEX}
uses classes,types,windows,sysutils,fmtbcd,math,dialogs,syncobjs
  {$IFDEF QEXP_REGEX}//是否启用正则表达式函数支持
  ,perlregex
  {$ENDIF};
(*
QDAC表达式计算实现 (c)2013,QDAC swish

[版权声明]
  本单元隶属于QDAC项目组，您可以自由引用本单元实现，但必需加入相关版权声明。
本单元实现的目的不是提供一个全功能的解释器，而是为QDAC项目提供复杂表达式的解析
和执行支持。

2013.7.18
  * 修正了发现的内存泄露
  * 修正了DisplayName属性读取可能为空的问题
  
2013.7.17

  + 字符串中转义字符支持，按C++的转义字符规则解释
  + 内置函数增加Char
  * 修改了Bytes.FromStr的参数，增加了一个是否增加BOM的选项，以便保存正确的编码到文件
  * 其它小修订

2013.7.15
  + 加入Bytes.ByteSize函数，同时TQExprParser.Add改名为AddVar
  + 加入DecodeToken函数
  + 支持行注释符//和块注释符/**/
  * TQFunctions.RegisterFunction的第一个参数改为TQVar，以便支持对象化的调用方式，
    如A.Find(...)，这种函数的实现要取其所隶属的对象，访问其Parent属性
2013.7.13
  + 加入Bytes.FromHex/Bytes.Insert/Bytes.Append/Bytes/Bytes.SaveToFile/
    Bytes.LoadFromFile/Bytes.SetByte/Bytes.GetByte函数实现
  + 整合音儿小白(Yangxd)的部分修订和思路，感谢音儿小白

2013.7.12
  + 加入Eval函数，可以直接将字符串做为脚本来执行，该字符串可以调用外部的函数
  + 加入BeforeExecStatement和AfterExecStatement事件，以便在某个步骤执行完成时进行
    处理（可用于单步）
  + 加入BeforeExecute和AfterExecute事件，分别在执行前和执行结束时触发

2013.7.11
  + 支持while/break函数
  + 支持语句块，格式为
    {
    语句1;
    ...
    语句n;
    }
  + 支持用户自定义函数，格式为
    function 函数名(参数列表)
    {
    实现代码
    }
  * 修正了自增或自减时没有正确设置返回值，造成表达式的计算结果可能为空的问题
2013.6.13
  * 修改加入全局的QExpGlobal来管理全局函数，避免不必要的重复注册
==============================================
  本实现支持以下功能：
  1、一到多条语句的计算执行;
  2、支持运算符的优先级处理；
  3、支持在表达式中使用函数；
  4、支持用户自定义变量
  5、支持复合语句的执行
  本实现默认包含了以下函数的实现：
  (1)、日期时间
  DateAdd、DateDiff、DatePart、Now、Date、Time、MonthDays、IsLeapYear、EncodeDate、
  EncodeTime、Today、Yesterday、Tomorrow、IsToday、DateStart、DateEnd、DateReplace
  (2)、统计函数
  Avg、Sum、Max、Min、Count、StdDev、Round（四舍五入）、BankRound（银行家算法）
  (3)、字符串函数
  Left、Right、UpperCase、LowerCase、StrCmp、QuotedStr、DequoteStr、Pos
  Length,SubString,Like,StartWith,EndWith,RegexMatch,RegexMatchNext,RegexLastOffset,
  RegexLastLength,RegexSplit,RegexSplitCount,RegexSplitText,RegexSplitOffset,RegexReplace
  (4)、辅助函数
  MessageBox、IfThen,Goto,Exit,While,Break
  要实现用户自定义函数，可以创建对应函数的实现，然后注册到TQExprParser即可使用。
具体参考代码参数TDateFunctions等实现的Register函数.
注意：
  1、在QExp的实现中，没有所谓的常量，常量实际上也被定义为变量，而且目前版本没有限制可以更改值；
  2、在QExp的实现中，当全局变量和局部变量重名时，会优先组合局部变量
  3、在QExp的实现中，如果没有为Result全局变量赋值，则会使用最后一个语句的执行结果做为Result的值；
  4、变量(TQVar)默认不是善变的(Mutable=False)，如果变量的值在每次引用时必需改变，设置该属性为True，
  则每次引用相应的变量实例时，其值都会重新计算
  5、变量或函数名是不区分大小写的，因此Var1和VAR1是等价的。
  6、默认实现的表达式操作符是按照C规则，如果要使用Pascal风格的表达式，使用TPasParser替代TExprParser.
  而在使用TPasParser时，Delphi不支持的操作，可以直接调用相应的操作符处理函数来执行，具体参考
  TOperatorFunctions的实现
*)
{$M+}
const
  //数据类型
  VARTYPE_NULL    = $00000000;//空类型
  VARTYPE_STRING  = $00000001;//字符串
  VARTYPE_BYTES   = $00000002;//二进制串
  VARTYPE_FUNCTION= $10000001;//函数
  VARTYPE_VARNAME = $10000002;//变量，可以认为是没有参数的函数
  VARTYPE_NUMERIC_MASK  = $20000000;//数字
  VARTYPE_INTEGER_MASK  = $01000000;//整数类型
  VARTYPE_HEX_MASK      = $02000000;//十六进制
  VARTYPE_FLOAT_MASK    = $04000000;//浮点类型
  VARTYPE_BCD_MASK      = $08000000;//BCD掩码
  VARTYPE_DATETIME_MASK = $00100000;//日期时间类型掩码
  VARTYPE_BOOLEAN = $21000000;//布尔
  VARTYPE_INTEGER = $21000001;//整数
  VARTYPE_INT64   = $21000002;//64位整数
  VARTYPE_CHEX    = $23000001;//C格式的十六进制
  VARTYPE_DHEX    = $23000002;//Pascal格式的十六进制
  VARTYPE_FLOAT   = $24000001;//浮点
  VARTYPE_DATE    = $24100002;//日期
  VARTYPE_TIME    = $24100003;//时间
  VARTYPE_DATETIME= $24100005;//日期时间
  VARTYPE_NUMERIC = $28000009;//数值
type
  TQExprParser=class;
  TQFunction=class;
  TQFunctions=class;
  TQVar=class;
  TQExprStatement=class;
  TQExprParserClass=class of TQExprParser;
  TQVarClass=class of TQVar;
  EParserError=class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(ACode:Integer;AMsg:String);overload;
    property ErrorCode:Integer read FErrorCode;
  end;
  //TQValue负责存贮具体的值
  TQValue=class
  private
    function GetAsBcd: TBcd;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: WideString;
    procedure SetAsBcd(const Value: TBcd);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: WideString);
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);
    procedure DoBeforeChange;
    procedure DoChange;
    function GetAsReference: TQVar;
    procedure SetAsReference(const Value: TQVar);
    function GetDataType: Integer;
    function GetIsReference: Boolean;
    function GetIsFunction: Boolean;
    function GetIsDateTime: Boolean;
    function GetIsFloat: Boolean;
    function GetIsInteger: Boolean;
    function GetIsNumeric: Boolean;
    function GetIsBoolean: Boolean;
    function GetText: WideString;
    function GetIsString: Boolean;
    function GetIsVar: Boolean;
    function GetAsBytes: PByte;
    function GetByteSize: Integer;
    function GetIsBytes: Boolean;
    function GetIsNull: Boolean;
  protected
    FDataType:Integer;//数据类型
    FValue:AnsiString;//值内容
    FBeforeChange: TNotifyEvent;//值发生变更前通知事件
    FOnChange:TNotifyEvent;//值发生变更时的通知事件
    function GetTargetValue(AVal:TQValue):TQValue;//在引用时，获取引用最终对应的值
  published
  public
    constructor Create;overload;//构造函数
    procedure Assign(const ASource:TQValue);virtual;//拷贝函数
    procedure Math_Add(const V2,RetVal:TQValue);virtual;//加法实现
    procedure Math_Sub(const V2,RetVal:TQValue);virtual;//减法实现
    procedure Math_Multiply(const V2,RetVal:TQValue);virtual;//乘法实现
    procedure Math_Div(const V2,RetVal:TQValue);virtual;//除法实现
    procedure Math_DivTrunc(const V2,RetVal:TQValue);virtual;//整除实现
    procedure Math_Mod(const V2,RetVal:TQValue);virtual;//求余实现
    procedure Math_Power(const V2,RetVal:TQValue);virtual;//幂实现
    procedure ShiftLeft(const V2,RetVal:TQValue);virtual;//左移位实现
    procedure ShiftRight(const V2,RetVal:TQValue);virtual;//右移位实现
    procedure Bit_And(const V2,RetVal:TQValue);virtual;//位与实现
    procedure Bit_Or(const V2,RetVal:TQValue);virtual;//位或实现
    procedure Bit_Not(RetVal:TQValue);virtual;//位非实现
    procedure Bit_Xor(const V2,RetVal:TQValue);virtual;//位异或实现
    function Logical_And(const V2:TQValue):Boolean;virtual;//逻辑与实现
    function Logical_Or(const V2:TQValue):Boolean;virtual;//逻辑或实现
    function Logical_Not:Boolean;virtual;//逻辑非实现
    function Compare_LT(const V2:TQValue):Boolean;virtual;//小于实现
    function Compare_GT(const V2:TQValue):Boolean;virtual;//大于实现
    function Compare_EQ(const V2:TQValue):Boolean;virtual;//等于实现
    function Compare_LE(const V2:TQValue):Boolean;virtual;//小于等于实现
    function Compare_GE(const V2:TQValue):Boolean;virtual;//大于等于实现

    procedure Clear;//清除当前值，字段类型将变是VARTYPE_NULL
    procedure SetAsBytes(ABytes:PByte;ALength:Integer);
    property AsBytes:PByte read GetAsBytes;
    property ByteSize:Integer read GetByteSize;
    property AsString:WideString read GetAsString write SetAsString;//做为字符串访问
    property AsBoolean:Boolean read GetAsBoolean write SetAsBoolean;//做为布尔值访问
    property AsInteger:Integer read GetAsInteger write SetAsInteger;//做为整形值访问
    property AsInt64:Int64 read GetAsInt64 write SetAsInt64;//做为64位整数访问
    property AsBcd:TBcd read GetAsBcd write SetAsBcd;//做为BCD类型访问
    property AsFloat:Double read GetAsFloat write SetAsFloat;//做为浮点数访问
    property AsDateTime:TDateTime read GetAsDateTime write SetAsDateTime;//做为日期时间类型访问
    property AsReference:TQVar read GetAsReference write SetAsReference;//做为引用访问，引用实例必需保持有效
    property DataType:Integer read GetDataType;//实际数据类型
    property IsReference:Boolean read GetIsReference;//是否是引用
    property IsFunction:Boolean read GetIsFunction;//是否是函数
    property IsInteger:Boolean read GetIsInteger;//是否是整数（布尔、整数、64位整数）
    property IsNumeric:Boolean read GetIsNumeric;//是否是数值类型
    property IsFloat:Boolean read GetIsFloat;//是否是浮点数
    property IsBoolean:Boolean read GetIsBoolean;//判断是否为布尔值
    property IsNull:Boolean read GetIsNull;//是否为空
    property IsDatetime:Boolean read GetIsDateTime;//是否为日期时间类型
    property IsString:Boolean read GetIsString;//判断是否是字符串
    property IsVar:Boolean read GetIsVar;//判断是否是变量
    property IsBytes:Boolean read GetIsBytes;//判断是否是二进制
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property BeforeChange:TNotifyEvent read FBeforeChange write FBeforeChange;
    property Text:WideString read GetText;//值定义
  end;

  PQVar=^TQVar;
  TQVarEnumProc=procedure (ASender,AChild:TQVar;AParam:Integer) of object;
  TQVarEnumOption=(veoVariant,veoFunction,veoTemp,veoNest);
  TQVarEnumOptions=set of TQVarEnumOption;
  //变量定义，常量使用同样的定义，理论上常量应不允许修改，暂未实现
  TQVar=class
  protected
    FData: Pointer;
    FName,FDisplayName:WideString;
    FParent:TQVar;
    FItems:TList;
    FValue:TQValue;
    FReadOnly: Boolean;
    FOnGetValue:TNotifyEvent;
    FOnValueChange:TNotifyEvent;
    FOnFetch:Boolean;
    FFetchIP:Cardinal;
    FOwner:TQExprParser;
    FMutable: Boolean;
    function GetValue:TQValue;virtual;
    procedure DoValueChanging(ASender:TObject);virtual;
    procedure DoValueChange(ASender:TObject);virtual;
    function InternalFind(const AName:WideString;var AIndex:Integer):Boolean;virtual;
    function GetCount: Integer;virtual;
    function GetItems(AIndex:Integer): TQVar;virtual;
    function GetPath: WideString;
    procedure SetName(Value: WideString);
    function GetIndex: Integer;
    procedure ValidateName(S:WideString);
    function GetText: WideString;
    function GetDisplayName: WideString;
    procedure SetDisplayName(const Value: WideString);
    function GetParser: TQExprParser;
    constructor Create;overload;//构造函数
  published
  public
    constructor Create(AOwner:TQExprParser);overload;virtual;//构造函数
    destructor Destroy;override;//析构函数
    function Add(AName:WideString;AMutable:Boolean=False):TQVar;overload;virtual;//添加一个普通的变量
    function AddFunction(AName: WideString; AHandler: TNotifyEvent;
      AFixedParams: WideString; AVarParams: Boolean): TQFunction;//添加一个函数
    function ForcePath(APath:WideString):TQVar;
    function ForEach(AEnumProc:TQVarEnumProc;AOptions:TQVarEnumOptions;AParam:Integer):Integer;
    function Enum(AList:TList;AOptions:TQVarEnumOptions):Integer;
    procedure Add(AVar:TQVar);overload;virtual;//添加一个用户定义的变量类型
    procedure Assign(ASource:TQVar);virtual;//值拷贝实现
    procedure Delete(AIndex:Integer;AFreeNeeded:Boolean=true);overload;virtual;//删除子变量
    procedure Delete(AName:WideString;AFreeNeeded:Boolean=true);overload;//删除子变量
    procedure Clear(AFreeNeeded:Boolean=true);virtual;//清除子变量
    function Copy(AOwner:TQExprParser):TQVar;virtual;//创建对象拷贝
    function Find(AName:WideString):TQVar;//查找
    function FindByPath(APath:WideString):TQVar;//按路径查找
    function IndexOf(AName:WideString):Integer;overload;//索引
    function IndexOf(const AVar:TQVar):Integer;overload;//索引
    property Name:WideString read FName write SetName;//名称，不能包含成员分隔符.
    property DisplayName:WideString read GetDisplayName write SetDisplayName;//显示名称
    property Path:WideString read GetPath;//路径
    property Parent:TQVar read FParent;//父
    property Count:Integer read GetCount;//子变量数
    property Items[AIndex:Integer]:TQVar read GetItems;default;//数组
    property Value:TQValue read GetValue;//值
    property OnValueChange:TNotifyEvent read FOnValueChange write FOnValueChange;//值变更事件
    property OnGetValue:TNotifyEvent read FOnGetValue write FOnGetValue;//值获取事件
    property Index:Integer read GetIndex;//索引
    property Data:Pointer read FData write FData;//附加数据成员，在本单元内部实现中，用于参数对原始函数的关联
    property Text:WideString read GetText;//实现文本
    property Mutable:Boolean read FMutable write FMutable;//值是否是易变的，如果是易变的，则其值不会被缓存
    property Owner:TQExprParser read FOwner;//所隶属的所有者
    property ReadOnly:Boolean read FReadOnly write FReadOnly;//是否只读（常量)
  end;

  //函数参数实现
  TQParameter=class
  protected
    FName:WideString;//名称
    FValue:TQValue;//参数值
    FOwner:TQFunction;//对应的函数
    FFixed:Boolean;//是否是固定参数，固定参数必需赋值
    FIndex:Integer;//参数索引
  public
    constructor Create(AOwner:TQFunction);overload;
    destructor Destroy;override;
    property Name:WideString read FName;
    property Value:TQValue read FValue;
    property Fixed:Boolean read FFixed;
    property Index:Integer read FIndex;
  end;
  //函数实现，继承自TQVar，可以当做是特殊的变量来看待
  TQFunction=class(TQVar)
  private
    FParams:TList;//参数列表
    FVarParams: Boolean;//参数数目是否可变
    FFixedParamCount:Integer;//固定参数个数
    FMaxParamCount:Integer;//最大允许的参数个数
    function GetParamCount: Integer;
    function GetParameters(AIndex: Integer): TQParameter;
    function GetDeclareText: WideString;//获取声明文本
    function GetCallText: WideString;//获取调用文本
    procedure ClearParams;//清除所有的参数
  published
  public
    constructor Create(AOwner:TQExprParser);override;
    destructor Destroy;override;
    procedure Assign(AVar:TQVar);override;
    function AddParam(const AName:WideString;AFixed:Boolean=True):TQParameter;//增加一个参数
    procedure ClearVarParams;//清除所有可变参数
    function ParamByName(const AName:WideString):TQParameter;//按名称检索参数,不存在抛异常
    function FindParam(const AName:WideString):TQParameter;//按名称检索参数
    procedure Call;
    property Parameters[AIndex:Integer]:TQParameter read GetParameters;//参数列表
    property ParamCount:Integer read GetParamCount;//参数个数
    property DeclareText:WideString read GetDeclareText;//函数声明格式
    property CallText:WideString read GetCallText;//函数调用文本
    property VarParams:Boolean read FVarParams write FVarParams;//是否是可变参数
    property FixedParamCount:Integer read FFixedParamCount;//固定参数个数（必需输入的参数）
    property MaxParamCount:Integer read FMaxParamCount write FMaxParamCount;//最大允许的可变参数个数
  end;
  //运算符定义
  TQExprOperator=(oprNone,
    //逗号操作符和语句结束符,PRI=0
    oprComma,oprStatementEnd,
    //赋值运算符，PRI=1
    oprAssign,oprSelfAdd,oprSelfSub,oprSelfMul,oprSelfDiv,oprSelfDivTrunc,oprSelfMod,
    oprSelfBitAnd,oprSelfBitOr,oprSelfBitXor,oprSelfLShift,oprSelfRShift,
    //逻辑运算符, PRI=2
    oprAnd,oprOr,oprNot,
    //位运算符, PRI=3
    oprBitAnd,oprBitOr,oprBitXor,oprBitNot,
    //比较操作符, PRI=4
    oprEqual,oprNotEqual,oprLessThan,oprLessThanEqual,oprGreatThan,oprGreatThanEqual,
    //移位操作符，PRI=5
    oprLShift,oprRShift,
    //算术加减运算符，PRI=6
    oprAdd,oprSub,
    //乘除余运算符，PRI＝7
    oprMul,oprDiv,oprDivTrunc,oprMod,
    //幂运算符，PRI=8
    oprPower,
    //自增或自减运算符，PRI=9
    oprInc,oprDec,
    //注释
    oprLineComment,oprBlockComment,
    //括号,PRI=10
    oprBraceStart,oprBraceEnd,oprBlockStart,oprBlockEnd,oprMax=oprBlockEnd
    );
  //运算符实现函数（与普通函数相比，增加绑定的操作符成员）
  TQOperatorFunction=class(TQFunction)
  private
    FBindOperator: TQExprOperator;
  published
  public
    constructor Create(AOwner:TQExprParser);override;
    procedure Assign(AVar:TQVar);override;
    property BindOperator:TQExprOperator read FBindOperator write FBindOperator;//绑定的操作符
  end;
  //脚本中自定义函数实现
  TQScriptFunction=class(TQFunction)
  protected
    FExprParser:TQExprParser;
    function GetValue:TQValue;override;
    procedure DoVarNeeded(ASender:TQExprParser;AName:WideString;var AResult:TQVar);
    procedure DoFunctionNeeded(ASender:TQExprParser;AName:WideString;var AResult:TQVar);
    procedure Parse(var p:PWideChar);
    procedure ParseBlock(var p:PWideChar);
    procedure DoGetParamValue(ASender:TObject);
    procedure DoGetParams(ASender:TObject);
    procedure DoGetParamCount(ASender:TObject);
  public
    constructor Create(AOwner:TQExprParser);override;
    destructor Destroy;override;
    procedure Assign(AVar:TQVar);override;
  end;
  //语句
  TQExprStatement=class
  private
    FItems:TList;//子语句列表
    FOwner:TQExprParser;//所有者
    FLineNo,FOffset:Integer;//行号、偏移量
    FParent:TQExprStatement;//父对象
    FText:WideString;//语句对应的文本内容
    FResult:TQVar;//结果值
    function GetCount: Cardinal;
    function GetItems(AIndex: Integer): TQExprStatement;
    function GetResult: TQVar;
    procedure InternalParse(var p:PWideChar;AParent:TQVar);
  published
  public
    constructor Create(AOwner:TQExprParser);overload;
    destructor Destroy;override;
    procedure Assign(ASource:TQExprStatement);
    function Add(ALineNo,AOffset:Integer):TQExprStatement;//添加子语句
    procedure Clear;//清除
    procedure Parse(var p:PWideChar);//解析一行代码
    property LineNo:Integer read FLineNo;//行号
    property Offset:Integer read FOffset;//起始偏移
    property Count:Cardinal read GetCount;//行数
    property Items[AIndex:Integer]:TQExprStatement read GetItems;//子语句
    property Owner:TQExprParser read FOwner;//所有者
    property Parent:TQExprStatement read FParent;//父语句
    property Result:TQVar read GetResult;//结果
  end;
  TQExprVarNeededEvent=procedure (ASender:TQExprParser;AName:WideString;var AResult:TQVar) of object;
  TQExprExecuteEvent=procedure(ASender:TQExprParser;AStatement:TQExprStatement) of object;
  TQExprAssignHelper=procedure(ASender:TQExprParser;AVar:TQVar) of object;
  //表达式解析对象
  TQExprParser=class(TQVar)
  protected
    FResult:TQVar;
    FLocals:TQVar;
    FAliases:TQVar;
    FAborted,FBreakCurrent:Boolean;
    FAssigning:Boolean;
    FVarIndex,FLineNo,FIP:Cardinal;
    FParsingText:PWideChar;
    FExecuted:Boolean;
    FStatements:TQExprStatement;
    FText:WideString;
    FOnVarNeeded: TQExprVarNeededEvent;
    FOnFunctionNeeded: TQExprVarNeededEvent;
    FAfterExecStatement: TQExprExecuteEvent;
    FBeforeExecStatement: TQExprExecuteEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FAssignHelper:TQExprAssignHelper;
    function OperatorType(var p:PWideChar):TQExprOperator;virtual;
    function GetValue:TQValue;override;
    function GetResult: TQVar;
    function GetCompiledText: WideString;
    procedure DoVarAssigned(AVar:TQVar);
  published
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Assign(ASource:TQExprParser);reintroduce;
    //添加常量
    function AddConst(const AName:WideString;const AVal:Integer):TQVar;overload;
    function AddConst(const AName:WideString;const AVal:Int64):TQVar;overload;
    function AddConst(const AName:WideString;const AVal:Double):TQVar;overload;
    function AddConst(const AName:WideString;const AVal:TDateTime):TQVar;overload;
    function AddConst(const AName:WideString;const AVal:WideString):TQVar;overload;
    function AddVar(const AName:WideString;AOnExecute:TNotifyEvent):TQVar;overload;//添加一个动态变量
    function AddFunction(const AName:WideString;AOnExecute:TNotifyEvent):TQFunction;overload;//添加一个函数
    function FunctionByName(const AName:WideString):TQFunction;virtual;//查找指定名称的函数
    function VarByName(const AName:WideString):TQVar;virtual;//查找指定名称的变量(如果对应的是函数，返回空）
    function VarByPath(AName:WideString):TQVar;virtual;//查找指定名称的变量(不区分函数还是普通变量)
    function NextTempName(ALeader:WideString):WideString;//下一个临时变量
    procedure AddAlias(const ANewName:WideString;ARef:TQVar);
    procedure RegisterFunctions(AFunctions:TQFunctions);//注册一组函数
    procedure Parse(const S:WideString);//解析表达式
    procedure Calc;//执行一次计算
    procedure Clear(AFreeNeeded:Boolean=true);override;//清除末次解析
    procedure BeginRegister;virtual;//开始注册
    procedure EndRegister;virtual;//结束注册
    procedure SetAssignHelper(AHelper:TQExprAssignHelper);
    property Result:TQVar read GetResult;//结果
    property Locals:TQVar read FLocals;//局部变量
    property CompiledText:WideString read GetCompiledText;//编译后的解释器代码（与原始代码相比，相当于格式化了）
    property Aborted:Boolean read FAborted write FAborted;//是否当前执行被中断（调用了Exit函数）
    property Text:WideString read FText write Parse;//原始代码文本
    property OnVarNeeded:TQExprVarNeededEvent read FOnVarNeeded write FOnVarNeeded;//当一个变量定义未找时触发
    property OnFunctionNeeded:TQExprVarNeededEvent read FOnFunctionNeeded write FOnFunctionNeeded;//当一个函数定义未找到时触发
    property BeforeExecStatement:TQExprExecuteEvent read FBeforeExecStatement write FBeforeExecStatement;//在执行语句前触发
    property AfterExecStatement:TQExprExecuteEvent read FAfterExecStatement write FAfterExecStatement;//在执行语句后触发
    property BeforeExecute:TNotifyEvent read FBeforeExecute write FBeforeExecute;//在执行代码前触发
    property AfterExecute:TNotifyEvent read FAfterExecute write FAfterExecute;//在执行代码后触发
  end;
  TQPasParser=class(TQExprParser)
  protected
    function OperatorType(var p:PWideChar):TQExprOperator;override;
  end;
  TQExpGlobal=class(TQExprParser)
  protected
    FLocker:TCriticalSection;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure BeginRegister;override;
    procedure EndRegister;override;
    function VarByName(const AName:WideString):TQVar;override;//查找指定名称的变量
    function VarByPath(AName:WideString):TQVar;override;
    function FunctionByName(const AName:WideString):TQFunction;override;
  end;
  //函数列表实现
  TQFunctions=class
  protected
    //注册一个函数
    function RegisterFunction(AParser:TQVar;AName:WideString;AHandler:TNotifyEvent;AFixedParams:WideString;AVarParams:Boolean=false):TQFunction;
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);virtual;abstract;//注册实现代码，子类必需实现
  end;
  //日期组成部分
  TDatePart=(dpNone,dpYear,dpMonth,dpDay,dpHour,dpMinute,dpSecond,dpMSecond);
  //日期范围部分
  TDateRange=(drNone,drCentury,drYear,drQuarter,drMonth,drDay,drHour,drMinute,drSecond);
  //日期函数
  TDateFunctions=class(TQFunctions)
  protected
    function DatePartOfName(const AName:WideString):TDatePart;
    function DateRangeOfName(const AName:WideString):TDateRange;
    procedure DoDateAdd(ASender:TObject);
    procedure DoDateDiff(ASender:TObject);
    procedure DoDatePart(ASender:TObject);
    procedure DoNow(ASender:TObject);
    procedure DoDate(ASender:TObject);
    procedure DoTime(ASender:TObject);
    procedure DoMonthDays(ASender:TObject);
    procedure DoIsLeapYear(ASender:TObject);
    procedure DoEncodeDate(ASender:TObject);
    procedure DoEncodeTime(ASender:TObject);
    procedure DoToday(ASender:TObject);
    procedure DoYesterday(ASender:TObject);
    procedure DoTomorrow(ASender:TObject);
    procedure DoIsToday(ASender:TObject);
    procedure DoDateStart(ASender:TObject);//年、季、月、周的起始日期
    procedure DoDateEnd(ASender:TObject);
    procedure DoDateReplace(ASender:TObject);//替换日期时间的指定部分
    function NeedDatePart(AValue:TQValue):TDatePart;
    function NeedDateRange(AValue:TQValue):TDateRange;
  public
    constructor Create;
    procedure Register(AParser:TQExprParser);override;
  end;
  //统计函数，将来数学函数也加到这里
  TMathFunctions=class(TQFunctions)
  protected
    procedure DoAvg(ASender:TObject);
    procedure DoSum(ASender:TObject);
    procedure DoMax(ASender:TObject);
    procedure DoMin(ASender:TObject);
    procedure DoCount(ASender:TObject);
    procedure DoStdDev(ASender:TObject);
    procedure DoRound(ASender:TObject);
    procedure DoBankRound(ASender:TObject);
  public
    constructor Create;
    procedure Register(AParser:TQExprParser);override;
  end;
  //操作符响应函数
  TOperatorFunctions=class(TQFunctions)
  protected
    procedure DoAssign(ASender:TObject);
    procedure DoMathAdd(ASender:TObject);
    procedure DoMathSub(ASender:TObject);
    procedure DoMathMultiply(ASender:TObject);
    procedure DoMathDiv(ASender:TObject);
    procedure DoMathMod(ASender:TObject);
    procedure DoMathPower(ASender:TObject);
    procedure DoBitAnd(ASender:TObject);
    procedure DoBitOr(ASender:TObject);
    procedure DoBitNot(ASender:TObject);
    procedure DoBitXor(ASender:TObject);
    procedure DoLogicalAnd(ASender:TObject);
    procedure DoLogicalOr(ASender:TObject);
    procedure DoLogicalNot(ASender:TObject);
    procedure DoCompareLT(ASender:TObject);
    procedure DoCompareGT(ASender:TObject);
    procedure DoCompareEQ(ASender:TObject);
    procedure DoCompareNE(ASender:TObject);
    procedure DoCompareLE(ASender:TObject);
    procedure DoCompareGE(ASender:TObject);
    procedure DoShiftLeft(ASender:TObject);// <<
    procedure DoShiftRight(ASender:TObject);// >>
    procedure DoDivTrunc(ASender:TObject);// \
    procedure DoAddToSelf(ASender:TObject);// +=
    procedure DoSubToSelf(ASender:TObject);// -=
    procedure DoMulToSelf(ASender:TObject);// *=
    procedure DoDivToSelf(ASender:TObject);// /=
    procedure DoModToSelf(ASender:TObject);// %=
    procedure DoAndToSelf(ASender:TObject);// &=
    procedure DoOrToSelf(ASender:TObject);// |=
    procedure DoXorToSelf(ASender:TObject);// ^=
    procedure DoLShiftToSelf(ASender:TObject);//<<=
    procedure DoRShiftToSelf(ASender:TObject);//>>=
    procedure DoDivTruncToSelf(ASender:TObject);// \=
    procedure DoIncrement(ASender:TObject);//++
    procedure DoDecrement(ASender:TObject);//--
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);override;
  end;
  //字符串处理函数
  TStringFunctions=class(TQFunctions)
  private
    {$IFDEF QEXP_REGEX}
    FRegex:TPerlRegEx;
    FSplitStrings:TStringList;
    FLastRegexExp:WideString;
    {$ENDIF}
    procedure DoLeft(ASender:TObject);
    procedure DoRight(ASender:TObject);
    procedure DoUpper(ASender:TObject);
    procedure DoLower(ASender:TObject);
    procedure DoStrCmp(ASender:TObject);
    procedure DoQuotedStr(ASender:TObject);
    procedure DoDequoteStr(ASender:TObject);
    procedure DoPos(ASender:TObject);
    procedure DoLength(ASender:TObject);
    procedure DoSubString(ASender:TObject);
    procedure DoLike(ASender:TObject);
    procedure DoStartWith(ASender:TObject);
    procedure DoEndWith(ASender:TObject);
    procedure DoDecodeToken(ASender:TObject);
    {$IFDEF QEXP_REGEX}
    procedure DoRegexMatch(ASender:TObject);
    procedure DoRegexMatchNext(ASender:TObject);
    procedure DoRegexLastOffset(ASender:TObject);
    procedure DoRegexLastLength(ASender:TObject);
    procedure DoRegexSplit(ASender:TObject);
    procedure DoRegexSplitCount(ASender:TObject);
    procedure DoRegexSplitText(ASender:TObject);
    procedure DoRegexSplitOffset(ASender:TObject);
    procedure DoRegexReplace(ASender:TObject);
    function SetupRegexExp(S:WideString):Boolean;
    {$ENDIF}
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);override;
  end;
  //二进制格式支持
  TQExpBytesFunctions=class(TQFunctions)
  protected
    procedure DoGetByte(ASender:TObject);
    procedure DoSetByte(ASender:TObject);
    procedure DoGetByteSize(ASender:TObject);
    procedure DoFromHex(ASender:TObject);
    procedure DoInsertBytes(ASender:TObject);
    procedure DoAppendBytes(ASender:TObject);
    procedure DoSaveBytes(ASender:TObject);
    procedure DoLoadBytes(ASender:TObject);
    procedure DoFromStr(ASender:TObject);
    procedure DoReadText(ASender:TObject);
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);override;
  end;

  //辅助工具函数
  TUtilFunctions=class(TQFunctions)
  private
    procedure DoInteger(ASender:TObject);
    procedure DoInt64(ASender:TObject);
    procedure DoFloat(ASender:TObject);
    procedure DoDateTime(ASender:TObject);
    procedure DoString(ASender:TObject);
    procedure DoChar(ASender:TObject);
    procedure DoMsgBox(ASender:TObject);
    procedure DoIfThen(ASender:TObject);
    procedure DoGoto(ASender:TObject);
    procedure DoExit(ASender:TObject);
    procedure DoWhile(ASender:TObject);
    procedure DoBreak(ASender:TObject);
    procedure DoEval(ASender:TObject);
    procedure DoEvalVarNeeded(ASender:TQExprParser;AName:WideString;var AResult:TQVar);
    procedure DoEvalFunctionNeeded(ASender:TQExprParser;AName:WideString;var AResult:TQVar);
  public
    constructor Create;overload;
    destructor Destroy;override;
    procedure Register(AParser:TQExprParser);override;
  end;
  TOperatorParamPosition=(oppNone,oppLeft,oppRight,oppBoth,oppAny);
  TQOperatorInfo=record
    Text:WideString;
    Pri:Integer;
    ParamPos:TOperatorParamPosition;
    OnExecute:TQOperatorFunction;
  end;
  //工具函数
function DetectTextType(p:PWideChar):Integer;
function DetectDateTime(p:PWideChar):TDateTime;
procedure ParserError(ACode:Integer;AMsg:String);
function CharIn(const c:WideChar;s:PWideChar):Boolean;
function UnescapeText(S:WideString):WideString;
function EscapeText(S:WideString):WideString;
function DecodeQuotedStr(var p:PWideChar;var AResult:WideString):Boolean;
function ExtractVarName(ANamePath:WideString):WideString;
function ExtractVarPath(ANamePath:WideString):WideString;
function BytesToStr(ABytes:PByte;AByteSize:Integer):WideString;
function StrToBytes(S:WideString):AnsiString;
function DecodeText(p:PAnsiChar;l:Integer):WideString;
{$IFNDEF QDAC_SUPPORT}
function DecodeToken(var p:PWideChar;ADelimiters:PWideChar;AQuoteChar:WideChar;AIgnoreSpace:Boolean=true):WideString;
{$ENDIF}
procedure QExp_TestCase;
var
  Operators:array [oprNone..oprMax] of TQOperatorInfo=(
    (Text:'';Pri:-1;ParamPos:oppNone),
    //逗号操作符和语句结束符,PRI=0
    (Text:',';Pri:0;ParamPos:oppNone),
    (Text:';';Pri:0;ParamPos:oppNone),
    //赋值运算符，PRI=1
    (Text:'=';Pri:1;ParamPos:oppBoth),
    (Text:'+=';Pri:1;ParamPos:oppBoth),
    (Text:'-=';Pri:1;ParamPos:oppBoth),
    (Text:'*=';Pri:1;ParamPos:oppBoth),
    (Text:'/=';Pri:1;ParamPos:oppBoth),
    (Text:'\=';Pri:1;ParamPos:oppBoth),
    (Text:'%=';Pri:1;ParamPos:oppBoth),
    (Text:'&=';Pri:1;ParamPos:oppBoth),
    (Text:'|=';Pri:1;ParamPos:oppBoth),
    (Text:'^=';Pri:1;ParamPos:oppBoth),
    (Text:'<<=';Pri:1;ParamPos:oppBoth),
    (Text:'>>=';Pri:1;ParamPos:oppBoth),
    //逻辑运算符, PRI=2
    (Text:'&&';Pri:2;ParamPos:oppBoth),
    (Text:'||';Pri:2;ParamPos:oppBoth),
    (Text:'!';Pri:2;ParamPos:oppRight),
    //位运算符, PRI=3
    (Text:'&';Pri:3;ParamPos:oppBoth),
    (Text:'|';Pri:3;ParamPos:oppBoth),
    (Text:'^';Pri:3;ParamPos:oppBoth),
    (Text:'~';Pri:3;ParamPos:oppRight),
    //比较操作符, PRI=4
    (Text:'==';Pri:4;ParamPos:oppBoth),
    (Text:'!=';Pri:4;ParamPos:oppBoth),
    (Text:'<';Pri:4;ParamPos:oppBoth),
    (Text:'<=';Pri:4;ParamPos:oppBoth),
    (Text:'>';Pri:4;ParamPos:oppBoth),
    (Text:'>=';Pri:4;ParamPos:oppBoth),
    //移位操作符，PRI=5
    (Text:'<<';Pri:5;ParamPos:oppBoth),
    (Text:'>>';Pri:5;ParamPos:oppBoth),
    //算术加减运算符，PRI=6
    (Text:'+';Pri:6;ParamPos:oppBoth),
    (Text:'-';Pri:6;ParamPos:oppBoth),
    //乘除余运算符，PRI＝7
    (Text:'*';Pri:7;ParamPos:oppBoth),
    (Text:'/';Pri:7;ParamPos:oppBoth),
    (Text:'\';Pri:7;ParamPos:oppBoth),
    (Text:'%';Pri:7;ParamPos:oppBoth),
    //幂运算符，PRI=8
    (Text:'**';Pri:8;ParamPos:oppBoth),
    //自增或自减运算符，PRI=9
    (Text:'++';Pri:9;ParamPos:oppAny),
    (Text:'--';Pri:9;ParamPos:oppAny),
    //注释
    (Text:'//';Pri:10;ParamPos:oppNone),
    (Text:'/*';Pri:10;ParamPos:oppNone),
    //括号,PRI=10
    (Text:'{';Pri:10;ParamPos:oppNone),
    (Text:'}';Pri:10;ParamPos:oppNone),
    (Text:'(';Pri:255;ParamPos:oppNone),
    (Text:')';Pri:255;ParamPos:oppNone)
    );
  QExpGlobal:TQExprParser;
const
  EPARSER_USER                = $10000000;
  EPARSER_CHAR_UNEXPECT       = $00000001;
  EPARSER_UNSUPPORT_OPRERATOR = $00000002;
  EPARSER_DIVBYZERO           = $00000003;
  EPARSER_NOMEAN_VALUE        = $00000004;
  EPARSER_NAME_EXISTS         = $00000005;
  EPARSER_NAME_EMPTY          = $00000006;
  EPARSER_NAME_DOT            = $00000007;
  EPARSER_PARAM_MISSED        = $00000008;
  EPARSER_BAD_DATEPART        = $00000009;
  EPARSER_BAD_MONTH           = $0000000A;
  EPARSER_BAD_DATERANGE       = $0000000B;
  EPARSER_PARAM_ATLEASTONE    = $0000000C;
  EPARSER_LEFT_VAR_NEEDED     = $0000000D;
  EPARSER_BAD_TOKEN           = $0000000E;
  EPARSER_PARAM_TOOMANY       = $0000000F;
  EPARSER_PARAM_NOT_END       = $00000010;
  EPARSER_FUNC_MISSED         = $00000011;
  EPARSER_OPERATOR_NOTIMPL    = $00000012;
  EPARSER_OPERATOR_NOPARAM    = $00000013;
  EPARSER_OPERATOR_PARAM_NEEDINTEGER   = $00000014;
  EPARSER_CIRCULAR_REF        = $00000015;
  EPARSER_OUT_OF_RANGE        = $00000016;
  EPARSER_MIN_MAX             = $00000017;
  EPARSER_BAD_TYPE_VALUE      = $00000018;
  EPARSER_MAX_UNSUPPORT       = $00000019;
  EPARSER_MIN_UNSUPPORT       = $0000001A;
  EPARSER_VAR_NEEDED          = $0000001B;
  EPARSER_BAD_MATCH_STATE     = $0000001C;
  EPARSER_BAD_REGEX_EXP       = $0000001D;
  EPARSER_PARAM_TOOFEW        = $0000001E;
  EPARSER_BAD_NAME            = $0000001F;
  EPARSER_NEED_CHAR           = $00000020;
  EPARSER_CONST_READONLY      = $00000021;
  EPARSER_BAD_TYPE            = $00000022;
  EPARSER_BAD_PARAM_ORDER     = $00000023;
  EPARSER_FUNCTION_NEEDCALL   = $00000024;
  EPARSER_NEED_END            = $00000025;

{$IFNDEF QDAC_SUPPORT}
resourcestring
  EMSG_CHAR_UNEXPECT = '多余的字符 %s .';
  EMSG_UNSUPPORT_OPERATOR='操作符 %s 不支持当前数据类型.';
  EMSG_DIVBYZERO='除数不能为0.';
  EMSG_NOMEAN_VALUE='对于操作%s，当前值无任何意义.';
  EMSG_NAME_EXISTS='指定的名称 %s 已存在.';
  EMSG_NAME_EMPTY='变量或函数名称不能为空.';
  EMSG_NAME_DOT='变量或函数名称不能包含".".';
  EMSG_PARAM_MISSED='指定的参数 %s 不存在.';
  EMSG_BAD_DATEPART='指定的日期部分值 %s 无效.';
  EMSG_BAD_MONTH='指定的值 %s 不是有效的月份值.';
  EMSG_BAD_DATERANGE='指定的日期范围值 %s 无效.';
  EMSG_PARAM_ATLEASTONE='函数 %s 需要至少提供一个参数.';
  EMSG_LEFT_VAR_NEEDED='操作符 %s 要求左值为变量.';
  EMSG_BAD_TOKEN='%s 不是一个可以识别的词.';
  EMSG_PARAM_TOOMANY='提供给函数 %s 的参数超过定义.';
  EMSG_PARAM_NOT_END='函数参数未结束(缺少右括号).';
  EMSG_FUNC_MISSED='未找到指定的函数 %s.';
  EMSG_OPERATOR_NOTIMPL='操作符 %s 的实现不存在或未关联.';
  EMSG_OPERATOR_NOPARAM='操作符 %s 缺少需要的参数.';
  EMSG_OPERATOR_PARAM_NEEDINTEGER='操作符 %s 需要整形参数.';
  EMSG_CIRCULAR_REF='发现对 %s 的循环引用.';
  EMSG_OUT_OF_RANGE='函数%s的参数%s的值超出允许范围.';
  EMSG_MIN_MAX='最大值不能最小值．';
  EMSG_BAD_TYPE_VALUE='值 %s 不符合要求格式或超出允许范围．';
  EMSG_MAX_UNSUPPORT='类型不支持设置最大值';
  EMSG_MIN_UNSUPPORT='类型不支持设置最小值';
  EMSG_VAR_NEEDED='函数 %s 的参数 %s 的参数要求是一个变量．';
  EMSG_BAD_MATCH_STATE='当前未处于匹配状态．';
  EMSG_BAD_REGEX_EXP='无效的正则表达式:%s．';
  EMSG_PARAM_TOOFEW='提供给函数 %s 的参数不足．';
  EMSG_BAD_NAME='无法识别的名称%s．';
  EMSG_NEED_CHAR='期望后面出现字符 %s.';
  EMSG_CONST_READONLY='常量 %s 的值不允许被修改。';
  EMSG_BAD_TYPE='指定的值类型与期望的类型不符。';
  EMSG_BAD_PARAM_ORDER='可选参数必需位于必需参数后面。';
  EMSG_FUNCTION_NEEDCALL='%s 做为一个函数调用时需要使用()包含调用参数。';
  EMSG_NEED_END='期待语句结束符 ";"，而不是 "%s"';
{$ENDIF}  
implementation
uses dateutils,forms
{$IFDEF QDAC_SUPPORT}
,mmds,mmdslang
{$ENDIF}
;
{$IFNDEF QDAC_SUPPORT}
type
  TTextEncoding=(teAnsi, { Ansi编码 }
    teUnicode16LE, { Unicode LE 编码 }
    teUnicode16BE, { Unicode BE 编码 }
    teUTF8 { UTF8编码 }
    );
{$ENDIF}
var
  DateFunctions:TDateFunctions;
  MathFunctions:TMathFunctions;
  StringFunctions:TStringFunctions;
  UtilFunctions:TUtilFunctions;
  BytesFunction:TQExpBytesFunctions;
  OperatorFunctions:TOperatorFunctions;
{
int WINAPI MessageBoxTimeoutW(IN HWND hWnd, IN LPCWSTR lpText, IN LPCWSTR lpCaption, IN UINT uType, IN WORD wLanguageId, IN DWORD dwMilliseconds);
}
function MessageBoxTimeoutW(hWnd:HWND;lpText,lpCaption:PWideChar;uType:Cardinal;wLanguageId:Word;dwMilliseconds:DWORD):Integer;WINAPI;external user32;

{$IFNDEF QDAC_SUPPORT}
{将指定的UTF8字符串转换为Unicode编码的字符串
Parameters
  S : 源UTF8编码字符串
Returns
  返回编码结果
}
function UTF8Decode(S:PAnsiChar;L:Integer):WideString;overload;
var
  ALen:Integer;
begin
SetLength(Result,0);
if L<=0 then
  begin
  L:=0;
  while S[L]<>#0 do
    Inc(L);
  end;
if L>0 then
  begin
  ALen:=MultiByteToWideChar(CP_UTF8,0,S,L,nil,0);
  SetLength(Result,ALen);
  MultiByteToWideChar(CP_UTF8,0,S,L,PWideChar(Result),ALen);
  end;
end;
function UTF8Decode(S:AnsiString):WideString;overload;
begin
Result:=Utf8Decode(PAnsiChar(S),Length(S));
end;

{将指定的Unicode字符串转换为UTF8编码的字符串
Parameters
  S : 原始字符串
Returns
  返回转换后的字符串  
}
function UTF8Encode(S:WideString):AnsiString;
var
  ALen,L:Integer;
begin
SetLength(Result,0);
L:=Length(S);
if L>0 then
  begin
  ALen:=WideCharToMultiByte(CP_UTF8,0,PWideChar(S),L,nil,0,nil,nil);
  SetLength(Result,ALen);
  WideCharToMultiByte(CP_UTF8,0,PWideChar(S),L,PAnsiChar(Result),ALen,nil,nil);
  end
end;
{$ENDIF}
procedure ParserError(ACode:Integer;AMsg:String);
begin
raise EParserError.Create(ACode,AMsg);
end;

function CharIn(const c:WideChar;s:PWideChar):Boolean;
begin
Result:=False;
while s^<>#0 do
  begin
  if s^=c then
    begin
    Result:=True;
    Break;
    end;
  Inc(s);
  end;
end;

function IsBreakChar(c:WideChar):Boolean;
begin
Result:=False;
if c<'.' then//!"#$%&'()*+,-
  Result:=True
else if c='/' then
  Result:=True
else if (c>=':') and (c<='@') then//:;<=>?@
  Result:=True
else if (c>='[') and (c<='^') then//[\]^
  Result:=True
else if c='`' then
  Result:=True
else if (c>'z') and (c<='~') then
  Result:=True;
end;

function DecodeQuotedStr(var p:PWideChar;var AResult:WideString):Boolean;
var
  ws,pd:PWideChar;
  AQuoter:WideChar;
  AVal,C:Integer;
begin
if CharIn(p^,'"''') then
  begin
  AQuoter:=p^;
  SetLength(AResult,0);
  Inc(p);
  ws:=p;
  //计算到下一个引号之间的内容长度
  while p^<>#0 do
    begin
    if p^=AQuoter then
      begin
      if p[1]=AQuoter then
        begin
        Inc(p,2);
        end
      else
        Break;
      end
    else if p^='\' then
      begin
      if p[1]=AQuoter then
        begin
        Inc(p,2);
        end
      else
        Inc(p);
      end
    else
      Inc(p);
    end;
  SetLength(AResult,p-ws);
  pd:=PWideChar(AResult);
  while ws<>p do
    begin
    if (ws[0]='\') then
      begin
      if ws[1]=AQuoter then
        begin
        pd^:=AQuoter;
        Inc(ws,2);
        end
      else if ws[1]='r' then
        begin
        pd^:=#13;
        Inc(ws,2);
        end
      else if ws[1]='n' then
        begin
        pd^:=#10;
        Inc(ws,2);
        end
      else if ws[1]='t' then
        begin
        pd^:=#9;
        Inc(ws,2);
        end
      else if ws[1]='a' then
        begin
        pd^:=#7;
        Inc(ws,2);
        end
      else if ws[1]='b' then
        begin
        pd^:=#8;
        Inc(ws,2);
        end
      else if ws[1]='f' then
        begin
        pd^:=#12;
        Inc(ws,2);
        end
      else if ws[1]='v' then
        begin
        pd^:=#11;
        Inc(ws,2);
        end
      else if ws[1]='\' then
        begin
        pd^:='\';
        Inc(ws,2);
        end
      else if ws[1]='''' then
        begin
        pd^:='''';
        Inc(ws,2);
        end
      else if ws[1]='"' then
        begin
        pd^:='"';
        Inc(ws,2);
        end
      else if ws[1]='0' then
        begin
        pd^:=#0;
        Inc(ws,2);
        end
      else if (ws[1]='x') or (ws[1]='X') then
        begin
        AVal:=0;
        Inc(ws,2);
        C:=0;
        while ws[C]<>#0 do
          begin
          if (ws[C]>='0') and (ws[C]<='9') then
            AVal:=(AVal shl 4)+Ord(ws[C])-Ord('0')
          else if (ws[C]>='a') and (ws[C]<='f') then
            AVal:=(AVal shl 4)+Ord(ws[C])-Ord('a')+10
          else if (ws[C]>='A') and (ws[C]<='F') then
            AVal:=(AVal shl 4)+Ord(ws[C])-Ord('A')
          else
            Break;
          Inc(C);
          if C=4 then//最多4个字节，Unicode字符
            Break;
          end;
        if C>0 then
          begin
          pd^:=WideChar(AVal);
          Inc(ws,C);
          end
        else
          begin
          pd^:='\';
          Inc(ws);
          end;
        end
      else//八进制？
        begin
        AVal:=0;
        C:=0;
        while ws[C]<>#0 do
          begin
          if (ws[C]>='0') and (ws[C]<='7') then
            begin
            AVal:=(AVal shl 3)+Ord(ws[C])-Ord(ws[0]);
            Inc(C);
            if C=6 then
              Break;
            end
          else
            Break;
          end;
        if C>0 then
          begin
          pd^:=WideChar(AVal);
          Inc(ws,C);
          end
        else
          begin
          pd^:='\';
          Inc(ws);
          end;
        end;
      end
    else if (ws[0]=AQuoter) and (ws[1]=AQuoter) then
      begin
      pd^:=AQuoter;
      Inc(ws,2);
      end
    else
      begin
      pd^:=ws^;
      Inc(ws);
      end;
    Inc(pd);
    end;
  SetLength(AResult,pd-PWideChar(AResult));
  Inc(p);
  Result:=True;
  end
else
  Result:=False;
while CharIn(p^ ,#9#10#13' ') do
  Inc(p);  
end;

function DecodeIdent(var p:PWideChar):WideString;
var
  ws:PWideChar;
begin
ws:=p;
while p^<>#0 do
  begin
  if IsBreakChar(p^) then
    Break;
  Inc(p);
  end;
Result:=Copy(ws,1,p-ws);
while CharIn(p^ ,#9#10#13' ') do
  Inc(p);
end;

function EscapeText(S:WideString):WideString;
var
  p,d:PWideChar;
begin
p:=PWideChar(S);
SetLength(Result,Length(S) shl 1);
d:=PWideChar(Result);
while p^<>#0 do
  begin
  if p^='"' then
    begin
    d^:='\';
    Inc(d);
    d^:='"';
    end
  else if p^=#13 then
    begin
    d^:='\';
    Inc(d);
    d^:='r';
    end
  else if p^=#10 then
    begin
    d^:='\';
    Inc(d);
    d^:='n';
    end
  else if p^='\' then
    begin
    d^:='\';
    Inc(d);
    d^:='\';
    end
  else
    d^:=p^;
  Inc(d);
  Inc(p);
  end;
SetLength(Result,d-PWideChar(Result));
end;

function UnescapeText(S:WideString):WideString;
var
  ps,pd:PWideChar;
begin
ps:=PWideChar(S);
SetLength(Result,Length(S));
pd:=PWideChar(Result);
while ps^<>#0 do
  begin
  if ps^='"' then
    begin
    if ps[1]='"' then
      begin
      pd^:=ps^;
      Inc(ps);
      end
    else//Bad Format
      Break;
    end
  else if ps^='\' then
    begin
    if ps[1]='r' then
      pd^:=#10
    else if ps[1]='n' then
      pd^:=#13
    else if ps[1]='\' then
      pd^:='\'
    else
      pd^:=ps^;
    Inc(ps);
    end
  else
    pd^:=ps^;
  Inc(pd);
  Inc(ps);
  end;
SetLength(Result,pd-PWideChar(Result));  
end;

function ExtractVarPath(ANamePath:WideString):WideString;
var
  ps,p:PWideChar;
begin
Result:='';
ps:=PWideChar(ANamePath);
p:=ps+Length(ANamePath)-1;
while p>ps do
  begin
  if p^='.' then
    begin
    Result:=Copy(ps,0,p-ps);
    Break;
    end
  else
    Dec(p);
  end;
end;

function ExtractVarName(ANamePath:WideString):WideString;
var
  ps,p:PWideChar;
begin
Result:='';
ps:=PWideChar(ANamePath);
p:=ps+Length(ANamePath)-1;
while p>ps do
  begin
  if p^='.' then
    begin
    Result:=p+1;
    Break;
    end
  else
    Dec(p);
  end;
end;

function BytesToStr(ABytes:PByte;AByteSize:Integer):WideString;
{$IFNDEF UNICODE}
var
  S:AnsiString;
{$ENDIF}
begin
{$IFDEF UNICODE}
SetLength(Result,AByteSize shl 1);
BinToHex(ABytes,PWideChar(Result),AByteSize);
{$ELSE}
SetLength(S,AByteSize shl 1);
BinToHex(PAnsiChar(ABytes),PAnsiChar(S),AByteSize);
Result:=S;
{$ENDIF}
end;
function StrToBytes(S:WideString):AnsiString;
begin
SetLength(Result,Length(S) shr 1);
{$IFDEF UNICODE}
HexToBin(PWideChar(S),PAnsiChar(Result),Length(Result));
{$ELSE}
HexToBin(PAnsiChar(AnsiString(S)),PAnsiChar(Result),Length(Result));
{$ENDIF}
end;
{
检测指定的字符串类型
Parameters
  p : 要检测的字符串
Returns
  返回检测的类型结果
}
function DetectTextType(p:PWideChar):Integer;
  function DetectNumber(ADefType:Integer):Integer;
  var
    AIEEEFormat:Boolean;
  begin
  Result:=ADefType;
  AIEEEFormat:=False;
  while p^<>#0 do
    begin
    if p^='.' then
      begin
      if (Result and VARTYPE_INTEGER)<>0 then
        begin
        if (Result and VARTYPE_HEX_MASK)=0 then
          begin
          Result:=VARTYPE_FLOAT;
          Inc(p);
          end
        else
          begin
          Result:=VARTYPE_VARNAME;
          Break;
          end;
        end
      else
        begin
        Result:=VARTYPE_VARNAME;
        Break;
        end;
      end
    else if (p^>='0') and (p^<='9') then
      Inc(p)
    else if ((Result and VARTYPE_HEX_MASK)<>0) and (((p^>='A') and (p^<='F')) or ((p^>='a') and (p^<='f'))) then
      Inc(p)
    else if (p^='e') or (p^='E') then
      begin
      if not AIEEEFormat then
        begin
        AIEEEFormat:=True;
        Result:=VARTYPE_FLOAT;
        if (p[1]='+') or (p[1]='-') then//1E+10,1E-10等
          Inc(p);
        Inc(p);
        end
      else
        begin
        Result:=VARTYPE_VARNAME;
        Break;
        end;
      end
    else
      begin
      Result:=VARTYPE_VARNAME;
      Break;
      end;
    end
  end;
  function DetectString:Integer;
  var
    ADateTime:TDateTime;
    ADate:Integer;
  begin
  ADateTime:=DetectDateTime(p);
  if SameValue(ADateTime,0) then
    Result:=VARTYPE_STRING
  else
    begin
    ADate:=Trunc(ADateTime);
    if ADate=0 then
      Result:=VARTYPE_TIME
    else if SameValue(ADateTime-ADate,0) then
      Result:=VARTYPE_DATE
    else
      Result:=VARTYPE_DATETIME;
    end;
  end;
begin
case p^ of
  '#':
    begin
    Inc(p);
    Result:=DetectNumber(VARTYPE_INTEGER);
    end;
  '$':
    begin
    Inc(p);
    Result:=DetectNumber(VARTYPE_DHEX);
    end;
  '+':
    Result:=DetectTextType(p+1);
  '-':
    Result:=DetectTextType(p+1);
  '0'..'9':
    begin
    if (p[1]='x') or (p[1]='X') then
      begin
      Inc(p,2);
      Result:=DetectNumber(VARTYPE_CHEX);
      end
    else
      begin
      Result:=DetectNumber(VARTYPE_INTEGER);
      end;
    end;
  '.':
    begin
    Result:=DetectNumber(VARTYPE_FLOAT);
    end;
  '''','"':
    Result:=DetectString;
  't','T':
    if (((p[1]='r') or (p[1]='R')) and ((p[2]='u') or (p[2]='U')) and ((p[3]='e') or (p[3]='E'))) then
      Result:=VARTYPE_BOOLEAN
    else
      Result:=VARTYPE_VARNAME;
  'f','F':
    if (((p[1]='a') or (p[1]='A')) and ((p[2]='l') or (p[2]='L')) and ((p[3]='s') or (p[3]='S')) and ((p[4]='e') or (p[4]='E'))) then
      Result:=VARTYPE_BOOLEAN
    else
      Result:=VARTYPE_VARNAME
  else
    Result:=VARTYPE_VARNAME;
end;
end;

function DetectDateTime(p:PWideChar):TDateTime;
var
  ws:PWideChar;
  Y,M,D,H,N,S:Word;
  AVal,ACount:Integer;
  ATime:TDateTime;
  function ParseNum(var AVal:Integer):Integer;
  begin
  AVal:=0;
  Result:=0;
  while (p^>='0') and (p^<='9') do
    begin
    AVal:=AVal * 10+ Ord(p^)-Ord(WideChar('0'));
    Inc(Result);
    Inc(p);
    end;
  end;
begin
//日期格式yyyy-mm-dd yyyy/mm/dd
ws:=p;
Result:=0;
if (p^='''') or (p^='"') then
  Inc(p);
ACount:=ParseNum(AVal);
if ACount in [2,4] then
  begin
  if (p^='-') or (p^='/') then
    begin
    Y:=AVal;
    Inc(p);
    ACount:=ParseNum(AVal);
    if (ACount in [1..2]) and ((p^='-') or (p^='/')) then
      begin
      M:=AVal;
      Inc(p);
      ACount:=ParseNum(AVal);
      if (ACount in [1..2]) and ((p^=ws^) or CharIn(p^,' T'#9)) then
        begin
        D:=AVal;
        Result:=EncodeDate(Y,M,D);
        if p^<>ws^ then
          begin
          while (p^=' ') or (p^=#9) do
            Inc(p);
          if p^='T' then
            Inc(p);
          if p^<>ws^ then
            begin
            ATime:=DetectDateTime(p);
            if ATime<>0 then
              Result:=Result+ATime
            end
          end
        end;
      end;
    end
  else if (p^=':') and (ACount in [1..2]) and (AVal in [0..24]) then //ATime value
    begin
    H:=AVal;
    Inc(p);
    ACount:=ParseNum(AVal);
    if ACount in [1..2] then
      begin
      N:=AVal;
      if p^=':' then
        begin
        //秒
        Inc(p);
        ACount:=ParseNum(AVal);
        if ACount>0 then
          begin
          S:=AVal;
          if p^='.' then
            begin
            //毫秒
            Inc(p);
            ACount:=ParseNum(AVal);
            if ACount in [1..3] then
              Result:=EncodeTime(H,N,S,AVal)
            end
          else if p^=ws^ then
            Result:=EncodeTime(H,N,S,0)
          end
        end
      else if p^=ws^ then
        Result:=EncodeTime(H,N,0,0)
      end
    end;
  end;
end;

{检测指定内容的文本编码格式
Parameters
  p : 包含文本内容的指针对象，应该只包含一种编码格式，可以包含BOM
  l : 指针对应的内容长度，按字节计算
  b : 如果是通过BOM标志判断的，则为True，否则为False
Returns
  返回具体的编码格式，
}
function DetectTextEncode(const p:Pointer;l:Integer;var b:Boolean):TTextEncoding;
var
  pAnsi,s:PByte;
  pWide:PWideChar;
  ANoUtf8Found:Boolean;
  ACharLen,I,J:Integer;
begin
Result:=teAnsi;
b:=False;
if l>=2 then
  begin
  pAnsi:=PByte(p);
  pWide:=PWideChar(p);
  b:=True;
  if pWide^=#$FEFF then
    Result:=teUnicode16LE
  else if pWide^=#$FFFE then
    Result:=teUnicode16BE
	else if l>=3 then
    begin
		if (pAnsi^=$EF) and (PByte(Integer(pAnsi)+1)^=$BB) and (PByte(Integer(pAnsi)+2)^=$BF) then//UTF-8编码
			Result:=teUtf8
		else //检测字符中是否有符合UFT-8编码规则的字符，11...
			begin
      b:=False;
      for I := 0 to l - 1 do
        begin
				if (pAnsi^ and $80)<>0 then//高位为1
					begin
          if (pAnsi^ and $FE)=$FC then//6字节0x11 11 11 0?
						ACharLen:=6
					else if (pAnsi^ and $FC)=$F8 then//5字节0x11 11 10 ??
						ACharLen:=5
					else if (pAnsi^ and $F8)=$F0 then//4字节0x11 11 0? ??
						ACharLen:=4
					else if (pAnsi^ and $F0)=$E0 then//3字节0x11 10 ?? ??
						ACharLen:=3
					else if (pAnsi^ and $E0)=$C0 then//2字节0x11 0? ?? ??
						ACharLen:=2
					else//鬼才知道什么编码，当Ansi处理，反正不是UTF-8
            ACharLen:=0;
					if (ACharLen>0) and (i+ACharLen<=l) then
						begin
						ANoUtf8Found:=false;
            s:=PByte(Integer(PAnsi)+1);
						for J:=1 to ACharLen-2 do
							begin
							if (PByte(Integer(s)+j)^ and $C0)<>$80 then
								begin
								ANoUtf8Found:=true;
								break;
								end;
							end;
						if ANoUtf8Found then
							Result:=teAnsi
						else if ACharLen>2 then
							Result:=teUtf8;
            end;
          Break;
          end;
        Inc(pAnsi);
        end;
      end;
    end;
  end;
end;
{$IFNDEF QDAC_SUPPORT}
{从指定的字符串中按指定的分隔符列表解析子串
Parameters
  p : 源字符串
  ADelmiters : 分隔符列表
  AQuoteChar : 引号
  AIgnoreSpace : 是否忽略空白字符(如果取什么值，在引号中间的空白字符不会忽略）
Returns
  返回解析的首个子字符串
}
function DecodeToken(var p:PWideChar;ADelimiters:PWideChar;AQuoteChar:WideChar;AIgnoreSpace:Boolean=true):WideString;
var
  s:PWideChar;
  AQuoted:Boolean;
  function InDelimiter(c:WideChar):Boolean;
  var
    t:PWideChar;
  begin
  t:=ADelimiters;
  Result:=False;
  while t^<>#0 do
    begin
    if t^=c then
      begin
      Result:=True;
      Break;
      end;
    Inc(t);
    end;
  end;
begin
if AIgnoreSpace then
  begin
  while CharIn(p^,#9#10#13' ') do
    Inc(p);
  end;
s:=p;
if AQuoteChar<>#0 then
  AQuoted:=(s^=AQuoteChar)
else
  AQuoted:=False;
if AQuoted then
  Inc(p);
while p^<>#0 do
  begin
  if InDelimiter(p^)then
    begin
    if not AQuoted then
      break;
    end
  else if(p^=AQuoteChar)then
    begin
    if p[1]<>AQuoteChar then
        AQuoted:=False
    end;
  Inc(p);
  end;
SetLength(Result,p-s);
CopyMemory(PWideChar(Result),s,(p-s) shl 1);
if InDelimiter(p^) then
  Inc(p);
end;
{$ENDIF}
function DecodeText(p:PAnsiChar;l:Integer):WideString;
var
  AEncode:TTextEncoding;
  AByBOM:Boolean;
  function SwapByteOrder(p:PWord;ASize:Integer):PWideChar;
  var
    L,H:Word;
  begin
  Result:=PWideChar(p);
  while ASize>0 do
    begin
    L:=((p^ and $FF) shl 8) and $FF00;
    H:=((p^ and $FF00) shr 8) and $00FF;
    p^:=L or H;
    Inc(p);
    Dec(ASize);
    end;
  end;
begin
AEncode:=DetectTextEncode(p,l,AByBOM);
case AEncode of
  teAnsi:
    Result:=Copy(p,0,l);
  teUnicode16LE:
    begin
    if AByBOM then
      Result:=Copy(PWideChar(p),1,(l-2) shr 1)
    else
      Result:=Copy(p,0,l shr 1);
    end;
  teUnicode16BE:
    begin
    if AByBOM then
      begin
      Inc(p,2);
      Result:=SwapByteOrder(PWord(p),l-2);
      end
    else
      Result:=SwapByteOrder(PWord(p),l);
    end;
  teUTF8:
    begin
    if AByBOM then
      Result:=Utf8ToAnsi(Copy(p,3,l-3))
    else
      Result:=Utf8ToAnsi(Copy(p,0,l));
    end;
  end;
end;

{ TQValue }

procedure TQValue.Clear;
begin
FDataType:=VARTYPE_NULL;
SetLength(FValue,0);
end;

function TQValue.Compare_EQ(const V2: TQValue): Boolean;
var
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
Result:=False;
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  //Todo:可以优化以提高效率的地方
  Result:=SameValue(AValue1.AsFloat,AValue2.AsFloat)
  end
else if AValue1.IsString and AValue2.IsString then
  Result:=AValue1.AsString=AValue2.AsString
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprEqual].Text)]));
end;

function TQValue.Compare_GE(const V2: TQValue): Boolean;
begin
Result:=Compare_GT(V2) or Compare_EQ(V2);
end;

function TQValue.Compare_GT(const V2: TQValue): Boolean;
begin
Result:=V2.Compare_LT(Self);
end;

function TQValue.Compare_LE(const V2: TQValue): Boolean;
begin
Result:=Compare_LT(V2) or Compare_EQ(V2);
end;

function TQValue.Compare_LT(const V2: TQValue): Boolean;
var
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
Result:=False;
if AValue1.IsNumeric and AValue2.IsNumeric then
  Result:=AValue1.AsFloat<AValue2.AsFloat
else if AValue1.IsString and AValue2.IsString then
  Result:=AValue1.AsString<AValue2.AsString
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprEqual].Text)]));
end;

constructor TQValue.Create;
begin
inherited;
FDataType:=VARTYPE_NULL;
end;

procedure TQValue.DoBeforeChange;
begin
if Assigned(FBeforeChange) then
  FBeforeChange(Self);
end;

procedure TQValue.DoChange;
begin
if Assigned(FOnChange) then
  FOnChange(Self);
end;

function TQValue.GetAsBcd: TBcd;
begin
case FDataType of
  VARTYPE_NUMERIC://做为BCD
    Result:=PBcd(PAnsiChar(FValue))^;
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=IntegerToBcd(AsInteger);
  VARTYPE_INT64:
    Result:=StrToBcd(IntToStr(AsInt64));
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME:
    DoubleToBcd(AsFloat,Result);
  VARTYPE_NULL:
    Result:=NullBcd;
  VARTYPE_STRING:
    begin
    if not TryStrToBcd(AsString,Result) then
      raise EConvertError.Create(Format('%s不是有效的数值类型',[PWideChar(AsString)]));
    end;
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:=StrToBcd('1')
    else
      Result:=StrToBcd('0');
    end;
  VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsBcd;
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的数值类型',[PWideChar(AsString)]));
end;
end;

function TQValue.GetAsBoolean: Boolean;
begin
case FDataType of
  VARTYPE_BOOLEAN:
    Result:=PBoolean(PAnsiChar(FValue))^;
  VARTYPE_NULL:
    Result:=False;
  VARTYPE_STRING:
    Result:=StrToBool(AsString);
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=(AsInteger<>0);
  VARTYPE_INT64:
    Result:=(AsInt64<>0);
  VARTYPE_NUMERIC:
    Result:=not SameValue(BcdToDouble(AsBcd),0);
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME:
    Result:=not SameValue(AsFloat,0);
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的布尔类型',[PWideChar(AsString)]))
  else
    Result:=AsReference.Value.AsBoolean;
end;
end;

function TQValue.GetAsBytes: PByte;
begin
Result:=PByte(PAnsiChar(FValue));
end;

function TQValue.GetAsDateTime: TDateTime;
begin
case FDataType of
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
    Result:=AsFloat;
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:=1
    else
      Result:=0;
    end;
  VARTYPE_NULL:
    Result:=0;
  VARTYPE_STRING:
    Result:=StrToInt(AsString);
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=AsInteger;
  VARTYPE_INT64:
    Result:=AsInt64;
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的日期时间类型',[PWideChar(AsString)]));
  VARTYPE_NUMERIC:
    Result:=BcdToDouble(AsBcd)
  else //VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsDateTime;
end;
end;

function TQValue.GetAsFloat: Double;
begin
case FDataType of
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
    Result:=PDouble(PAnsiChar(FValue))^;
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:=1
    else
      Result:=0;
    end;
  VARTYPE_NULL:
    Result:=0;
  VARTYPE_STRING:
    Result:=StrToFloat(AsString);
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=AsInteger;
  VARTYPE_INT64:
    Result:=AsInt64;
  VARTYPE_NUMERIC:
    Result:=StrToFloat(BcdToStr(AsBcd));
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的数值类型',[PWideChar(AsString)]))
  else
//  VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsFloat;
end;
end;

function TQValue.GetAsInt64: Int64;
begin
case FDataType of
  VARTYPE_INT64:
    Result:=PInt64(PAnsiChar(FValue))^;
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=PInteger(PAnsiChar(FValue))^;
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME:
    Result:=Trunc(AsFloat);
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:=1
    else
      Result:=0;
    end;
  VARTYPE_NULL:
    Result:=0;
  VARTYPE_STRING:
    Result:=StrToInt(AsString);
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的数值类型',[PWideChar(AsString)]));
  VARTYPE_NUMERIC:
    Result:=BcdToInteger(AsBcd,true)
  else
//  VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsInt64;
end;
end;

function TQValue.GetAsInteger: Integer;
begin
case FDataType of
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=PInteger(PAnsiChar(FValue))^;
  VARTYPE_INT64:
    Result:=PInt64(PAnsiChar(FValue))^;
  VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME:
    Result:=Trunc(AsFloat);
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:=1
    else
      Result:=0;
    end;
  VARTYPE_NULL:
    Result:=0;
  VARTYPE_STRING:
    Result:=StrToInt(AsString);
  VARTYPE_NUMERIC:
    Result:=BcdToInteger(AsBcd,true);
  VARTYPE_BYTES:
    raise EConvertError.Create(Format('%s不是有效的数值类型',[PWideChar(AsString)]))
  else
//  VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsInteger;
end;
end;

function TQValue.GetAsReference: TQVar;
var
  AVar:TQVar;
  function InternalLookup(AItem:TQVar):TQVar;
  var
    ATemp:TQVar;
  begin
  Result:=AItem;
  if AItem.FValue.IsReference then
    begin
    ATemp:=PPointer(PAnsiChar(AItem.FValue.FValue))^;
    if ATemp=AVar then
      ParserError(EPARSER_CIRCULAR_REF,Format(EMSG_CIRCULAR_REF,[PWideChar(AVar.Name)]))
    else
      Result:=InternalLookup(ATemp);
    end
  end;
begin
if (FDataType=VARTYPE_VARNAME) or (FDataType=VARTYPE_FUNCTION) then
  begin
  AVar:=PPointer(PAnsiChar(FValue))^;
  if FDataType=VARTYPE_VARNAME then
    begin
    if AVar.FValue.IsReference then
      Result:=AVar.FValue.AsReference
    else
      Result:=AVar;
    end
  else
    Result:=AVar;
  end
else
  Result:=nil;
end;

function TQValue.GetAsString: WideString;
begin
case FDataType of
  VARTYPE_INTEGER,VARTYPE_CHEX,VARTYPE_DHEX:
    Result:=IntToStr(AsInteger);
  VARTYPE_INT64:
    Result:=IntToStr(AsInt64);
  VARTYPE_FLOAT:
    Result:=FloatToStr(AsFloat);
  VARTYPE_DATE:
    Result:=FormatDateTime('yyyy-mm-dd',AsDateTime);
  VARTYPE_TIME:
    Result:=FormatDateTime('hh:nn:ss.zzz',AsDateTime);
  VARTYPE_DATETIME:
    Result:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',AsDateTime);
  VARTYPE_BOOLEAN:
    begin
    if AsBoolean then
      Result:='True'
    else
      Result:='False';
    end;
  VARTYPE_NULL:
    SetLength(Result,0);
  VARTYPE_STRING:
    begin
    SetLength(Result,Length(FValue) shr 1);
    CopyMemory(PWideChar(Result),PAnsiChar(FValue),Length(FValue));
    end;
  VARTYPE_NUMERIC:
    Result:=BcdToStr(AsBcd);
  VARTYPE_FUNCTION,VARTYPE_VARNAME:
    Result:=AsReference.Value.AsString;
  VARTYPE_BYTES:
    Result:=BytesToStr(AsBytes,ByteSize);
end;
end;

function TQValue.GetByteSize: Integer;
begin
Result:=Length(FValue);
end;

function TQValue.GetDataType: Integer;
begin
if (FDataType<>VARTYPE_VARNAME) and (FDataType<>VARTYPE_FUNCTION) then
  Result:=FDataType
else
  Result:=AsReference.Value.DataType;
end;

function TQValue.GetIsBoolean: Boolean;
begin
Result:=(FDataType=VARTYPE_NULL);
end;

function TQValue.GetIsBytes: Boolean;
begin
Result:=(FDataType and VARTYPE_BYTES)<>0;
end;

function TQValue.GetIsDateTime: Boolean;
begin
Result:=(FDataType and VARTYPE_DATETIME_MASK)<>0;
end;

function TQValue.GetIsFloat: Boolean;
begin
Result:=(FDataType and (VARTYPE_FLOAT_MASK OR VARTYPE_BCD_MASK))<>0;
end;

function TQValue.GetIsFunction: Boolean;
begin
Result:=(FDataType=VARTYPE_FUNCTION);
end;

function TQValue.GetIsInteger: Boolean;
begin
Result:=(FDataType and VARTYPE_INTEGER_MASK)<>0;
end;

function TQValue.GetIsNull: Boolean;
begin
Result:=(FDataType=VARTYPE_NULL);
end;

function TQValue.GetIsNumeric: Boolean;
begin
Result:=(FDataType and VARTYPE_NUMERIC_MASK)<>0;
end;

function TQValue.GetIsReference: Boolean;
begin
Result:=(FDataType=VARTYPE_VARNAME) or (FDataType=VARTYPE_FUNCTION);
end;

function TQValue.GetIsString: Boolean;
begin
Result:=(FDataType=VARTYPE_STRING);
end;

function TQValue.GetIsVar: Boolean;
begin
Result:=(FDataType=VARTYPE_VARNAME);
end;

function TQValue.GetTargetValue(AVal: TQValue): TQValue;
begin
Result:=AVal;
while Result.IsReference do
  Result:=Result.AsReference.Value;
end;

function TQValue.GetText: WideString;
var
  AVal:TQValue;
  AVar:TQVar;
  AFunc:TQFunction;
  I:Integer;
  procedure FormatValueText;
  begin
  if Length(Result)>0 then
    begin
    if (AVal.DataType=VARTYPE_STRING) or ((AVal.DataType and VARTYPE_DATETIME_MASK)<>0) then
      Result:=Result+':"'+EscapeText(AVal.AsString)+'"'
    else if AVal.DataType=VARTYPE_NULL then
      Result:=Result+':null'
    else
      Result:=Result+':'+AVal.AsString;
    end
  else
    begin
    if (AVal.DataType=VARTYPE_STRING) or ((AVal.DataType and VARTYPE_DATETIME_MASK)<>0) then
      Result:='"'+EscapeText(AVal.AsString)+'"'
    else if AVal.DataType=VARTYPE_NULL then
      Result:='null'
    else
      Result:=AVal.AsString;
    end;
  end;
begin
AVal:=Self;
AVar:=nil;
while AVal.IsReference do
  begin
  AVar:=AVal.AsReference;
  AVal:=AVar.FValue;
  end;
if Assigned(AVar) and (PWideChar(AVar.Name)[0]<>'#') then//不是临时变量
  begin
  Result:=AVar.DisplayName;
//  ShowMessage(AVar.ClassName);
  if AVar is TQFunction then
    begin
    //函数
    AFunc:=AVar as TQFunction;
    Result:=Result+'(';
    if AFunc.ParamCount>0 then
      begin
      for I := 0 to AFunc.ParamCount - 1 do
        Result:=Result+AFunc.Parameters[I].Name+'='+AFunc.Parameters[I].Value.Text+',';
      SetLength(Result,Length(Result)-1);
      end;
    Result:=Result+')';
    end
  else
    FormatValueText;
  end
else
  begin
  AVal:=Self;
  Result:='';
  FormatValueText;
  end;
end;

function TQValue.Logical_And(const V2: TQValue): Boolean;
var
  AValue1,AValue2:TQValue;
begin
//逻辑与只适用于布尔值或者整型
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
Result:=False;
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  Result:=(AValue1.AsInt64<>0) and (AValue2.AsInt64<>0);
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprAnd].Text)]));
end;

function TQValue.Logical_Not: Boolean;
var
  AValue:TQValue;
begin
Result:=False;
AValue:=GetTargetValue(Self);
if AValue.IsInteger then
  Result:=(AValue.AsInt64=0)
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprNot].Text)]));
end;

function TQValue.Logical_Or(const V2: TQValue): Boolean;
var
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
Result:=False;
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  Result:=(AValue1.AsInt64<>0) or (AValue2.AsInt64<>0);
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprOr].Text)]));
end;

procedure TQValue.Math_Add(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;

  procedure DoBcdAdd;
  var
    o:TBcd;
  begin
  BcdAdd(AValue1.AsBcd,AValue2.AsBcd,o);
  RetVal.AsBcd:=o;
  end;
  procedure DoBytesAdd;
  var
    S:AnsiString;
  begin
  SetLength(S,AValue1.ByteSize+AValue2.ByteSize);
  CopyMemory(PAnsiChar(S),AValue1.AsBytes,AValue1.ByteSize);
  CopyMemory(PAnsiChar(S)+AValue1.ByteSize,AValue2.AsBytes,AValue2.ByteSize);
  RetVal.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
  end;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      begin
      if AValue1.AsBoolean or AValue2.AsBoolean then
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;
      end;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger+AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64+AValue2.AsInt64;
    VARTYPE_FLOAT:
      RetVal.AsFloat:=AValue1.AsFloat+AValue2.AsFloat;
    VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
      RetVal.AsDateTime:=AValue1.AsFloat+AValue2.AsFloat;
    VARTYPE_NUMERIC:
      DoBcdAdd;
  end;
  end
else if AValue1.IsString or AValue2.IsString  then
  RetVal.AsString:=AValue1.AsString+AValue2.AsString
else if AValue1.IsBytes or AValue2.IsBytes then
  DoBytesAdd
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprAdd].Text)]));
end;

procedure TQValue.Assign(const ASource: TQValue);
begin
FDataType:=ASource.FDataType;
FValue:=ASource.FValue;
DoChange;
end;

procedure TQValue.Bit_And(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      begin
      if AValue1.AsBoolean and AValue2.AsBoolean then
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;
      end;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger and AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64 and AValue2.AsInt64;
  end
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprBitAnd].Text)]));
end;

procedure TQValue.Math_Div(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
  procedure DoDiv;
  var
    o:TBcd;
  begin
  BcdDivide(AValue1.AsBcd,AValue2.AsBcd,o);
  RetVal.AsBcd:=o;
  end;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      if not AValue2.AsBoolean then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else if AValue1.AsBoolean then//1/1
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;//0/1
    VARTYPE_INTEGER:
      if AValue2.AsInteger=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInteger:=AValue1.AsInteger div AValue2.AsInteger;
    VARTYPE_INT64:
      if AValue2.AsInt64=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInt64:=AValue1.AsInt64 div AValue2.AsInt64;
    VARTYPE_FLOAT:
      if SameValue(AValue2.AsFloat,0) then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsFloat:=AValue1.AsFloat / AValue2.AsFloat;
    VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
      if SameValue(AValue2.AsFloat,0) then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsDateTime:=AValue1.AsFloat / AValue2.AsFloat;
    VARTYPE_NUMERIC:
      DoDiv;
  end;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprDiv].Text)]));
end;

procedure TQValue.Math_DivTrunc(const V2, RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
  procedure DoDiv;
  var
    o:TBcd;
  begin
  BcdDivide(AsBcd,V2.AsBcd,o);
  RetVal.AsBcd:=o;
  end;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if  AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      if not AValue2.AsBoolean then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else if AValue1.AsBoolean then//1/1
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;//0/1
    VARTYPE_INTEGER:
      if AValue2.AsInteger=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInteger:=AValue1.AsInteger div AValue2.AsInteger;
    VARTYPE_INT64:
      if AValue2.AsInt64=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInt64:=AValue1.AsInt64 div AValue2.AsInt64;
    VARTYPE_FLOAT,VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
      if SameValue(AValue2.AsFloat,0) then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInt64:=Trunc(AValue1.AsFloat / AValue2.AsFloat);
    VARTYPE_NUMERIC:
      DoDiv;
  end;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprDivTrunc].Text)]));
end;

procedure TQValue.Math_Mod(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      if not AValue2.AsBoolean then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else//1/1=0,0/1=0
        RetVal.AsBoolean:=False;//0/1
    VARTYPE_INTEGER:
      if AValue2.AsInteger=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInteger:=AValue1.AsInteger mod AValue2.AsInteger;
    VARTYPE_INT64:
      if AValue2.AsInt64=0 then
        ParserError(EPARSER_DIVBYZERO,EMSG_DIVBYZERO)
      else
        RetVal.AsInt64:=AValue1.AsInt64 mod AValue2.AsInt64;
  end;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprDivTrunc].Text)]));
end;

procedure TQValue.Math_Multiply(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
  procedure DoMultiply;
  var
    o:TBcd;
  begin
  BcdMultiply(AValue1.AsBcd,AValue2.AsBcd,o);
  RetVal.AsBcd:=o;
  end;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.FDataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.FDataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      if AValue1.AsBoolean and AValue2.AsBoolean then//1*1
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;//0*1,0*0
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger * AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64 * AValue2.AsInt64;
    VARTYPE_FLOAT:
      RetVal.AsFloat:=AValue1.AsFloat * AValue2.AsFloat;
    VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
      RetVal.AsDateTime:=AValue1.AsFloat * AValue2.AsFloat;
    VARTYPE_NUMERIC:
      DoMultiply;
  end;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprMul].Text)]));
end;

procedure TQValue.Bit_Not(RetVal: TQValue);
var
  AValue:TQValue;
begin
AValue:=GetTargetValue(Self);
if AValue.IsInteger then
  begin
  case AValue.DataType of
    VARTYPE_BOOLEAN:
      RetVal.AsBoolean:=not AValue.AsBoolean;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=not AValue.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=not AValue.AsInt64;
  end
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprBitNot].Text)]));
end;

procedure TQValue.Bit_Or(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      begin
      if AValue1.AsBoolean or AValue2.AsBoolean then
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;
      end;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger or AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64 or AValue2.AsInt64;
  end
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprBitOr].Text)]));
end;

procedure TQValue.Math_Power(const V2,RetVal: TQValue);
var
  AValue1,AValue2:TQValue;
  D1,D2:Double;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  D1:=AValue1.AsFloat;
  D2:=AValue2.AsFloat;
  if SameValue(D1,0) and SameValue(D2,0) then  //0**0无意义，Power函数会自己抛出异常，因此未检查
    ParserError(EPARSER_NOMEAN_VALUE,Format(EMSG_NOMEAN_VALUE,[PWideChar(Operators[oprPower].Text)]));
  RetVal.AsFloat:=Power(D1,D2);
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprPower].Text)]));
end;

procedure TQValue.Math_Sub(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
  procedure DoSub;
  var
    o:TBcd;
  begin
  BcdSubtract(AValue1.AsBcd,AValue2.AsBcd,o);
  RetVal.AsBcd:=o;
  end;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      if AValue1.AsBoolean then
        begin
        if AValue2.AsBoolean then//1-1
          RetVal.AsBoolean:=False
        else//1-0
          RetVal.AsBoolean:=True;
        end
      else if AValue2.AsBoolean then
        RetVal.AsBoolean:=True//0-1
      else
        RetVal.AsBoolean:=False;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger - AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64 - AValue2.AsInt64;
    VARTYPE_FLOAT:
      RetVal.AsFloat:=AValue1.AsFloat - AValue2.AsFloat;
    VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
      RetVal.AsDateTime:=AValue1.AsFloat - AValue2.AsFloat;
    VARTYPE_NUMERIC:
      DoSub;
  end;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprSub].Text)]));
end;

procedure TQValue.Bit_Xor(const V2,RetVal: TQValue);
var
  T1,T2:Integer;
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsNumeric and AValue2.IsNumeric then
  begin
  T1:=(AValue1.DataType and (not VARTYPE_HEX_MASK));
  T2:=(AValue2.DataType and (not VARTYPE_HEX_MASK));
  if T2>T1 then
    T1:=T2;
  case T1 of
    VARTYPE_BOOLEAN:
      begin
      if AValue1.AsBoolean then
        RetVal.AsBoolean:=not AValue2.AsBoolean
      else if AValue2.AsBoolean then
        RetVal.AsBoolean:=True
      else
        RetVal.AsBoolean:=False;
      end;
    VARTYPE_INTEGER:
      RetVal.AsInteger:=AValue1.AsInteger xor AValue2.AsInteger;
    VARTYPE_INT64:
      RetVal.AsInt64:=AValue1.AsInt64 xor AValue2.AsInt64;
  end
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprBitXor].Text)]));
end;

procedure TQValue.SetAsBcd(const Value: TBcd);
begin
DoBeforeChange;
FDataType:=VARTYPE_NUMERIC;
SetLength(FValue,SizeOf(TBcd));
CopyMemory(PAnsiChar(FValue),@Value,SizeOf(TBcd));
DoChange;
end;

procedure TQValue.SetAsBoolean(const Value: Boolean);
begin
DoBeforeChange;
FDataType:=VARTYPE_BOOLEAN;
SetLength(FValue,SizeOf(Boolean));
PBoolean(PAnsiChar(FValue))^:=Value;
DoChange;
end;

procedure TQValue.SetAsBytes(ABytes: PByte; ALength: Integer);
begin
DoBeforeChange;
FDataType:=VARTYPE_BYTES;
SetLength(FValue,ALength);
CopyMemory(PAnsiChar(FValue),ABytes,ALength);
DoChange;
end;

procedure TQValue.SetAsDateTime(const Value: TDateTime);
var
  ADate:Integer;
  ATime:Double;
begin
DoBeforeChange;
SetLength(FValue,SizeOf(Double));
PDouble(PAnsiChar(FValue))^:=Value;
ADate:=Trunc(Value);
ATime:=Value-ADate;
SetLength(FValue,SizeOf(Double));
if SameValue(ATime,0) then//Time=0,us
  FDataType:=VARTYPE_DATE
else if ADate=0 then
  FDataType:=VARTYPE_TIME
else
  FDataType:=VARTYPE_DATETIME;
DoChange;
end;

procedure TQValue.SetAsFloat(const Value: Double);
begin
DoBeforeChange;
FDataType:=VARTYPE_FLOAT;
SetLength(FValue,SizeOf(Double));
PDouble(PAnsiChar(FValue))^:=Value;
DoChange;
end;

procedure TQValue.SetAsInt64(const Value: Int64);
begin
DoBeforeChange;
FDataType:=VARTYPE_INT64;
SetLength(FValue,SizeOf(Int64));
PInt64(PAnsiChar(FValue))^:=Value;
DoChange;
end;

procedure TQValue.SetAsInteger(const Value: Integer);
begin
DoBeforeChange;
FDataType:=VARTYPE_INTEGER;
SetLength(FValue,SizeOf(Integer));
PInteger(PAnsiChar(FValue))^:=Value;
DoChange;
end;

procedure TQValue.SetAsReference(const Value: TQVar);
begin
DoBeforeChange;
if Value is TQFunction then
  FDataType:=VARTYPE_FUNCTION
else
  FDataType:=VARTYPE_VARNAME;
SetLength(FValue,SizeOf(Pointer));
PPointer(PAnsiChar(FValue))^:=Value;
DoChange;
end;

procedure TQValue.SetAsString(const Value: WideString);
var
  L:Integer;
begin
DoBeforeChange;
FDataType:=VARTYPE_STRING;
L:=Length(Value) shl 1;
SetLength(FValue,L);
CopyMemory(PAnsiChar(FValue),PWideChar(Value),L);
DoChange;
end;

procedure TQValue.ShiftLeft(const V2, RetVal: TQValue);
var
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  if AValue1.FDataType=VARTYPE_INT64 then
    RetVal.AsInt64:=AValue1.AsInt64 shl AValue2.AsInteger
  else
    RetVal.AsInteger:=AValue1.AsInteger shl AValue2.AsInteger;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprLShift].Text)]));
end;

procedure TQValue.ShiftRight(const V2, RetVal: TQValue);
var
  AValue1,AValue2:TQValue;
begin
AValue1:=GetTargetValue(Self);
AValue2:=GetTargetValue(V2);
if AValue1.IsInteger and AValue2.IsInteger then
  begin
  if AValue1.FDataType=VARTYPE_INT64 then
    RetVal.AsInt64:=AValue1.AsInt64 shr AValue2.AsInteger
  else
    RetVal.AsInteger:=AValue1.AsInteger shr AValue2.AsInteger;
  end
else
  ParserError(EPARSER_UNSUPPORT_OPRERATOR,Format(EMSG_UNSUPPORT_OPERATOR,[PWideChar(Operators[oprRShift].Text)]));
end;

{ TQVar }

function TQVar.Add(AName: WideString;AMutable:Boolean): TQVar;
var
  AIndex:Integer;
begin
AName:=Trim(AName);
ValidateName(AName);
if not InternalFind(AName,AIndex) then
  begin
  if Self is TQExprParser then
    Result:=TQVar.Create(Self as TQExprParser)
  else
    Result:=TQVar.Create(Owner);
  Result.FName:=AName;
  Result.FParent:=Self;
  Result.Mutable:=AMutable;
  FItems.Insert(AIndex,Result);
  end
else
  begin
  Result:=FItems[AIndex];
  Result.Mutable:=AMutable;
  end;
end;

procedure TQVar.Add(AVar: TQVar);
var
  AIdx:Integer;
begin
if not InternalFind(AVar.Name,AIdx) then
  begin
  if Assigned(AVar.Parent) then
    AVar.Parent.FItems.Remove(AVar);
  AVar.FParent:=Self;
  FItems.Insert(AIdx,AVar);
  end
else
  ParserError(EPARSER_NAME_EXISTS,Format(EMSG_NAME_EXISTS,[PWideChar(AVar.Name)]));
end;

function TQVar.AddFunction(AName: WideString;
  AHandler: TNotifyEvent; AFixedParams: WideString;
  AVarParams: Boolean): TQFunction;
var
  APos:Integer;
  AParent:TQVar;
  APath:WideString;
  AOptParamFound:Boolean;
  function TrimName(S:WideString):WideString;
  var
    ps,pe:PWideChar;
  begin
  ps:=PWideChar(S);
  pe:=ps+Length(S)-1;
  while CharIn(ps^,#9#10#13' ') do
    Inc(ps);
  while (pe>ps) and CharIn(pe^,#9#10#13' ') do
    Dec(pe);
  if pe>ps then
    Result:=System.Copy(ps,0,pe-ps+1)
  else
    SetLength(Result,0);
  end;
begin
APath:=ExtractVarPath(AName);
if APath<>'' then
  begin
  AParent:=ForcePath(APath);
  AName:=ExtractVarName(AName);
  end
else
  AParent:=Self;
if Self is TQExprParser then
  Result:=TQFunction.Create(Self as TQExprParser)
else
  Result:=TQFunction.Create(Owner);
Result.OnGetValue:=AHandler;
Result.Name:=AName;
AOptParamFound:=False;
while Length(AFixedParams)>0 do
  begin
  APos:=Pos(',',AFixedParams);
  if APos=0 then
    begin
    AName:=TrimName(AFixedParams);
    SetLength(AFixedParams,0);
    end
  else
    begin
    AName:=TrimName(System.Copy(AFixedParams,1,APos-1));
    System.Delete(AFixedParams,1,APos);
    end;
  if (PWideChar(AName)^='[') and (PWideChar(AName)[Length(AName)-1]=']') then
    begin
    Result.AddParam(System.Copy(AName,2,Length(AName)-2),False);
    AOptParamFound:=True;
    end
  else
    Result.AddParam(AName,True);
  end;
if AOptParamFound then
  begin
  Result.MaxParamCount:=Result.ParamCount;
  Result.VarParams:=True;
  end
else
  Result.VarParams:=AVarParams;
AParent.Add(Result);
end;
procedure TQVar.Assign(ASource: TQVar);
var
  AVar:TQVar;
  I:Integer;
begin
Clear;
FData:=ASource.FData;
if ASource.FValue.IsReference then
  begin
  AVar:=Owner.VarByPath(ASource.FValue.AsReference.Path);
  if Assigned(AVar) then
    Value.AsReference:=AVar
  else
    ParserError(EPARSER_BAD_NAME,Format(EMSG_BAD_NAME,[PWideChar(ASource.FValue.AsReference.Path)]));
  end
else
  begin
  FValue.Assign(ASource.FValue);
  for I := 0 to ASource.Count - 1 do
    Add(ASource.Items[I].Copy(Owner));
  end;
FOnGetValue:=ASource.FOnGetValue;
FOnValueChange:=ASource.FOnValueChange;
end;

procedure TQVar.Clear(AFreeNeeded:Boolean);
var
  I:Integer;
begin
if AFreeNeeded then
  begin
  for I := 0 to FItems.Count - 1 do
    Items[I].Free;
  end;
FItems.Clear;
FValue.Clear;
end;

function TQVar.Copy(AOwner:TQExprParser): TQVar;
begin
Result:=TQVarClass(ClassType).Create(AOwner);
Result.Assign(Self);
Result.FName:=FName;
Result.FDisplayName:=DisplayName;
end;

constructor TQVar.Create;
begin
inherited;
raise Exception.Create('NoImpl');
FItems:=TList.Create;
FOwner:=nil;
FValue:=TQValue.Create;
FValue.OnChange:=DoValueChange;
end;

constructor TQVar.Create(AOwner:TQExprParser);
begin
inherited Create;
FItems:=TList.Create;
FOwner:=AOwner;
FValue:=TQValue.Create;
FValue.OnChange:=DoValueChange;
end;

procedure TQVar.Delete(AIndex: Integer;AFreeNeeded:Boolean);
begin
if AFreeNeeded then
  Items[AIndex].Free;
FItems.Delete(AIndex);
end;

procedure TQVar.Delete(AName: WideString;AFreeNeeded:Boolean);
var
  AIndex:Integer;
begin
if InternalFind(AName,AIndex) then
  Delete(AIndex,AFreeNeeded);
end;

destructor TQVar.Destroy;
begin
Clear;
FItems.Free;
FValue.Free;
inherited;
end;

procedure TQVar.DoValueChange(ASender: TObject);
begin
if Assigned(FOnValueChange) then
  FOnValueChange(Self);
end;

procedure TQVar.DoValueChanging(ASender: TObject);
begin
if ReadOnly then
  ParserError(EPARSER_CONST_READONLY,Format(EMSG_CONST_READONLY,[PWideChar(DisplayName)]))
end;

function TQVar.Enum(AList: TList;AOptions:TQVarEnumOptions): Integer;
var
  AVar:TQVar;
  I:Integer;
  procedure AddChildren(AParent:TQVar);
  var
    Accept:Boolean;
    I: Integer;
  begin
  for I := 0 to AParent.Count-1 do
    begin
    AVar:=AParent.Items[I];
    Accept:=True;
    if PWideChar(AVar.Name)[0]='#' then//临时变量
      begin
      if not (veoTemp in AOptions) then
        Accept:=False
      end;
    if AVar is TQFunction then
      begin
      if not (veoFunction in AOptions) then
        Accept:=False
      end
    else if AVar is TQVar then
      begin
      if not (veoVariant in AOptions) then
        Accept:=False;
      end;
    if Accept then
      begin
      AList.Add(AVar);
      Inc(Result);
      end;
    if (AVar.Count>0) and (veoNest in AOptions) then
      AddChildren(AVar);
    end;
  end;
begin
Result:=0;
if Assigned(AList) then
  AddChildren(Self);
end;

function TQVar.Find(AName: WideString): TQVar;
var
  AIndex:Integer;
begin
if InternalFind(AName,AIndex) then
  Result:=Items[AIndex]
else
  Result:=nil;
end;

function TQVar.FindByPath(APath: WideString): TQVar;
var
  AParent:TQVar;
  AName:WideString;
  APos:Integer;
begin
AParent:=Self;
repeat
  APos:=Pos('.',APath);
  if APos<>0 then
    begin
    AName:=System.Copy(APath,1,APos-1);
    APath:=System.Copy(APath,APos+1,MaxInt);
    AParent:=AParent.Find(AName);
    end
  else
    begin
    AParent:=AParent.Find(APath);
    end;
until (APos=0) or (not Assigned(AParent));
Result:=AParent;
end;

function TQVar.ForcePath(APath: WideString): TQVar;
var
  AParent:TQVar;
  AName:WideString;
  APos:Integer;
begin
AParent:=Self;
repeat
  APos:=Pos('.',APath);
  if APos<>0 then
    begin
    AName:=System.Copy(APath,1,APos-1);
    APath:=System.Copy(APath,APos+1,MaxInt);
    Result:=AParent.Find(AName);
    end
  else
    begin
    AName:=APath;
    Result:=AParent.Find(AName);
    end;
  if not Assigned(Result) then
    Result:=AParent.Add(AName);
  AParent:=Result;
until (APos=0);
end;

function TQVar.ForEach(AEnumProc: TQVarEnumProc; AOptions:TQVarEnumOptions;AParam: Integer): Integer;
var
  AList:TList;
  I:Integer;
begin
if Assigned(AEnumProc) then
  begin
  AList:=TList.Create;
  try
    Result:=Enum(AList,AOptions);
    for I := 0 to AList.Count-1 do
      AEnumProc(Self,AList[I],AParam);
  finally
    AList.Free;
  end;
  end
else
  Result:=0;
end;

function TQVar.GetCount: Integer;
begin
Result:=FItems.Count;
end;

function TQVar.GetDisplayName: WideString;
var
  APos:Integer;
begin
if Length(FDisplayName)>0 then
  Result:=FDisplayName
else if FValue.IsReference then
  Result:=FValue.AsReference.DisplayName
else
  begin
  APos:=Pos('#',FName);
  if APos<>0 then
    Result:=System.Copy(FName,1,APos-1)
  else
    Result:=FName;
  end;
end;

function TQVar.GetIndex: Integer;
begin
if Assigned(FParent) then
  Result:=FParent.IndexOf(FName)
else
  Result:=-1;
end;

function TQVar.GetItems(AIndex:Integer): TQVar;
begin
Result:=FItems[AIndex];
end;

function TQVar.GetParser: TQExprParser;
var
  AParent:TQVar;
begin
AParent:=Self;
Result:=nil;
while Assigned(AParent) do
  begin
  if AParent is TQExprParser then
    begin
    Result:=AParent as TQExprParser;
    Break;
    end;
  AParent:=AParent.Parent;
  end;
end;

function TQVar.GetPath: WideString;
var
  AParent:TQVar;
begin
Result:=FName;
AParent:=FParent;
while Assigned(AParent) do
  begin
  if Length(AParent.FName)>0 then
    Result:=AParent.FName+'.'+Result;
  AParent:=AParent.Parent;
  end;
end;

function TQVar.GetText: WideString;
var
  I:Integer;
  AItem:TQVar;
  AFunc:TQFunction;
begin
if Self is TQFunction then
  begin
  AFunc:=Self as TQFunction;
  Result:=DisplayName+'(';
  if AFunc.ParamCount>0 then
    begin
    for I := 0 to AFunc.ParamCount - 1 do
      Result:=Result+AFunc.Parameters[I].Name+'='+AFunc.Parameters[I].Value.Text+',';
    SetLength(Result,Length(Result)-1);
    end;
  Result:=Result+')';
  end
else
  begin
  Result:=FName;
  if Length(Result)>0 then
    Result:=Result+'=';
  if Count>1 then
    begin
    Result:=Result+'{'#13#10;
    for I := 0 to Count - 1 do
      begin
      AItem:=Items[I];
      if AItem is TQFunction then
        Result:=Result+AItem.Text+#13#10
      else
        Result:=Result+AItem.Text+';'#13#10;
      end;
    if PWideChar(Result)[Length(Result)-1]=',' then
      SetLength(Result,Length(Result)-1);
    Result:=Result+'}';
    end
  else if not Assigned(FOnGetValue) then
    begin
    if FValue.IsReference then
      Result:=Result+FValue.AsReference.Text
    else if ((FValue.DataType and VARTYPE_DATETIME_MASK)<>0) or (FValue.DataType=VARTYPE_STRING) then
      Result:=Result+EscapeText(FValue.AsString)
    else
      Result:=Result+FValue.AsString;
    end
  end;
end;

function TQVar.GetValue: TQValue;
begin
if (Assigned(Owner) and (FFetchIP<>Owner.FIP)) or Mutable then//如果值是善变的，则值在每次获取时执行事件来获取最新值，否则使用缓存值
  begin
  if Assigned(Owner) then
    FFetchIP:=Owner.FIP;
  if Assigned(FOnGetValue) and (not FOnFetch) then
    begin
    FOnFetch:=True;
    try
        FOnGetValue(Self);
    finally
      FOnFetch:=False;
    end;
    end;
  end;
Result:=FValue;
end;

function TQVar.IndexOf(AName: WideString): Integer;
begin
if not InternalFind(AName,Result) then
  Result:=-1;
end;

function TQVar.IndexOf(const AVar: TQVar): Integer;
begin
Result:=IndexOf(AVar.Name);
end;

function TQVar.InternalFind(const AName: WideString;
  var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
Result := False;
L := 0;
H := Count - 1;
while L <= H do
begin
  I := (L + H) shr 1;
  C := CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(Items[I].Name),-1,PWideChar(AName),-1)-CSTR_EQUAL;
  if C < 0 then L := I + 1 else
  begin
    H := I - 1;
    if C = 0 then
    begin
      Result := True;
      L := I;
    end;
  end;
end;
aIndex := L;
end;

procedure TQVar.SetDisplayName(const Value: WideString);
begin
if Value<>FDisplayName then
  FDisplayName:=Value;
end;

procedure TQVar.SetName(Value: WideString);
var
  AIndex:Integer;
begin
Value:=Trim(Value);
ValidateName(Value);
if FName<>Value then
  begin
  if Assigned(FParent) then
    begin
    if FParent.InternalFind(Value,AIndex) then
      ParserError(EPARSER_NAME_EXISTS,Format(EMSG_NAME_EXISTS,[PWideChar(Value)]))
    else
      FParent.FItems.Move(FParent.FItems.IndexOf(Self),AIndex);
    end;
  FName := Value;
  end;
end;

procedure TQVar.ValidateName(S:WideString);
begin
if Length(S)=0 then
  ParserError(EPARSER_NAME_EMPTY,EMSG_NAME_EMPTY);
if Pos('.',S)<>0 then
  ParserError(EPARSER_NAME_DOT,EMSG_NAME_DOT);
end;

{ TQFunction }

function TQFunction.AddParam(const AName: WideString;AFixed:Boolean):TQParameter;
begin
if AFixed then
  begin
  if ParamCount>0 then
    begin
    if not Parameters[ParamCount-1].Fixed then
      ParserError(EPARSER_BAD_PARAM_ORDER,EMSG_BAD_PARAM_ORDER);
    end;
  Inc(FFixedParamCount);
  end;
Result:=TQParameter.Create(Self);
Result.FName:=AName;
Result.FFixed:=AFixed;
Result.FIndex:=FParams.Add(Result);
end;

procedure TQFunction.Assign(AVar: TQVar);
var
  I:Integer;
  AFunc:TQFunction;
  AParam:TQParameter;
begin
inherited;
if AVar is TQFunction then
  begin
  AFunc:=AVar as TQFunction;
  VarParams:=AFunc.VarParams;
  ClearParams;
  for I := 0 to AFunc.ParamCount - 1 do
    begin
    AParam:=AFunc.Parameters[I];
    AddParam(AParam.Name,AParam.Fixed);
    end;
  end;
end;

procedure TQFunction.Call;
begin
GetValue;
end;

procedure TQFunction.ClearParams;
var
  I:Integer;
begin
for I := 0 to FParams.Count - 1 do
  Parameters[I].Free;
FParams.Clear;
FMaxParamCount:=MaxInt;
FFixedParamCount:=0;
end;

procedure TQFunction.ClearVarParams;
var
  I:Integer;
begin
for I := FParams.Count-1 downto 0 do
  begin
  if not Parameters[I].Fixed then
    begin
    Parameters[I].Free;
    FParams.Delete(I);
    end
  else
    Break;
  end;
end;

constructor TQFunction.Create(AOwner:TQExprParser);
begin
inherited Create(AOwner);
FParams:=TList.Create;
FFixedParamCount:=0;
FMaxParamCount:=MaxInt;
end;

destructor TQFunction.Destroy;
begin
ClearParams;
FParams.Free;
inherited;
end;

function TQFunction.FindParam(const AName: WideString): TQParameter;
var
  I:Integer;
  AItem:TQParameter;
begin
Result:=nil;
for I := 0 to ParamCount - 1 do
  begin
  AItem:=Parameters[I];
  if CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AItem.Name),-1,PWideChar(AName),-1)=CSTR_EQUAL then
    begin
    Result:=AItem;
    Break;
    end;
  end;
end;

function TQFunction.GetCallText: WideString;
var
  I:Integer;
  AParam:TQParameter;
begin
I:=Pos('#',FName);
if I<>0 then
  Result:=System.Copy(FName,1,I-1)+'('
else
  Result:=Name+'(';
if ParamCount>0 then
  begin
  for I := 0 to ParamCount - 1 do
    begin
    AParam:=Parameters[I];
    if AParam.Value.IsReference then
      begin
      if AParam.Value.AsReference is TQFunction then
        Result:=Result+(AParam.Value.AsReference as TQFunction).CallText+','
      else
        Result:=Result+AParam.Value.AsReference.Name+',';
      end
    else
      begin
      case AParam.Value.DataType of
        VARTYPE_NULL:
          Result:=Result+'NULL,';
        VARTYPE_INTEGER,VARTYPE_INT64,VARTYPE_FLOAT:
          Result:=Result+AParam.Value.AsString+','
        else
          Result:=Result+'"'+EscapeText(AParam.Value.AsString)+'",';
      end;
      end;
    end;
  SetLength(Result,Length(Result)-1);
  end;
Result:=Result+')';
end;

function TQFunction.GetDeclareText: WideString;
var
  I,C:Integer;
begin
I:=Pos('#',FName);
if I<>0 then
  Result:=System.Copy(FName,1,I-1)+'('
else
  Result:=FName+'(';
C:=ParamCount;
if C>0 then
  begin
  for I := 0 to ParamCount - 1 do
    Result:=Result+Parameters[I].Name+',';
  SetLength(Result,Length(Result)-1);
  end;
if VarParams then
  begin
  if C>0 then
    Result:=Result+',...'
  else
    Result:=Result+'...';
  end;
Result:=Result+');';
end;

function TQFunction.GetParamCount: Integer;
begin
Result:=FParams.Count;
end;

function TQFunction.GetParameters(AIndex: Integer): TQParameter;
begin
Result:=FParams[AIndex];
end;


function TQFunction.ParamByName(const AName: WideString): TQParameter;
begin
Result:=FindParam(AName);
if not Assigned(Result) then
  ParserError(EPARSER_PARAM_MISSED,Format(EMSG_PARAM_MISSED,[PWideChar(AName)]));
end;

{ TQExprParser }

function TQExprParser.AddFunction(const AName: WideString;
  AOnExecute: TNotifyEvent):TQFunction;
var
  AIndex:Integer;
  AItem:TQVar;
begin
if InternalFind(AName,AIndex) then
  begin
  AItem:=Items[AIndex];
  if AItem is TQFunction then
    begin
    Result:=AItem as TQFunction;
    Result.OnGetValue:=AOnExecute;
    end
  else
    begin
    Result:=nil;
    ParserError(EPARSER_NAME_EXISTS,Format(EMSG_NAME_EXISTS,[PWideChar(AName)]));
    end;
  end
else
  begin
  Result:=TQFunction.Create(Self);
  Result.OnGetValue:=AOnExecute;
  Result.FName:=AName;
  Result.FParent:=Self;
  FItems.Insert(AIndex,Result);
  end;
end;

function TQExprParser.AddVar(const AName: WideString;
  AOnExecute: TNotifyEvent): TQVar;
var
  AIndex:Integer;
  AItem:TQVar;
begin
if InternalFind(AName,AIndex) then
  begin
  AItem:=Items[AIndex];
  if not (AItem is TQFunction) then
    begin
    Result:=AItem;
    Result.OnGetValue:=AOnExecute
    end
  else
    begin
    Result:=nil;
    ParserError(EPARSER_NAME_EXISTS,Format(EMSG_NAME_EXISTS,[PWideChar(AName)]));
    end;
  end
else
  begin
  Result:=TQVar.Create(Self);
  Result.OnGetValue:=AOnExecute;
  Result.FName:=AName;
  Result.FParent:=Self;
  FItems.Insert(AIndex,Result);
  end;
end;

function TQExprParser.AddConst(const AName: WideString;
  const AVal: Int64): TQVar;
begin
Result:=Add(AName);
Result.Value.AsInt64:=AVal;
Result.ReadOnly:=True;
end;

function TQExprParser.AddConst(const AName: WideString;
  const AVal: Integer): TQVar;
begin
Result:=Add(AName);
Result.Value.AsInteger:=AVal;
Result.ReadOnly:=True;
end;

function TQExprParser.AddConst(const AName: WideString;
  const AVal: Double): TQVar;
begin
Result:=Add(AName);
Result.Value.AsFloat:=AVal;
Result.ReadOnly:=True;
end;

procedure TQExprParser.AddAlias(const ANewName: WideString; ARef: TQVar);
begin
FAliases.Add(ANewName).Value.AsReference:=ARef;
end;

function TQExprParser.AddConst(const AName, AVal: WideString): TQVar;
begin
Result:=Add(AName);
Result.Value.AsString:=AVal;
Result.ReadOnly:=True;
end;

function TQExprParser.AddConst(const AName: WideString;
  const AVal: TDateTime): TQVar;
begin
Result:=Add(AName);
Result.Value.AsDateTime:=AVal;
Result.ReadOnly:=True;
end;

procedure TQExprParser.Assign(ASource: TQExprParser);
var
  I:Integer;
  AVar:TQVar;
begin
FAssigning:=True;
try
  Clear;
  for I := 0 to ASource.Count - 1 do
    begin
    AVar:=ASource.Items[I];
    if (AVar<>ASource.FLocals) and (AVar<>ASource.FLocals) then
      Add(AVar.Copy(Self));
    end;
  FLocals.Assign(ASource.Locals);
  FAliases.Assign(ASource.FAliases);
  FStatements.Assign(ASource.FStatements);
  FOnVarNeeded:=ASource.OnVarNeeded;
  FOnFunctionNeeded:=ASource.OnFunctionNeeded;
  FAborted:=False;
  FVarIndex:=ASource.FVarIndex;
  FExecuted:=False;
  FText:=ASource.Text;
  FData:=ASource.FData;
  FOnGetValue:=ASource.FOnGetValue;
  FOnValueChange:=ASource.FOnValueChange;
finally
  FAssigning:=False;
end;
end;

procedure TQExprParser.BeginRegister;
begin

end;

procedure TQExprParser.Calc;
var
  I:Integer;
  ALastVal,APriorVal:TQValue;
  AStatement:TQExprStatement;
  procedure ClearVarStates(AParent:TQVar);
  var
    J:Integer;
  begin
//  OutputDebugStringW(PWideChar(AParent.Name+' State reseted'));
  AParent.FFetchIP:=0;
  for J := 0 to AParent.Count - 1 do
    ClearVarStates(AParent.Items[J]);
  end;
begin
if Assigned(BeforeExecute) then
  BeforeExecute(Self);
FResult.Clear();
ClearVarStates(FLocals);
for I := 0 to FStatements.Count - 1 do
  ClearVarStates(FStatements.Items[I].Result);
FLineNo:=0;
FIP:=1;
APriorVal:=nil;
while (FLineNo<FStatements.Count) and (not Aborted) do
  begin
  AStatement:=FStatements.Items[FLineNo];
  ALastVal:=AStatement.Result.Value;
  Inc(FLineNo);
  Inc(FIP);
  FBreakCurrent:=False;
  if Assigned(BeforeExecStatement) then
    BeforeExecStatement(Self,AStatement);
  while ALastVal.IsReference and (not FBreakCurrent) do
    ALastVal:=ALastVal.AsReference.Value;
  if Assigned(AfterExecStatement) then
    AfterExecStatement(Self,AStatement);
  if not ALastVal.IsNull then
    APriorVal:=ALastVal;
  if FBreakCurrent then
    Break;
  end;
if Assigned(APriorVal) then
  FResult.FValue.Assign(APriorVal);
FExecuted:=True;
if Assigned(AfterExecute) then
  AfterExecute(Self);
end;

procedure TQExprParser.Clear(AFreeNeeded:Boolean);
begin
if Assigned(FStatements) then
  begin
  FLocals.Clear();
  FResult.Clear();
  FValue.Clear();
  FAliases.Clear();
  FLineNo:=0;
  FExecuted:=False;
  FStatements.Clear;
  end;
end;

constructor TQExprParser.Create;
begin
inherited Create(nil);
FResult:=Add('Result');
FLocals:=Add('Locals');
FAliases:=Add('Aliases');
FStatements:=TQExprStatement.Create(Self);
end;

destructor TQExprParser.Destroy;
begin
Clear;
inherited Clear;
FStatements.Free;
FStatements:=nil;
inherited;
end;

procedure TQExprParser.DoVarAssigned(AVar: TQVar);
begin
if Assigned(FAssignHelper) then
  begin
  FAssignHelper(Self,AVar);
  FAssignHelper:=nil;
  end;
end;

procedure TQExprParser.EndRegister;
begin

end;

function TQExprParser.FunctionByName(const AName: WideString): TQFunction;
var
  AVar:TQVar;
  AParent:TQVar;
begin
AParent:=FLocals;
while Assigned(AParent) do
  begin
  AVar:=AParent.FindByPath(AName);
  if not Assigned(AVar) then
    AParent:=AParent.Parent
  else
    begin
    if AVar is TQFunction then
      Break
    else
      begin
      AParent:=AParent.Parent;
      AVar:=nil;
      end;
    end;
  end;
if not Assigned(AVar) then
  begin
  AVar:=FAliases.FindByPath(AName);
  if Assigned(AVar) then
    begin
    AVar:=AVar.Value.AsReference;
    if not (AVar is TQFunction) then
      AVar:=nil;
    end
  else if Self<>QExpGlobal then
    AVar:=QExpGlobal.FunctionByName(AName);
  if not Assigned(AVar) then
    begin
    if Assigned(FOnFunctionNeeded) then
      FOnFunctionNeeded(Self,AName,AVar);
    end;
  end;
if Assigned(AVar) then
  Result:=AVar as TQFunction
else
  Result:=nil
end;

function TQExprParser.GetCompiledText: WideString;
var
  I:Cardinal;
begin
Result:='';
for I := 0 to FStatements.Count - 1 do
  begin
  if I=FLineNo then
    Result:=Result+'>>'+Format('%.4d',[I])+':'+FStatements.Items[I].Result.Text+#13#10
  else
    Result:=Result+'  '+Format('%.4d',[I])+':'+FStatements.Items[I].Result.Text+#13#10;
  end;
end;

function TQExprParser.GetResult: TQVar;
begin
if not FExecuted then
  Calc;
Result:=FResult;
end;

function TQExprParser.GetValue: TQValue;
begin
Result:=GetResult.Value;
end;

function TQExprParser.NextTempName(ALeader: WideString): WideString;
begin
Result:='#'+ALeader+'_'+IntToStr(FVarIndex);
Inc(FVarIndex);
end;

function TQExprParser.OperatorType(var p: PWideChar): TQExprOperator;
begin
Result:=oprNone;
case p^ of
  '+':
    begin
    if p[1]='+' then
      begin
      Inc(p,2);
      Result:=oprInc;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfAdd;
      end
    else
      begin
      Inc(p);
      Result:=oprAdd;
      end;
    end;
  '-':
    begin
    if p[1]='-' then
      begin
      Inc(p,2);
      Result:=oprDec;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfSub;
      end
    else
      begin
      Inc(p);
      Result:=oprSub;
      end;
    end;
  '*':
    begin
    if p[1]='*' then
      begin
      Inc(p,2);
      Result:=oprPower;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfMul;
      end
    else
      begin
      Inc(p);
      Result:=oprMul;
      end;
    end;
  '/':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfDiv;
      end
    else if p[1]='/' then
      begin
      Inc(p,2);
      Result:=oprLineComment;
      end
    else if p[1]='*' then
      begin
      Inc(p,2);
      Result:=oprBlockComment;
      end
    else
      begin
      Inc(p);
      Result:=oprDiv;
      end;
    end;
  '\':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfDivTrunc;
      end
    else
      begin
      Inc(p);
      Result:=oprDivTrunc;
      end;
    end;
  '%':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfMod;
      end
    else
      begin
      Inc(p);
      Result:=oprMod;
      end;
    end;
  '&':
    begin
    if p[1]='&' then
      begin
      Inc(p,2);
      Result:=oprAnd;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfBitAnd;
      end
    else
      begin
      Inc(p);
      Result:=oprBitAnd;
      end;
    end;
  '|':
    begin
    if p[1]='|' then
      begin
      Inc(p,2);
      Result:=oprOr;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfBitOr;
      end
    else
      begin
      Inc(p);
      Result:=oprBitOr;
      end;
    end;
  '^':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprSelfBitXor;
      end
    else
      begin
      Inc(p,1);
      Result:=oprBitXor;
      end;
    end;
  '~':
    begin
    Inc(p);
    Result:=oprBitNot;
    end;
  '!':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprNotEqual;
      end
    else
      begin
      Inc(p);
      Result:=oprNot;
      end;
    end;
  '<':
    begin
    if p[1]='<' then
      begin
      if p[2]='=' then//<<=
        begin
        Inc(p,3);
        Result:=oprSelfLShift
        end
      else
        begin
        Inc(p,2);
        Result:=oprLShift;
        end;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprLessThanEqual;
      end
    else if p[1]='>' then
      begin
      Inc(p,2);
      Result:=oprNotEqual;
      end
    else
      begin
      Inc(p);
      Result:=oprLessThan;
      end;
    end;
  '>':
    begin
    if p[1]='>' then
      begin
      if p[2]='=' then
        begin
        Inc(p,3);
        Result:=oprSelfRShift;
        end
      else
        begin
        Inc(p,2);
        Result:=oprRShift;
        end;
      end
    else if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprGreatThanEqual;
      end
    else
      begin
      Inc(p);
      Result:=oprGreatThan;
      end;
    end;
  '=':
    begin
    if p[1]='=' then
      begin
      Inc(p,2);
      Result:=oprEqual;
      end
    else
      begin
      Inc(p);
      Result:=oprAssign;
      end;
    end;
  '(':
    begin
    Inc(p);
    Result:=oprBlockStart;
    end;
  ')':
    begin
    Result:=oprBlockEnd;
    Inc(p);
    end;
  ';':
    begin
    Result:=oprStatementEnd;
    Inc(p);
    end;
  ',':
    begin
    Result:=oprComma;
    Inc(p);
    end;
  '{':
    Begin
    Result:=oprBraceStart;
    Inc(p);
    End;
  '}':
    begin
    Result:=oprBraceEnd;
    Inc(p);
    end;
end;
while CharIn(p^ ,#9#10#13' ') do
  Inc(p);
end;

procedure TQExprParser.Parse(const S: WideString);
var
  p:PWideChar;
  AStatement:TQExprStatement;
begin
if FText<>S then
  begin
  FText:=S;
  p:=PWideChar(FText);
  FParsingText:=p;
  try
    Clear;
    while p^<>#0 do
      begin
      AStatement:=FStatements.Add(FLineNo,p-FParsingText);
      AStatement.Parse(p);
      if p^=';' then
        Inc(p);
      while CharIn(p^,#9#10#13' ') do
        Inc(p);
      Inc(FLineNo);
      end;
  finally
    FParsingText:=nil;
  end;
  end;
end;

procedure TQExprParser.RegisterFunctions(AFunctions: TQFunctions);
begin
AFunctions.Register(Self);
end;

procedure TQExprParser.SetAssignHelper(AHelper: TQExprAssignHelper);
begin
FAssignHelper:=AHelper;
end;

function TQExprParser.VarByName(const AName: WideString): TQVar;
var
  AParent:TQVar;
begin
AParent:=FLocals;
while Assigned(AParent) do
  begin
  Result:=AParent.FindByPath(AName);
  if not Assigned(Result) then
    AParent:=AParent.Parent
  else if Result is TQFunction then
    begin
    Result:=nil;
    AParent:=AParent.Parent
    end
  else
    Break;
  end;
if not Assigned(Result) then
  begin
  if Self<>QExpGlobal then
    Result:=QExpGlobal.VarByName(AName);
  if (not Assigned(Result)) and Assigned(FOnVarNeeded) then
    FOnVarNeeded(Self,AName,Result);
  if Assigned(Result) then
    begin
    if Result is TQFunction then
      Result:=nil;
    end;
  end
end;

function TQExprParser.VarByPath(AName: WideString): TQVar;
var
  AParent:TQVar;
begin
if Assigned(FLocals) then
  AParent:=FLocals
else
  AParent:=Self;
while Assigned(AParent) do
  begin
  Result:=AParent.FindByPath(AName);
  if not Assigned(Result) then
    AParent:=AParent.Parent
  else
    Break;
  end;
if not Assigned(Result) then
  begin
  if Self<>QExpGlobal then
    Result:=QExpGlobal.VarByName(AName);
  if (not Assigned(Result)) and Assigned(FOnVarNeeded) then
    FOnVarNeeded(Self,AName,Result);
  end;
end;

{ TDateFunctions }

constructor TDateFunctions.Create;
begin
inherited;
end;

function TDateFunctions.DatePartOfName(const AName: WideString): TDatePart;
begin
Result:=dpNone;
if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'year',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'y',-1)=CSTR_EQUAL)   then
  Result:=dpYear
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'month',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'m',-1)=CSTR_EQUAL)   then
  Result:=dpMonth
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'day',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'d',-1)=CSTR_EQUAL)   then
  Result:=dpDay
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'hour',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'h',-1)=CSTR_EQUAL)   then
  Result:=dpHour
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'minute',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'n',-1)=CSTR_EQUAL)   then
  Result:=dpMinute
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'second',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'s',-1)=CSTR_EQUAL)   then
  Result:=dpSecond
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'millsecond',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'ms',-1)=CSTR_EQUAL)   then
  Result:=dpMSecond;
end;

function TDateFunctions.DateRangeOfName(const AName: WideString): TDateRange;
begin
Result:=drNone;
if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'century',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'cent',-1)=CSTR_EQUAL)   then
  Result:=drCentury
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'year',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'y',-1)=CSTR_EQUAL)   then
  Result:=drYear
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'month',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'m',-1)=CSTR_EQUAL)   then
  Result:=drMonth
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'day',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'d',-1)=CSTR_EQUAL)   then
  Result:=drDay
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'hour',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'h',-1)=CSTR_EQUAL)   then
  Result:=drHour
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'minute',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'n',-1)=CSTR_EQUAL)   then
  Result:=drMinute
else if (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'second',-1)=CSTR_EQUAL) or
  (CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,PWideChar(AName),-1,'s',-1)=CSTR_EQUAL)   then
  Result:=drSecond;
end;

procedure TDateFunctions.DoDate(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Date;
end;
{DateAdd(ADate,APart,ADelta)}
procedure TDateFunctions.DoDateAdd(ASender: TObject);
var
  AFunc:TQFunction;
  ADate,APart,ADelta:TQValue;
  ADeltaVal:Integer;
begin
AFunc:=ASender as TQFunction;
ADate:=AFunc.Parameters[0].Value;
APart:=AFunc.Parameters[1].Value;
ADelta:=AFunc.Parameters[2].Value;
ADeltaVal:=ADelta.AsInteger;
case NeedDatePart(APart) of
  dpYear:
    AFunc.Value.AsDateTime:=IncYear(ADate.AsDateTime,ADeltaVal);
  dpMonth:
    AFunc.Value.AsDateTime:=IncMonth(ADate.AsDateTime,ADeltaVal);
  dpDay:
    AFunc.Value.AsDateTime:=IncDay(ADate.AsDateTime,ADeltaVal);
  dpHour:
    AFunc.Value.AsDateTime:=IncHour(ADate.AsDateTime,ADeltaVal);
  dpMinute:
    AFunc.Value.AsDateTime:=IncMinute(ADate.AsDateTime,ADeltaVal);
  dpSecond:
    AFunc.Value.AsDateTime:=IncSecond(ADate.AsDateTime,ADeltaVal);
  dpMSecond:
    AFunc.Value.AsDateTime:=IncMilliSecond(ADate.AsDateTime,ADeltaVal);
end;
end;
{DateDiff(AFirstDate,ASecondDate,APart)}
procedure TDateFunctions.DoDateDiff(ASender: TObject);
var
  AFunc:TQFunction;
  ADate1,ADate2:TQValue;
  APartVal:TDatePart;
begin
AFunc:=ASender as TQFunction;
ADate1:=AFunc.Parameters[0].Value;
ADate2:=AFunc.Parameters[1].Value;
APartVal:=NeedDatePart(AFunc.Parameters[2].Value);
case APartVal of
  dpYear:
    AFunc.Value.AsInteger:=YearsBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpMonth:
    AFunc.Value.AsInteger:=MonthsBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpDay:
    AFunc.Value.AsInteger:=DaysBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpHour:
    AFunc.Value.AsInteger:=HoursBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpMinute:
    AFunc.Value.AsInteger:=MinutesBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpSecond:
    AFunc.Value.AsInteger:=SecondsBetween(ADate1.AsDateTime,ADate2.AsDateTime);
  dpMSecond:
    AFunc.Value.AsInteger:=MilliSecondsBetween(ADate1.AsDateTime,ADate2.AsDateTime);
end;
end;

procedure TDateFunctions.DoDateEnd(ASender: TObject);
var
  AFunc:TQFunction;
  ADate,ARange:TQValue;
  function ReplaceTime(H,M,S,MS:WORD):TDateTime;
  var
    OH,OM,OS,OMS:Word;
  begin
  DecodeTime(ADate.AsDateTime,OH,OM,OS,OMS);
  if H>=60 then
    H:=OH;
  if M>=60 then
    M:=OM;
  if S>=60 then
    S:=OS;
  if MS>=60 then
    MS:=OMS;
  Result:=ADate.AsInteger+EncodeTime(H,M,S,MS);
  end;
begin
AFunc:=ASender as TQFunction;
ADate:=AFunc.Parameters[0].Value;
ARange:=AFunc.Parameters[1].Value;
case NeedDateRange(ARange) of
  drCentury:
    AFunc.Value.AsDateTime:=EncodeDate((YearOf(ADate.AsDateTime) div 100)*100+99,12,31);
  drYear:
    AFunc.Value.AsDateTime:=EndOfTheYear(ADate.AsDateTime);
  drQuarter:
    case MonthOf(ADate.AsDateTime) of
      1,2,3:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),3,31);
      4,5,6:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),6,30);
      7,8,9:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),9,30);
      10,11,12:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),12,31);
    end;
  drMonth:
    AFunc.Value.AsDateTime:=EndOfTheMonth(ADate.AsDateTime);
  drDay:
    AFunc.Value.AsDateTime:=EndOfTheDay(ADate.AsDateTime);
  drHour:
    AFunc.Value.AsDateTime:=ReplaceTime(60,59,59,999);
  drMinute:
    AFunc.Value.AsDateTime:=ReplaceTime(60,60,59,999);
  drSecond:
    AFunc.Value.AsDateTime:=ReplaceTime(60,60,60,999);
end;
end;

procedure TDateFunctions.DoDatePart(ASender: TObject);
var
  AVarDate,AVarPart:TQValue;
  APart:TDatePart;
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AVarDate:=AFunc.Parameters[0].Value;
AVarPart:=AFunc.Parameters[1].Value;
APart:=NeedDatePart(AVarPart);
case APart of
  dpYear:
    AFunc.Value.AsInteger:=YearOf(AVarDate.AsDateTime);
  dpMonth:
    AFunc.Value.AsInteger:=MonthOf(AVarDate.AsDateTime);
  dpDay:
    AFunc.Value.AsInteger:=DayOf(AVarDate.AsDateTime);
  dpHour:
    AFunc.Value.AsInteger:=HourOf(AVarDate.AsDateTime);
  dpMinute:
    AFunc.Value.AsInteger:=MinuteOf(AVarDate.AsDateTime);
  dpSecond:
    AFunc.Value.AsInteger:=SecondOf(AVarDate.AsDateTime);
  dpMSecond:
    AFunc.Value.AsInteger:=MilliSecondOf(AVarDate.AsDateTime);
end;
end;

procedure TDateFunctions.DoDateReplace(ASender: TObject);
var
  AFunc:TQFunction;
  AVarDate,AVarPart,AVarValue:TQValue;
  APart:TDatePart;
  Y,M,D,H,N,S,MS:WORD;
begin
AFunc:=ASender as TQFunction;
AVarDate:=AFunc.Parameters[0].Value;
AVarPart:=AFunc.Parameters[1].Value;
AVarValue:=AFunc.Parameters[2].Value;
APart:=NeedDatePart(AVarPart);
DecodeDateTime(AVarDate.AsDateTime,Y,M,D,H,N,S,MS);  
case APart of
  dpYear:
    AFunc.Value.AsDateTime:=EncodeDateTime(WORD(AVarValue.AsInteger),M,D,H,N,S,MS);
  dpMonth:
    AFunc.Value.AsDateTime:=EncodeDateTime(Y,WORD(AVarValue.AsInteger),D,H,N,S,MS);
  dpDay:
    AFunc.Value.AsDateTime:=EncodeDatetime(Y,M,WORD(AVarValue.AsInteger),H,N,S,MS);
  dpHour:
    AFunc.Value.AsDateTime:=EncodeDateTime(Y,M,D,WORD(AVarValue.AsInteger),N,S,MS);
  dpMinute:
    AFunc.Value.AsDateTime:=EncodeDateTime(Y,M,D,H,WORD(AVarValue.AsInteger),S,MS);
  dpSecond:
    AFunc.Value.AsDateTime:=EncodeDateTime(Y,M,D,H,N,WORD(AVarValue.AsInteger),MS);
  dpMSecond:
    AFunc.Value.AsDateTime:=EncodeDateTime(Y,M,D,H,N,S,WORD(AVarValue.AsInteger));
end;
end;

procedure TDateFunctions.DoDateStart(ASender: TObject);
var
  AFunc:TQFunction;
  ADate,ARange:TQValue;
  function ReplaceTime(H,M,S,MS:WORD):TDateTime;
  var
    OH,OM,OS,OMS:Word;
  begin
  DecodeTime(ADate.AsDateTime,OH,OM,OS,OMS);
  if H>=60 then
    H:=OH;
  if M>=60 then
    M:=OM;
  if S>=60 then
    S:=OS;
  if MS>=60 then
    MS:=OMS;
  Result:=ADate.AsInteger+EncodeTime(H,M,S,MS);
  end;
begin
AFunc:=ASender as TQFunction;
ADate:=AFunc.Parameters[0].Value;
ARange:=AFunc.Parameters[1].Value;
case NeedDateRange(ARange) of
  drCentury:
    AFunc.Value.AsDateTime:=EncodeDate((YearOf(ADate.AsDateTime) div 100)*100,1,1);
  drYear:
    AFunc.Value.AsDateTime:=StartOfTheYear(ADate.AsDateTime);
  drQuarter:
    case MonthOf(ADate.AsDateTime) of
      1,2,3:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),1,1);
      4,5,6:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),4,1);
      7,8,9:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),7,1);
      10,11,12:
        AFunc.Value.AsDateTime:=EncodeDate(YearOf(ADate.AsDateTime),10,1);
    end;
  drMonth:
    AFunc.Value.AsDateTime:=StartOfTheMonth(ADate.AsDateTime);
  drDay:
    AFunc.Value.AsDateTime:=StartOfTheDay(ADate.AsDateTime);
  drHour:
    AFunc.Value.AsDateTime:=ReplaceTime(60,0,0,0);
  drMinute:
    AFunc.Value.AsDateTime:=ReplaceTime(60,60,0,0);
  drSecond:
    AFunc.Value.AsDateTime:=ReplaceTime(60,60,60,0);
end;
end;

procedure TDateFunctions.DoEncodeDate(ASender: TObject);
var
  AFunc:TQFunction;
  AVarYear,AVarMonth,AVarDay:TQValue;
begin
AFunc:=ASender as TQFunction;
AVarYear:=AFunc.Parameters[0].Value;
AVarMonth:=AFunc.Parameters[1].Value;
AVarDay:=AFunc.Parameters[2].Value;
AFunc.Value.AsDateTime:=EncodeDate(AVarYear.AsInteger,AVarMonth.AsInteger,AVarDay.AsInteger);  
end;

procedure TDateFunctions.DoEncodeTime(ASender: TObject);
var
  AFunc:TQFunction;
  AVarHour,AVarMinute,AVarSecond,AVarMilliSecond:TQValue;
begin
AFunc:=ASender as TQFunction;
AVarHour:=AFunc.Parameters[0].Value;
AVarMinute:=AFunc.Parameters[1].Value;
AVarSecond:=AFunc.Parameters[2].Value;
AVarMilliSecond:=AFunc.Parameters[3].Value;
AFunc.Value.AsDateTime:=EncodeTime(AVarHour.AsInteger,AVarMinute.AsInteger,AVarSecond.AsInteger,AVarMilliSecond.AsInteger);
end;

procedure TDateFunctions.DoIsLeapYear(ASender: TObject);
var
  AFunc:TQFunction;
  AVarDate:TQValue;
begin
AFunc:=ASender as TQFunction;
AVarDate:=AFunc.Parameters[0].Value;
if AVarDate.IsDatetime then
  AFunc.Value.AsBoolean:=IsLeapYear(YearOf(AVarDate.AsDateTime))
else
  AFunc.Value.AsBoolean:=IsLeapYear(AVarDate.AsInteger);
end;

procedure TDateFunctions.DoIsToday(ASender: TObject);
var
  AFunc:TQFunction;
  AVarDate:TQValue;
begin
AFunc:=ASender as TQFunction;
AVarDate:=AFunc.Parameters[0].Value;
AFunc.Value.AsBoolean:=IsToday(AVarDate.AsDateTime);
end;

procedure TDateFunctions.DoMonthDays(ASender: TObject);
var
  AFunc:TQFunction;
  AVarYear,AVarMonth:TQValue;
begin
AFunc:=ASender as TQFunction;
AVarYear:=AFunc.Parameters[0].Value;
AVarMonth:=AFunc.Parameters[1].Value;
if not (AVarMonth.AsInteger in [1..12]) then
  ParserError(EPARSER_BAD_MONTH,Format(EMSG_BAD_MONTH,[PWideChar(AVarMonth.AsString)]));
AFunc.Value.AsInteger:=MonthDays[IsLeapYear(AVarYear.AsInteger)][AVarMonth.AsInteger];
end;

procedure TDateFunctions.DoNow(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Now;
end;

procedure TDateFunctions.DoTime(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Time;
end;

procedure TDateFunctions.DoToday(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Today;
end;

procedure TDateFunctions.DoTomorrow(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Tomorrow;
end;

procedure TDateFunctions.DoYesterday(ASender: TObject);
begin
(ASender as TQFunction).Value.AsDateTime:=Yesterday;
end;


function TDateFunctions.NeedDatePart(AValue: TQValue): TDatePart;
begin
if AValue.IsString then//以字符串格式传递的参数
  Result:=DatePartOfName(AValue.AsString)
else
  Result:=TDatePart(AValue.AsInteger);
if (Result<=dpNone) or (Result>dpMSecond) then
  ParserError(EPARSER_BAD_DATEPART,Format(EMSG_BAD_DATEPART,[PWideChar(AValue.AsString)]));
end;

function TDateFunctions.NeedDateRange(AValue: TQValue): TDateRange;
begin
if AValue.IsString then
  Result:=DateRangeOfName(AValue.AsString)
else
  Result:=TDateRange(AValue.AsInteger);
if (Result<=drNone) or (Result>drSecond) then
  ParserError(EPARSER_BAD_DATERANGE,Format(EMSG_BAD_DATERANGE,[PWideChar(AValue.AsString)]));
end;

procedure TDateFunctions.Register(AParser: TQExprParser);
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    RegisterFunction(AParser,'DateAdd',DoDateAdd,'ADate,APart,ADelta');
    RegisterFunction(AParser,'DateDiff',DoDateDiff,'AFirstDate,ASecondDate,APart');
    RegisterFunction(AParser,'DatePart',DoDatePart,'ADate,APart');
    RegisterFunction(AParser,'Now',DoNow,'');
    RegisterFunction(AParser,'Date',DoDate,'');
    RegisterFunction(AParser,'Time',DoTime,'');
    RegisterFunction(AParser,'MonthDays',DoMonthDays,'AYear,AMonth');
    RegisterFunction(AParser,'IsLeapYear',DoIsLeapYear,'AYear');
    RegisterFunction(AParser,'EncodeDate',DoEncodeDate,'AYear,AMonth,ADay');
    RegisterFunction(AParser,'Encodetime',DoEncodeTime,'AHour,AMinute,ASecond,AMillSecond');
    RegisterFunction(AParser,'Today',DoToday,'');
    RegisterFunction(AParser,'Yesterday',DoYesterday,'');
    RegisterFunction(AParser,'Tomorrow',DoTomorrow,'');
    RegisterFunction(AParser,'IsToday',DoIsToday,'ADate');
    RegisterFunction(AParser,'DateStart',DoDateStart,'ADate,ARange');
    RegisterFunction(AParser,'DateEnd',DoDateEnd,'ADate,ARange');
    RegisterFunction(AParser,'DateReplace',DoDateReplace,'ADate,APart,ANewVal');
  finally
    AParser.EndRegister;
  end;
  end;
end;

{ TQParameter }

constructor TQParameter.Create(AOwner: TQFunction);
begin
inherited Create;
FOwner:=AOwner;
FValue:=TQValue.Create;
end;

destructor TQParameter.Destroy;
begin
FValue.Free;
inherited;
end;

{ TMathFunctions }

constructor TMathFunctions.Create;
begin
inherited;
end;

procedure TMathFunctions.DoAvg(ASender: TObject);
var
  AFunc:TQFunction;
  I,C:Integer;
  ASum:Double;
  AParam:TQParameter;
begin
ASum:=0;
AFunc:=ASender as TQFunction;
C:=AFunc.ParamCount;
if C>0 then
  begin
  for I := 0 to C - 1 do
    begin
    AParam:=AFunc.Parameters[I];
    ASum:=ASum+AParam.Value.AsFloat;
    end;
  AFunc.Value.AsFloat:=ASum / C;
  end
else
  ParserError(EPARSER_PARAM_ATLEASTONE,Format(EMSG_PARAM_ATLEASTONE,[PWideChar(AFunc.DisplayName)]));
end;

procedure TMathFunctions.DoCount(ASender: TObject);
var
  ADistinct:Boolean;
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
ADistinct:=AFunc.Parameters[0].Value.AsBoolean;
if ADistinct then
  begin
  raise Exception.Create('Distinct参数暂时只支持False');
  end
else
  AFunc.Value.AsInteger:=AFunc.ParamCount-1;
end;

procedure TMathFunctions.DoMax(ASender: TObject);
var
  AMax:Double;
  I:Integer;
  AFunc:TQFunction;
  AVal:TQValue;
begin
AFunc:=(ASender as TQFunction);
if AFunc.ParamCount>0 then
  begin
  AMax:=AFunc.Parameters[0].Value.AsFloat;
  for I := 1 to AFunc.ParamCount - 1 do
    begin
    AVal:=AFunc.Parameters[I].Value;
    if AVal.AsFloat>AMax then
      AMax:=AVal.AsFloat;
    end;
  AFunc.Value.AsFloat:=AMax;
  end
else
  ParserError(EPARSER_PARAM_ATLEASTONE,Format(EMSG_PARAM_ATLEASTONE,[PWideChar(AFunc.DisplayName)]));
end;

procedure TMathFunctions.DoMin(ASender: TObject);
var
  AMin:Double;
  I:Integer;
  AFunc:TQFunction;
  AVal:TQValue;
begin
AFunc:=(ASender as TQFunction);
if AFunc.ParamCount>0 then
  begin
  AMin:=AFunc.Parameters[0].Value.AsFloat;
  for I := 1 to AFunc.ParamCount - 1 do
    begin
    AVal:=AFunc.Parameters[I].Value;
    if AVal.AsFloat<AMin then
      AMin:=AVal.AsFloat;
    end;
  AFunc.Value.AsFloat:=AMin;
  end
else
  ParserError(EPARSER_PARAM_ATLEASTONE,Format(EMSG_PARAM_ATLEASTONE,[PWideChar(AFunc.DisplayName)]));
end;

procedure TMathFunctions.DoStdDev(ASender: TObject);
var
  AFunc:TQFunction;
  AValues:array of Double;
  I:Integer;
begin
AFunc:=ASender as TQFunction;
if AFunc.ParamCount>0 then
  begin
  SetLength(AValues,AFunc.ParamCount);
  for I := 0 to AFunc.ParamCount - 1 do
    AValues[I]:=AFunc.Parameters[I].Value.AsFloat;
  AFunc.Value.AsFloat:=StdDev(AValues);
  end
else
  ParserError(EPARSER_PARAM_ATLEASTONE,Format(EMSG_PARAM_ATLEASTONE,[PWideChar(AFunc.DisplayName)]));
end;

procedure TMathFunctions.DoSum(ASender: TObject);
var
  AFunc:TQFunction;
  ASum:Double;
  I,C:Integer;
begin
AFunc:=ASender as TQFunction;
ASum:=0;
C:=AFunc.ParamCount;
for I := 0 to C - 1 do
  ASum:=AFunc.Parameters[I].Value.AsFloat;
AFunc.Value.AsFloat:=ASum;
end;

procedure TMathFunctions.Register(AParser: TQExprParser);
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    RegisterFunction(AParser,'Avg',DoAvg,'',True);
    RegisterFunction(AParser,'Sum',DoSum,'',True);
    //Distinct参数暂时没意义（未实现）
    RegisterFunction(AParser,'Count',DoCount,'Distinct',True);
    RegisterFunction(AParser,'Max',DoMax,'',True);
    RegisterFunction(AParser,'Min',DoMin,'',True);
    RegisterFunction(AParser,'StdDev',DoStdDev,'',True);
    RegisterFunction(AParser,'Round',DoRound,'AValue,ADigit',False);
    RegisterFunction(AParser,'BankRound',DoBankRound,'AValue,ADigit',False);
  finally
    AParser.EndRegister;
  end;
  end;
end;

procedure TMathFunctions.DoRound(ASender: TObject);
var
  AFunc:TQFunction;
  ADigit:Integer;
  AVal:Double;
begin
AFunc:=ASender as TQFunction;
ADigit:=AFunc.Parameters[1].Value.AsInteger;
if (ADigit>=-37) and (ADigit<=37) then
  begin
  AVal:=AFunc.Parameters[0].Value.AsFloat+Power(10,ADigit-1);
  AFunc.Value.AsFloat:=RoundTo(AVal,ADigit);
  end
else
  ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,[PWideChar(AFunc.DisplayName),PWideChar(AFunc.Parameters[1].Name)]));
end;

procedure TMathFunctions.DoBankRound(ASender: TObject);
var
  AFunc:TQFunction;
  ADigit:Integer;
begin
AFunc:=ASender as TQFunction;
ADigit:=AFunc.Parameters[1].Value.AsInteger;
if (ADigit>=-37) and (ADigit<=37) then
  AFunc.Value.AsFloat:=RoundTo(AFunc.Parameters[0].Value.AsFloat,ADigit)
else
  ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,[PWideChar(AFunc.DisplayName),PWideChar(AFunc.Parameters[1].Name)]));
end;

{ TOperatorFunctions }

constructor TOperatorFunctions.Create;
  function Add(AOpr:TQExprOperator;AName:WideString;AHandler:TNotifyEvent;AFixedParams:WideString;AVarParams:Boolean=false):TQOperatorFunction;
  var
    APos:Integer;
  begin
  Result:=TQOperatorFunction.Create(nil);
  Result.BindOperator:=AOpr;
  Result.OnGetValue:=AHandler;
  Result.Name:=AName;
  Operators[AOpr].OnExecute:=Result;
  Result.VarParams:=AVarParams;
  while Length(AFixedParams)>0 do
    begin
    APos:=Pos(',',AFixedParams);
    if APos=0 then
      begin
      Result.AddParam(AFixedParams,true);
      Break;
      end
    else
      begin
      Result.AddParam(Copy(AFixedParams,1,APos-1));
      Delete(AFixedParams,1,APos);
      end;
    end;
  end;
begin
Add(oprAssign,'Assign',DoAssign,'Left,Right');
Add(oprAdd,'MathAdd',DoMathAdd,'Left,Right');
Add(oprSub,'MathSub',DoMathSub,'Left,Right');
Add(oprMul,'MathMultiply',DoMathMultiply,'Left,Right');//*
Add(oprDiv,'MathDiv',DoMathDiv,'Left,Right');// /
Add(oprMod,'MathMod',DoMathMod,'Left,Right');
Add(oprPower,'MathPower',DoMathPower,'Left,Right');// **
Add(oprBitAnd,'BitAnd',DoBitAnd,'Left,Right');
Add(oprBitOr,'BitOr',DoBitOr,'Left,Right');
Add(oprBitNot,'BitNot',DoBitNot,'Value');
Add(oprBitXor,'BitXor',DoBitXor,'Left,Right');
Add(oprAnd,'LogicalAnd',DoLogicalAnd,'Left,Right');
Add(oprOr,'LogicalOr',DoLogicalOr,'Left,Right');
Add(oprNot,'LogicalNot',DoLogicalNot,'Value');
Add(oprLessThan,'CompareLT',DoCompareLT,'Left,Right');
Add(oprGreatThan,'CompareGT',DoCompareGT,'Left,Right');
Add(oprEqual,'CompareEQ',DoCompareEQ,'Left,Right');
Add(oprNotEqual,'CompareNE',DoCompareNE,'Left,Right');
Add(oprLessThanEqual,'CompareLE',DoCompareLE,'Left,Right');
Add(oprGreatThanEqual,'CompareGE',DoCompareGE,'Left,Right');
Add(oprLShift,'ShiftLeft',DoShiftLeft,'Left,Right');
Add(oprRShift,'ShiftRight',DoShiftRight,'Left,Right');
Add(oprDivTrunc,'DivTrunc',DoDivTrunc,'Left,Right');
Add(oprSelfAdd,'AddToSelf',DoAddToSelf,'Left,Right');
Add(oprSelfSub,'SubToSelf',DoSubToSelf,'Left,Right');
Add(oprSelfMul,'MulToSelf',DoMulToSelf,'Left,Right');// *=
Add(oprSelfDiv,'DivToSelf',DoDivToSelf,'Left,Right');
Add(oprSelfMod,'ModToSelf',DoModToSelf,'Left,Right');
Add(oprSelfBitAnd,'AndToSelf',DoAndToSelf,'Left,Right');
Add(oprSelfBitOr,'OrToSelf',DoOrToSelf,'Left,Right');
Add(oprSelfBitXor,'XorToSelf',DoXorToSelf,'Left,Right');
Add(oprSelfDivTrunc,'DivTruncToSelf',DoDivTruncToSelf,'Left,Right');
Add(oprInc,'Inc',DoIncrement,'AValue');
Add(oprDec,'Dec',DoDecrement,'AValue');
Add(oprSelfLShift,'ShlToSelf',DoLShiftToSelf,'AVar,ABits');
Add(oprSelfRShift,'ShrToSelf',DoRShiftToSelf,'AVar,ABits');
inherited;
end;

destructor TOperatorFunctions.Destroy;
var
  I:Integer;
begin
  for I := 0 to Integer(oprMax) - 1 do
    Operators[TQExprOperator(I)].OnExecute.Free;
  inherited;
end;

procedure TOperatorFunctions.DoAddToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Math_Add(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoAndToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Bit_And(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoAssign(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  AValue,ASrcVal:TQValue;
  AVar:TQVar;
begin
AFunc:=ASender as TQOperatorFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsVar then
  begin
  AVar:=AValue.AsReference;
  if AVar.Owner.FAliases.IndexOf(AVar.Name)>=0 then//是别名
    AFunc.Value.AsReference:=AVar
  else
    begin
    ASrcVal:=AFunc.Parameters[1].Value;
    if ASrcVal.IsReference then
      AValue.AsReference.Value.Assign(ASrcVal.AsReference.Value)
    else
      AValue.AsReference.Value.Assign(ASrcVal);
    AFunc.Owner.DoVarAssigned(AValue.AsReference);
    AFunc.Value.AsReference:=AValue.AsReference;
    end;
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoBitAnd(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Bit_And(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoBitNot(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Bit_Not(AFunc.Value);
end;

procedure TOperatorFunctions.DoBitOr(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Bit_Or(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoBitXor(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Bit_Xor(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoCompareEQ(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Compare_EQ(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoCompareGE(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Compare_GE(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoCompareGT(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Compare_GT(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoCompareLE(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Compare_LE(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoCompareLT(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Compare_LT(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoCompareNE(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=not AFunc.Parameters[0].Value.Compare_EQ(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoDecrement(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1:=p1.AsReference.Value;
  if p1.IsInteger then
    begin
    case p1.DataType of
      VARTYPE_BOOLEAN:
        p1.AsBoolean:=not p1.AsBoolean;//布尔值只有0和1，0++=1,1++=0
      VARTYPE_INTEGER:
        p1.AsInteger:=p1.AsInteger-1;
      VARTYPE_INT64:
        p1.AsInt64:=p1.AsInt64-1;
    end;
    AFunc.Value.Assign(p1);
    end
  else
    ParserError(EPARSER_OPERATOR_PARAM_NEEDINTEGER,Format(EMSG_OPERATOR_PARAM_NEEDINTEGER,[PWideChar(Operators[AFunc.BindOperator].Text)]));
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoDivToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsReference and (not p1.IsFunction) then
  begin
  p1.Math_Div(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoDivTrunc(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_DivTrunc(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoDivTruncToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Math_DivTrunc(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoIncrement(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
  AVar:TQVar;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  AVar:=p1.AsReference;
  if AVar.Value.IsInteger then
    begin
    case p1.DataType of
      VARTYPE_BOOLEAN:
        AVar.Value.AsBoolean:=not AVar.Value.AsBoolean;//布尔值只有0和1，0++=1,1++=0
      VARTYPE_INTEGER:
        AVar.Value.AsInteger:=AVar.Value.AsInteger+1;
      VARTYPE_INT64:
        AVar.Value.AsInt64:=AVar.Value.AsInt64+1;
    end;
    AFunc.Value.Assign(AVar.Value);
    end
  else
    ParserError(EPARSER_OPERATOR_PARAM_NEEDINTEGER,Format(EMSG_OPERATOR_PARAM_NEEDINTEGER,[PWideChar(Operators[AFunc.BindOperator].Text)]));
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoLogicalAnd(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Logical_And(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoLogicalNot(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Logical_Not;
end;

procedure TOperatorFunctions.DoLogicalOr(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsBoolean:=AFunc.Parameters[0].Value.Logical_Or(AFunc.Parameters[1].Value);
end;

procedure TOperatorFunctions.DoLShiftToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.ShiftLeft(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoMathAdd(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Add(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoMathDiv(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Div(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoMathMod(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Mod(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoMathMultiply(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Multiply(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoMathPower(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Power(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoMathSub(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Parameters[0].Value.Math_Sub(AFunc.Parameters[1].Value,AFunc.Value);
end;

procedure TOperatorFunctions.DoModToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Math_Mod(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoMulToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Math_Multiply(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoOrToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Bit_Or(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoRShiftToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.ShiftRight(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoShiftLeft(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.ShiftLeft(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoShiftRight(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.ShiftRight(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoSubToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Math_Sub(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.DoXorToSelf(ASender: TObject);
var
  AFunc:TQOperatorFunction;
  p1:TQValue;
begin
AFunc:=ASender as TQOperatorFunction;
p1:=AFunc.Parameters[0].Value;
if p1.IsVar then
  begin
  p1.Bit_Xor(AFunc.Parameters[1].Value,AFunc.Value);
  p1.AsReference.Value.Assign(AFunc.Value);
  end
else
  ParserError(EPARSER_LEFT_VAR_NEEDED,Format(EMSG_LEFT_VAR_NEEDED,[PWideChar(Operators[AFunc.BindOperator].Text)]));
end;

procedure TOperatorFunctions.Register(AParser: TQExprParser);
var
  I:Integer;
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    for I := 1 to Integer(oprMax) do
      begin
      if Assigned(Operators[TQExprOperator(I)].OnExecute) then
        AParser.Add(Operators[TQExprOperator(I)].OnExecute.Copy(AParser));
      end;
  finally
    AParser.EndRegister;
  end;
  end;
end;

{ TQExprStatement }

function TQExprStatement.Add(ALineNo, AOffset: Integer): TQExprStatement;
begin
Result:=TQExprStatement.Create(Owner);
Result.FParent:=Self;
Result.FLineNo:=ALineNo;
Result.FOffset:=AOffset;
FItems.Add(Result);
end;

procedure TQExprStatement.Assign(ASource: TQExprStatement);
var
  I:Integer;
  AChild:TQExprStatement;
begin
FText:=ASource.FText;
FLineNo:=ASource.FLineNo;
FOffset:=ASource.FOffset;
FResult.Assign(ASource.FResult);
for I := 0 to ASource.Count - 1 do
  begin
  AChild:=ASource.Items[I];
  Add(AChild.FLineNo,AChild.FOffset).Assign(AChild);
  end;
end;

procedure TQExprStatement.Clear;
var
  I:Integer;
begin
for I := 0 to FItems.Count - 1 do
  Items[I].Free;
FItems.Clear;
FResult.Clear;
end;

constructor TQExprStatement.Create(AOwner: TQExprParser);
begin
inherited Create;
FItems:=TList.Create;
FOwner:=AOwner;
FLineNo:=0;
FOffset:=0;
FResult:=TQVar.Create(AOwner);
FParent:=nil;
end;

destructor TQExprStatement.Destroy;
begin
Clear;
FItems.Free;
FResult.Free;
inherited;
end;

function TQExprStatement.GetCount: Cardinal;
begin
Result:=FItems.Count;
end;

function TQExprStatement.GetItems(AIndex: Integer): TQExprStatement;
begin
Result:=FItems[AIndex];
end;
function TQExprStatement.GetResult: TQVar;
begin
Result:=FResult;
end;

procedure TQExprStatement.InternalParse(var p: PWideChar;
  AParent:TQVar);
var
  AToken:WideString;
  AOperator,APriorOpr:TQExprOperator;
  AVar:TQVar;
  AFunc,ASubFunc:TQFunction;
  pLast:PWideChar;
  AType,AParamIndex:Integer;
  ARightParamNeeded:Boolean;
  //解析函数定义
  function ParseFunction:TQScriptFunction;
  begin
  //解析参数,如果是...，则意味着是可变参数
  Result:=TQScriptFunction.Create(Owner);
  try
    Result.Parse(p);
  except on E:Exception do
    begin
    Result.Free;
    raise;
    end;
  end;
  end;
begin
while p^<>#0 do
  begin
  pLast:=p;
  AOperator:=Owner.OperatorType(p);
  if AOperator=oprNone then
    begin
    if AParent.Count>0 then
      ParserError(EPARSER_NEED_END,Format(EMSG_NEED_END,[Copy(p,0,1)]));
    if DecodeQuotedStr(p,AToken) then
      begin
      AVar:=AParent.Add(Owner.NextTempName('V'));
      assert(AVar.Owner<>nil);
      AVar.Value.AsString:=AToken;
      end
    else
      begin
      AToken:=DecodeIdent(p);
      AType:=DetectTextType(PWideChar(AToken));
      if (AType and VARTYPE_DATETIME_MASK)<>0 then
        begin
        Avar:=AParent.Add(Owner.NextTempName('V'));
        assert(AVar.Owner<>nil);
        AVar.Value.AsDateTime:=DetectDateTime(PWideChar(AToken));
//        OutputDebugStringW(PWideChar('发现日期时间常量 '+AToken));
        end
      else
        begin
        case AType of
          VARTYPE_BOOLEAN:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsBoolean:=StrToBool(AToken);
//            OutputDebugStringW(PWideChar('发现布尔常量 '+AToken));
            end;
          VARTYPE_STRING:
            ParserError(EPARSER_BAD_TOKEN,Format(EMSG_BAD_TOKEN,[PWideChar(AToken)]));
          VARTYPE_INTEGER:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsInteger:=StrToInt(AToken);
//            OutputDebugStringW(PWideChar('发现32位整型常量 '+AToken));
            end;
          VARTYPE_INT64:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsInt64:=StrToInt64(AToken);
//            OutputDebugStringW(PWideChar('发现64位整型常量 '+AToken));
            end;
          VARTYPE_CHEX,VARTYPE_DHEX:
            begin
            if PWideChar(AToken)[0]='$' then //Delphi Style:$...
              begin
              AVar:=AParent.Add(Owner.NextTempName('V'));
              assert(AVar.Owner<>nil);
              if Length(AToken)<=9 then//整形
                begin
                AVar.Value.AsInteger:=StrToInt(AToken);
                end
              else
                begin
                AVar.Value.AsInt64:=StrToInt(AToken);
                end;
              end
            else//C Style:0x...
              begin
              AVar:=AParent.Add(Owner.NextTempName('V'));
              assert(AVar.Owner<>nil);
              if Length(AToken)<=10 then//整形
                begin
                AVar.Value.AsInteger:=StrToInt(AToken);
                end
              else
                begin
                AVar.Value.AsInt64:=StrToInt64(AToken);
                end;
              end;
//            OutputDebugStringW(PWideChar('发现十六进制整型常量 '+AToken));
            end;
          VARTYPE_FLOAT:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsFloat:=StrToFloat(AToken);
//            OutputDebugStringW(PWideChar('发现浮点型常量 '+AToken));
            end;
          VARTYPE_DATE,VARTYPE_TIME,VARTYPE_DATETIME:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsDateTime:=DetectDateTime(PWideChar(AToken));
//            OutputDebugStringW(PWideChar('发现日期时间型常量 '+AToken));
            end;
          VARTYPE_NUMERIC:
            begin
            AVar:=AParent.Add(Owner.NextTempName('V'));
            assert(AVar.Owner<>nil);
            AVar.Value.AsBcd:=StrToBcd(AToken);
//            OutputDebugStringW(PWideChar('发现BCD型常量 '+AToken));
            end;
          VARTYPE_VARNAME:
            begin
            if CompareStringW(LOCALE_SYSTEM_DEFAULT,NORM_IGNORECASE,'function',-1,PWideChar(AToken),-1)=CSTR_EQUAL then
              begin
              //函数定义,格式：
              //function 函数名称(参数列表)
              //{
              //...
              //}
              //取函数名
              ASubFunc:=ParseFunction;
              if Assigned(AParent.Data) and (TObject(AParent.Data) is TQOperatorFunction) and (TQOperatorFunction(AParent.Data).BindOperator=oprAssign) then
                begin
                Owner.AddAlias(TQFunction(AParent.Data).Parameters[0].Value.AsReference.Name,ASubFunc);
                AParent.Add(Owner.NextTempName('V')).Value.AsReference:=ASubFunc;
                end;
              end
            else if p^='(' then//一个标志符后面跟(意味着是一个函数
              begin
              Inc(p);
              ASubFunc:=Owner.FunctionByName(AToken);
              if Assigned(ASubFunc) then
                begin
                ASubFunc:=ASubFunc.Copy(Owner) as TQFunction;
                ASubFunc.Name:=Owner.NextTempName(ASubFunc.Name);//临时函数名
                Owner.FLocals.Add(ASubFunc);
                AVar:=AParent.Add(Owner.NextTempName('V'));
                assert(AVar.Owner<>nil);
                AVar.Value.AsReference:=ASubFunc;
                AParamIndex:=0;
                while (p^<>#0) do
                  begin
                  while CharIn(p^,#9#10#13' ') do
                      Inc(p);
                  if p^=')' then
                    Break
                  else
                    begin
                    AVar:=Owner.FLocals.Add(Owner.NextTempName(ASubFunc.Name+'_'+IntToStr(AParamIndex)));
                    assert(AVar.Owner<>nil);
                    if AParamIndex<ASubFunc.ParamCount then
                      ASubFunc.Parameters[AParamIndex].Value.AsReference:=AVar
                    else if ASubFunc.VarParams then
                      begin
                      if AParamIndex<ASubFunc.MaxParamCount then
                        ASubFunc.AddParam(Owner.NextTempName('P')).Value.AsReference:=AVar
                      else
                        ParserError(EPARSER_PARAM_TOOMANY,Format(EMSG_PARAM_TOOMANY,[PWideChar(ASubFunc.DisplayName)]));
                      end
                    else
                      ParserError(EPARSER_PARAM_TOOMANY,Format(EMSG_PARAM_TOOMANY,[PWideChar(ASubFunc.DisplayName)]));
                    AVar.Data:=ASubFunc;//用Data成员来关联相应的函数实现?
                    InternalParse(p,AVar);
                    if p^=',' then
                      Inc(p);
                    Inc(AParamIndex);
                    end;
                  end;
                if p^=')' then
                  begin
                  Inc(p);
                  while CharIn(p^,#9#10#13' ') do
                      Inc(p);
                  if AParamIndex<ASubFunc.FixedParamCount then
                    ParserError(EPARSER_PARAM_TOOFEW,Format(EMSG_PARAM_TOOFEW,[PWideChar(ASubFunc.DisplayName)]))
                  end
                else
                  ParserError(EPARSER_PARAM_NOT_END,Format(EMSG_PARAM_NOT_END,[PWideChar(ASubFunc.DisplayName)]));
                end
              else
                ParserError(EPARSER_FUNC_MISSED,Format(EMSG_FUNC_MISSED,[PWideChar(AToken)]));
              end
            else
              begin
              AVar:=Owner.VarByPath(AToken);
              if not Assigned(AVar) then
                AVar:=Owner.FLocals.ForcePath(AToken)
              else if AVar is TQFunction then
                begin
                if Assigned(AParent.Data) and (TObject(AParent.Data) is TQOperatorFunction) and (TQOperatorFunction(AParent.Data).BindOperator=oprAssign) then
                  Owner.AddAlias(TQFunction(AParent.Data).Parameters[0].Value.AsReference.Name,AVar);//定义别名
                end;
              AParent.Add(Owner.NextTempName('V')).Value.AsReference:=AVar;
              end;
            end;
        end;
        end;
      end;
    end
  else if AOperator in [oprComma,oprBlockEnd] then//,
    begin
    p:=pLast;
    Break
    end
  else if AOperator=oprStatementEnd then
    begin
    p:=pLast;
    Break;
    end
  else if AOperator = oprBlockStart then//括号，优先级最高
    begin
    AVar:=AParent.Add(Owner.NextTempName('V'));
    assert(AVar.Owner<>nil);
    InternalParse(p,AVar);
    if p^=')' then
      Inc(p);
    while CharIn(p^,#9#10#13' ') do
      Inc(p);
    end
  else if AOperator = oprBraceStart then
    begin
    AVar:=TQScriptFunction.Create(Owner);
    AVar.Name:=Owner.NextTempName('B');
    TQScriptFunction(AVar).ParseBlock(p);
    Owner.Locals.Add(AVar);
    AParent.Add(AVar);
    end
  else if AOperator = oprLineComment then
    begin
    while (p^<>#0) and (not CharIn(p^,#10#13)) do
      Inc(p);
    while CharIn(p^,#9#10#13' ') do
      Inc(p);
    end
  else if AOperator = oprBlockComment then
    begin
    while p^<>#0 do
      begin
      if (p[0]='*') and (p[1]='/') then
        begin
        Inc(p,2);
        Break;
        end
      else
        Inc(p);
      end;
    while CharIn(p^,#9#10#13' ') do
      Inc(p);    
    end
  else//如果直接是+x/-x这样的表达式，则Left值为NULL，直接当作0进行运算，也是正确的
    begin
    if Operators[AOperator].ParamPos in [oppBoth,oppRight] then
      ARightParamNeeded:=True
    else if Operators[AOperator].ParamPos=oppAny then
      ARightParamNeeded:=(AParent.Count=0)
    else
      ARightParamNeeded:=False;
    ASubFunc:=Operators[AOperator].OnExecute;
    if not Assigned(ASubFunc) then
      ParserError(EPARSER_OPERATOR_NOTIMPL,Format(EMSG_OPERATOR_NOTIMPL,[PWideChar(Operators[AOperator].Text)]));
    ASubFunc:=ASubFunc.Copy(Owner) as TQFunction;
    assert(ASubFunc.Owner<>nil);
    ASubFunc.Name:=Owner.NextTempName(ASubFunc.Name);
    Owner.FLocals.Add(ASubFunc);
    APriorOpr:=oprNone;
    if Assigned(AParent.Data) and (TQFunction(AParent.Data) is TQOperatorFunction) then
      begin
      AFunc:=TQOperatorFunction(AParent.Data);
      APriorOpr:=TQOperatorFunction(AParent.Data).BindOperator;
      end
    else
      AFunc:=nil;
    if Operators[APriorOpr].Pri<=Operators[AOperator].Pri then
      begin
      if AParent.Count>0 then
        begin
        AVar:=AParent.Items[0].Copy(Owner);
        assert(AVar.Owner<>nil);
        AVar.Name:=Owner.NextTempName(AVar.DisplayName);
        ASubFunc.Parameters[0].Value.AsReference:=AVar;
        Owner.FLocals.Add(AVar);
        AParent.Items[0].Value.AsReference:=ASubFunc;
//        AParent.Delete(0);
//        AParent.Add(ASubFunc);
        end
      else
        begin
        if (AOperator=oprAdd) or (AOperator=oprSub) then//+/-号
          begin
          ASubFunc.Parameters[0].Value.AsInteger:=0;
          AVar:=AParent.Add(Owner.NextTempName('V'));
          assert(AVar.Owner<>nil);
          AVar.Value.AsReference:=ASubFunc;
          end
        else if ASubFunc.ParamCount>1 then
          ParserError(EPARSER_OPERATOR_NOPARAM,Format(EMSG_OPERATOR_NOPARAM,[PWideChar(Operators[AOperator].Text)]))
        else
          AParent.Add(ASubFunc);
        end;
      end
    else
      begin
      //P.Data=ParentFunction
      if Assigned(AFunc) then
        begin
        if Assigned(AParent.Parent.Data) then
          begin
          AFunc:=TQFunction(AParent.Parent.Data);
          AFunc.Parameters[AFunc.ParamCount-1].Value.AsReference:=ASubFunc;
          ASubFunc.Parameters[0].Value.AsReference:=TQFunction(AParent.Data);
          end
        else
          begin
          p:=pLast;
          Break;
          end;
        end;
      end;
    if ARightParamNeeded then
      begin
      AVar:=Owner.FLocals.Add(Owner.NextTempName(ASubFunc.DisplayName));
      assert(AVar.Owner<>nil);
      AVar.Data:=ASubFunc;
      if ASubFunc.ParamCount=1 then
        ASubFunc.Parameters[0].Value.AsReference:=AVar
      else
        ASubFunc.Parameters[1].Value.AsReference:=AVar;
      InternalParse(p,AVar);
      end;
    end;
  end;
if AParent.Count=1 then
  begin
  AParent.Value.AsReference:=AParent.Items[0];
  end;
//OutputDebugStringW(PWideChar('==>'+AParent.Text));

end;

//解析一句代码
procedure TQExprStatement.Parse(var p: PWideChar);
var
  ps:PWideChar;
begin
FResult.Clear;
ps:=p;
while p^<>#0 do
  begin
  if (p^=';') then//语句结束
    begin
    Inc(p);
    Break
    end
  else
    begin
    InternalParse(p,FResult);
    if (p^=',') then
      Inc(p)
    else if p^=')' then
      ParserError(EPARSER_CHAR_UNEXPECT,Format(EMSG_CHAR_UNEXPECT,[')']));
    end;
  end;
FText:=Copy(ps,1,p-ps);
end;

{ TQOperatorFunction }

procedure TQOperatorFunction.Assign(AVar: TQVar);
begin
inherited;
if AVar is TQOperatorFunction then
  FBindOperator:=TQOperatorFunction(AVar).FBindOperator;
end;


constructor TQOperatorFunction.Create(AOwner: TQExprParser);
begin
inherited Create(AOwner);
end;

{ TQFunctions }

constructor TQFunctions.Create;
begin
inherited;
Register(QExpGlobal);
end;

destructor TQFunctions.Destroy;
begin

  inherited;
end;

function TQFunctions.RegisterFunction(AParser:TQVar;AName: WideString; AHandler: TNotifyEvent;
  AFixedParams: WideString; AVarParams: Boolean): TQFunction;
begin
Result:=AParser.AddFunction(AName,AHandler,AFixedParams,AVarParams);
end;

{ TStringFunctions }

constructor TStringFunctions.Create;
begin
inherited;
end;

destructor TStringFunctions.Destroy;
begin
{$IFDEF QEXP_REGEX}
if Assigned(FRegex) then
  FRegex.Free;

if Assigned(FSplitStrings) then
  begin
  FSplitStrings.Clear;
  FSplitStrings.Free;
  end;
{$ENDIF}
inherited;
end;
//DecodeToken(S,ADelimiters,AQuoter,AIgnoreCase)
procedure TStringFunctions.DoDecodeToken(ASender: TObject);
var
  AFunc:TQFunction;
  p:PWideChar;
  S:WideString;
  AVar:TQVar;
  AValue:TQValue;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsVar then
  AValue:=AValue.AsReference.Value;
S:=AValue.AsString;
p:=PWideChar(S);
AFunc.Value.AsString:=DecodeToken(p,PWideChar(AFunc.Parameters[1].Value.AsString),PWideChar(AFunc.Parameters[2].Value.AsString)^,AFunc.Parameters[3].Value.AsBoolean);
AValue.AsString:=p;
end;

procedure TStringFunctions.DoDequoteStr(ASender: TObject);
var
  AFunc:TQFunction;
  S,R:WideString;
  AQuoterChar:WideChar;
  pw,pd:PWideChar;
begin
AFunc:=ASender as TQFunction;
S:=AFunc.Parameters[0].Value.AsString;
AQuoterChar:=PWideChar(AFunc.Parameters[1].Value.AsString)[0];
if AQuoterChar=#0 then
  begin
  AQuoterChar:=PWideChar(S)[0];
  if (AQuoterChar<>'''') and (AQuoterChar='"') then
    AQuoterChar:=#0;
  end;
if AQuoterChar=#0 then
  AFunc.Value.AsString:=S
else
  begin
  pw:=PWideChar(S);
  SetLength(R,Length(S));
  pd:=PWideChar(R);
  while pw^<>#0 do
    begin
    if ((pw[0]='\') and (pw[1]=AQuoterChar)) or ((pw[0]=AQuoterChar) and (pw[1]=AQuoterChar)) then
      begin
      pd^:=AQuoterChar;
      Inc(pw,2);
      end
    else
      begin
      pd^:=pw^;
      Inc(pw);
      end;
    Inc(pd);
    end;
  AFunc.Value.AsString:=Copy(R,1,pd-PWideChar(R));
  end;
end;

procedure TStringFunctions.DoEndWith(ASender: TObject);
var
  AFunc:TQFunction;
  S1,S2:WideString;
  AFlags:Integer;
begin
AFunc:=ASender as TQFunction;
S1:=AFunc.Parameters[0].Value.AsString;
S2:=AFunc.Parameters[1].Value.AsString;
if AFunc.Parameters[2].Value.AsBoolean then
  AFlags:=NORM_IGNORECASE
else
  AFlags:=0;
if Length(S1)>Length(S2) then
  AFunc.Value.AsBoolean:=(CompareStringW(LOCALE_SYSTEM_DEFAULT,AFlags,PWideChar(Copy(S1,Length(S1)-Length(S2)+1,Length(S2))),Length(S2),PWideChar(S2),Length(S2))=CSTR_EQUAL)
else
  AFunc.Value.AsBoolean:=False;
end;

procedure TStringFunctions.DoLeft(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsString:=Copy(AFunc.Parameters[0].Value.AsString,1,AFunc.Parameters[1].Value.AsInteger);
end;

procedure TStringFunctions.DoLength(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsInteger:=Length(AFunc.Parameters[0].Value.AsString);
end;

procedure TStringFunctions.DoLike(ASender: TObject);
var
  AStr,APat:WideString;
  P1,P2:PWideChar;
  C:WideChar;
  AMinCount:Integer;
  AFunc:TQFunction;
  AIgnoreCase,AResult:Boolean;
  function FirstCharOf(var w:PWideChar):WideChar;
  begin
  if w^='\' then
    begin
    case w[1] of
      'r':// \r
        Result:=#13;
      'n':// \n
        Result:=#10;
      't':// \t
        Result:=#9;
      'b':// \b
        Result:=#7;
      else//其它一律取后面的字符进行比较
        Result:=w[1];
    end;
    Inc(w,2);
    end
  else
    begin
    Result:=w^;
    Inc(w);
    end;
  end;
begin
AFunc:=ASender as TQFunction;
AStr:=AFunc.Parameters[0].Value.AsString;
APat:=AFunc.Parameters[1].Value.AsString;
AIgnoreCase:=AFunc.Parameters[2].Value.AsBoolean;
P2:=PWideChar(APat);
P1:=PWideChar(AStr);
AResult:=True;
if AIgnoreCase then
  begin
  CharUpperBuffW(P1,Length(AStr));
  CharUpperBuffW(P2,Length(APat));
  end;
while P2^<>#0 do
  begin
  if (P2^='_') or (P2^=P1^) then
    begin
    Inc(P2);
    Inc(P1);
    end
  else if P2^='%' then
    begin
    Inc(P2);
    AMinCount:=0;
    while P2^<>#0 do
      begin
      if P2^='_' then
        Inc(AMinCount)
      else if P2^<>'%' then
        Break;
      Inc(P2);
      end;
    //至少路过的字符数
    while AMinCount>0 do
      begin
      if P1^<>#0 then
        begin
        Inc(P1);
        Dec(AMinCount);
        end
      else
        begin
        AResult:=False;
        Break;
        end;
      end;
    if AMinCount>0 then
      Break;
    //找到第一个与P2^匹配的字符
    if P2^=#0 then
      Break
    else
      begin
      C:=FirstCharOf(P2);
      while P1^<>#0 do
        begin
        if P1^<>C then
          Inc(P1)
        else
          Break;
        end;
      if P1^<>C then //意味着到结束仍不相等
        begin
        AResult:=False;
        Break;
        end
      else if P2^=#0 then//P2已经结束了，除非P1结束，否则不等
        begin
        Inc(P1);
        if P1^<>#0 then
          AResult:=False;
        Break;
        end
      else
        Inc(P1);
      end;
    end
  else if P2^='\' then
    begin
    if P1^<>FirstCharOf(P2) then
      begin
      AResult:=False;
      Break;
      end;
    Inc(P1);
    end
  else
    begin
    AResult:=False;
    Break;
    end;
  end;
AFunc.Value.AsBoolean:=AResult;
end;

procedure TStringFunctions.DoLower(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsString:=CharLowerW(PWideChar(AFunc.Parameters[0].Value.AsString));
end;

procedure TStringFunctions.DoPos(ASender: TObject);
var
  AFunc:TQFunction;
  AStr,ASub:WideString;
  function InternalPos(AIgnoreCase:Boolean):Integer;
  var
    p1,p2:PWideChar;
    AFlags,L:Integer;
  begin
  p1:=PWideChar(AStr);
  p2:=PWideChar(ASub);
  L:=Length(ASub);
  if AIgnoreCase then
    AFlags:=NORM_IGNORECASE
  else
    AFlags:=0;
  Result:=0;
  while p1^<>#0 do
    begin
    if p1^=p2^ then
      begin
      if CompareStringW(LOCALE_SYSTEM_DEFAULT,AFlags,p1,L,p2,L)=CSTR_EQUAL then
        begin
        Result:=p1-PWideChar(AStr)+1;
        Break;
        end
      else
        Inc(p1);
      end
    else
      Inc(p1);
    end;
  end;
begin
AFunc:=ASender as TQFunction;
ASub:=AFunc.Parameters[0].Value.AsString;
AStr:=AFunc.Parameters[1].Value.AsString;
AFunc.Value.AsInteger:=InternalPos(AFunc.Parameters[2].Value.AsBoolean);
end;

procedure TStringFunctions.DoQuotedStr(ASender: TObject);
var
  S,R:WideString;
  ps,pd:PWideChar;
  AFunc:TQFunction;
  AQuoter:WideChar;
begin
AFunc:=ASender as TQFunction;
S:=AFunc.Parameters[0].Value.AsString;
AQuoter:=PWideChar(AFunc.Parameters[1].Value.AsString)[0];
if AQuoter=#0 then
  AQuoter:='"';
SetLength(R,(Length(S)+1) shl 1);
ps:=PWideChar(S);
pd:=PWideChar(R);
pd[0]:=AQuoter;
Inc(pd);
while ps^<>#0 do
  begin
  if ps^=AQuoter then
    begin
    pd[0]:=AQuoter;
    pd[1]:=AQuoter;
    Inc(pd,2);
    end
  else
    begin
    pd[0]:=ps^;
    Inc(pd);
    end;
  Inc(ps);
  end;
AFunc.Value.AsString:=Copy(R,1,pd-PWideChar(R))+AQuoter;  
end;
{AStr,APat,AMatches,AStartOffset,AIncOffset}
{$IFDEF QEXP_REGEX}
procedure TStringFunctions.DoRegexLastLength(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if not Assigned(FRegex) then
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
AFunc.Value.AsInteger:=FRegex.MatchedLength;
end;

procedure TStringFunctions.DoRegexLastOffset(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if not Assigned(FRegex) then
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
AFunc.Value.AsInteger:=FRegex.MatchedOffset;
end;
//AStr,APat,AStartOffset
procedure TStringFunctions.DoRegexMatch(ASender: TObject);
var
  AFunc:TQFunction;
  APat:WideString;
  ASubject:AnsiString;
begin
AFunc:=ASender as TQFunction;
if not Assigned(FRegex) then
  FRegex:=TPerlRegEx.Create;
ASubject:=Utf8Encode(AFunc.Parameters[0].Value.AsString);
APat:=AFunc.Parameters[1].Value.AsString;
if SetupRegexExp(APat) then
  begin
  FRegex.Subject:=ASubject;
  FRegex.Start:=AFunc.Parameters[2].Value.AsInteger;
  if not FRegex.Match then
    AFunc.Value.Asstring:=''
  else
    AFunc.Value.AsString:=FRegex.MatchedText;
  end
else
  ParserError(EPARSER_BAD_REGEX_EXP,Format(EMSG_BAD_REGEX_EXP,[PWideChar(APat)]));
end;

procedure TStringFunctions.DoRegexMatchNext(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if Assigned(FRegex) and FRegex.FoundMatch then
  begin
  if FRegex.MatchAgain then
    AFunc.Value.AsString:=FRegex.MatchedText
  else
    AFunc.Value.AsString:='';
  end
else
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
end;
//'AStr,APat,AReplace,ALimit'
procedure TStringFunctions.DoRegexReplace(ASender: TObject);
var
  AFunc:TQFunction;
  AStr,APat,AReplace:WideString;
  ALimit:Integer;
begin
AFunc:=ASender as TQFunction;
AStr:=AFunc.Parameters[0].Value.AsString;
APat:=AFunc.Parameters[1].Value.AsString;
AReplace:=AFunc.Parameters[2].Value.AsString;
ALimit:=AFunc.Parameters[3].Value.AsInteger;
if SetupRegexExp(APat) then
  begin
  FRegex.Replacement:=Utf8Encode(AReplace);
  if ALimit<0 then
    FRegex.ReplaceAll
  else if ALimit>0 then
    begin
    if not FRegex.FoundMatch then
      FRegex.Match;
    if FRegex.FoundMatch then
      begin
      repeat
        begin
        FRegex.Replace;
        Dec(ALimit);
        end;
      until (ALimit=0) or (not FRegex.MatchAgain);
      end;
    end;
  AFunc.Value.AsString:=Utf8Decode(FRegex.Subject);
  end
else
  ParserError(EPARSER_BAD_REGEX_EXP,Format(EMSG_BAD_REGEX_EXP,[PWideChar(APat)]));
end;
//AStr,APat,ALimit,AIncEmpty,AIncGroup
procedure TStringFunctions.DoRegexSplit(ASender: TObject);
var
  AFunc:TQFunction;
  AStr,APat:WideString;
  AItem:AnsiString;
  ALimit,AOffset:Integer;
  AIncEmpty,AIncGroup:Boolean;
begin
AFunc:=ASender as TQFunction;
AStr:=AFunc.Parameters[0].Value.AsString;
APat:=AFunc.Parameters[1].Value.AsString;
ALimit:=AFunc.Parameters[2].Value.AsInteger;
AIncEmpty:=AFunc.Parameters[3].Value.AsBoolean;
AIncGroup:=AFunc.Parameters[4].Value.AsBoolean;
if SetupRegexExp(APat) then
  begin
  if not Assigned(FSplitStrings) then
    FSplitStrings:=TStringList.Create;
  FSplitStrings.Clear;
  FRegex.Subject:=Utf8Encode(AStr);
  if (ALimit=1) or (not FRegex.Match) then
    FSplitStrings.Add(AStr)
  else
    begin
    FSplitStrings.BeginUpdate;
    try
      AOffset:=1;
      repeat
        begin
        AItem:=Copy(FRegex.Subject,AOffset,FRegex.MatchedOffset-AOffset);
        if (Length(AItem)>0) or AIncEmpty then
          FSplitStrings.AddObject(AItem,TObject(AOffset));
        if AIncGroup and (FRegex.GroupCount > 0) then
          FSplitStrings.Add(FRegex.Groups[FRegex.GroupCount]);
        AOffset:=FRegex.MatchedOffset+FRegex.MatchedLength;
        Dec(ALimit);
        end;
      until (ALimit=0) or (not FRegex.MatchAgain);
      AItem:=Copy(FRegex.Subject,AOffset,MaxInt);
      if (Length(AItem)>0) or AIncEmpty then
        {$IFDEF UNICODE}
        FSplitStrings.AddObject(String(AItem),TObject(AOffset))
        {$ELSE}
        FSplitStrings.AddObject(AItem,TObject(AOffset))
        {$ENDIF}
    finally
      FSplitStrings.EndUpdate;
    end;
    end;
  AFunc.Value.AsInteger:=FSplitStrings.Count;
  end
else
  ParserError(EPARSER_BAD_REGEX_EXP,Format(EMSG_BAD_REGEX_EXP,[PWideChar(APat)]));
end;

procedure TStringFunctions.DoRegexSplitCount(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if not (Assigned(FRegex) and Assigned(FSplitStrings)) then
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
AFunc.Value.AsInteger:=FSplitStrings.Count;
end;

procedure TStringFunctions.DoRegexSplitOffset(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if not (Assigned(FRegex) and Assigned(FSplitStrings)) then
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
AFunc.Value.AsInteger:=Integer(FSplitStrings.Objects[AFunc.Parameters[0].Value.AsInteger]);
end;

procedure TStringFunctions.DoRegexSplitText(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
if not (Assigned(FRegex) and Assigned(FSplitStrings)) then
  ParserError(EPARSER_BAD_MATCH_STATE,EMSG_BAD_MATCH_STATE);
AFunc.Value.AsString:=FSplitStrings[AFunc.Parameters[0].Value.AsInteger];
end;
{$ENDIF}
procedure TStringFunctions.DoRight(ASender: TObject);
var
  AFunc:TQFunction;
  S:WideString;
  C:Integer;
begin
AFunc:=ASender as TQFunction;
S:=AFunc.Parameters[0].Value.AsString;
C:=AFunc.Parameters[1].Value.AsInteger;
AFunc.Value.AsString:=Copy(S,Length(S)-C+1,C);
end;
//strcmp(s1,l1,s2,l2,ignorecase);
procedure TStringFunctions.DoStartWith(ASender: TObject);
var
  AFunc:TQFunction;
  S1,S2:WideString;
  AFlags:Integer;
begin
AFunc:=ASender as TQFunction;
S1:=AFunc.Parameters[0].Value.AsString;
S2:=AFunc.Parameters[1].Value.AsString;
if AFunc.Parameters[2].Value.AsBoolean then
  AFlags:=NORM_IGNORECASE
else
  AFlags:=0;
if Length(S1)>Length(S2) then
  AFunc.Value.AsBoolean:=(CompareStringW(LOCALE_SYSTEM_DEFAULT,AFlags,PWideChar(S1),Length(S2),PWideChar(S2),Length(S2))=CSTR_EQUAL)
else
  AFunc.Value.AsBoolean:=False;
end;

procedure TStringFunctions.DoStrCmp(ASender: TObject);
var
  AFunc:TQFunction;
  S1,S2:WideString;
  L1,L2,AFlags:Integer;
begin
AFunc:=ASender as TQFunction;
S1:=AFunc.Parameters[0].Value.AsString;
L1:=AFunc.Parameters[1].Value.AsInteger;
S2:=AFunc.Parameters[2].Value.AsString;
L2:=AFunc.Parameters[3].Value.AsInteger;
AFlags:=0;
if AFunc.Parameters[4].Value.AsBoolean then
  AFlags:=AFlags or NORM_IGNORECASE;
AFunc.Value.AsInteger:=CompareStringW(LOCALE_SYSTEM_DEFAULT,AFlags,PWideChar(S1),L1,PWideChar(S2),L2)-CSTR_EQUAL;
end;

procedure TStringFunctions.DoSubString(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsString:=System.Copy(AFunc.Parameters[0].Value.AsString,AFunc.Parameters[1].Value.AsInteger,AFunc.Parameters[2].Value.AsInteger);
end;

procedure TStringFunctions.DoUpper(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsString:=CharUpperW(PWideChar(AFunc.Parameters[0].Value.AsString));
end;

procedure TStringFunctions.Register(AParser: TQExprParser);
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    RegisterFunction(AParser,'Left',DoLeft,'AValue,ACount');
    RegisterFunction(AParser,'Right',DoRight,'AValue,ACount');
    RegisterFunction(AParser,'UpperCase',DoUpper,'AValue');
    RegisterFunction(AParser,'LowerCase',DoLower,'AValue');
    RegisterFunction(AParser,'StrCmp',DoStrCmp,'AFirst,AFirstLen,ASecond,ASecondLen,AIgnoreCase');
    RegisterFunction(AParser,'QuotedStr',DoQuotedStr,'AValue,AQuoter');
    RegisterFunction(AParser,'DequoteStr',DoDequoteStr,'AValue,AQouter');
    RegisterFunction(AParser,'Pos',DoPos,'ASubStr,AStr,AIgnoreCase');
    RegisterFunction(AParser,'Length',DoLength,'AStr');
    RegisterFunction(AParser,'SubString',DoSubString,'AStr,AStartPos,ACount');
    RegisterFunction(AParser,'Like',DoLike,'AStr,APat,AIgnoreCase');
    RegisterFunction(AParser,'StartWith',DoStartWith,'AStr,ASub,AIgnoreCase');
    RegisterFunction(AParser,'EndWith',DoEndWith,'AStr,ASub,AIgnoreCase');
    RegisterFunction(AParser,'DecodeToken',DoDecodeToken,'AStrVar,ADelimiters,AQuoter,AIgnoreCase');
    {$IFDEF QEXP_REGEX}
    RegisterFunction(AParser,'RegexMatch',DoRegexMatch,'AStr,APat,AStartOffset');
    RegisterFunction(AParser,'RegexMatchNext',DoRegexMatchNext,'');
    RegisterFunction(AParser,'RegexLastOffset',DoRegexLastOffset,'');
    RegisterFunction(AParser,'RegexLastLength',DoRegexLastLength,'');
    RegisterFunction(AParser,'RegexSplit',DoRegexSplit,'AStr,APat,ALimit,AIncEmpty,AIncGroup');
    RegisterFunction(AParser,'RegexSplitCount',DoRegexSplitCount,'');
    RegisterFunction(AParser,'RegexSplitText',DoRegexSplitText,'AIndex');
    RegisterFunction(AParser,'RegexSplitOffset',DoRegexSplitOffset,'AIndex');
    RegisterFunction(AParser,'RegexReplace',DoRegexReplace,'AStr,APat,AReplace,ALimit');
    {$ENDIF}
  finally
    AParser.EndRegister;
  end;
  end;
end;
{$IFDEF QEXP_REGEX}
function TStringFunctions.SetupRegexExp(S: WideString):Boolean;
var
  APat:AnsiString;
  ps,p,pe:PAnsiChar;
  AOptions:TPerlRegExOptions;
begin
if FLastRegexExp<>S then
  begin
  APat:=Utf8Encode(S);
  p:=PAnsiChar(APat);
  ps:=p;
  Result:=True;
  try
    if ps^ in ['A'..'Z','a'..'z','0'..'9','\',' ',#9,#10,#13] then
      FRegex.RegEx:=UTF8String(APat)
    else
      begin
      Inc(ps);
      pe:=p+Length(APat)-1;
      AOptions:=[];
      while pe[0]<>p[0] do
        begin
        case pe^ of
          'i'://忽略大小写
            AOptions:=AOptions+[preCaseLess];
          'm':
            AOptions:=AOptions+[preMultiLine];
          's':
            AOptions:=AOptions+[preSingleLine];
          'x':
            AOptions:=AOptions+[preExtended];
          'A':
            AOptions:=AOptions+[preAnchored];
          'U':
            AOptions:=AOptions+[preUnGreedy];
          'n':
            AOptions:=AOptions+[preNoAutoCapture];
        end;
        Dec(pe);
        end;
      APat:=Copy(ps,1,pe-ps);
      FRegex.Options:=AOptions;
      FRegex.RegEx:=UTF8String(APat);
      end;
    FRegex.Compile;
  finally
    if not FRegex.Compiled then
      begin
      FRegex.RegEx:='';
      FLastRegexExp:='';
      end
    else
    FLastRegexExp:=WideString(APat);
  end;
  end
else
  Result:=True;
end;
{$ENDIF}

{ TQExpBytesFunctions }

constructor TQExpBytesFunctions.Create;
begin
inherited;
end;

destructor TQExpBytesFunctions.Destroy;
begin
  inherited;
end;

procedure TQExpBytesFunctions.DoGetByte(ASender: TObject);
var
  AFunc:TQFunction;
  AValue:TQValue;
  AIndex:Integer;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
AIndex:=AFunc.Parameters[1].Value.AsInteger;
if AValue.IsVar then
  AValue:=AValue.AsReference.Value;
if AValue.IsBytes then
  begin
  if (AIndex<AValue.ByteSize) and (AIndex>=0) then
    AFunc.Value.AsInteger:= PByte(PAnsiChar(AValue.AsBytes)+AIndex)^
  else
    ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,['SetByte','AIndex']));
  end
else
  ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
end;
procedure TQExpBytesFunctions.DoGetByteSize(ASender: TObject);
var
  AFunc:TQFunction;
  AValue:TQValue;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsVar then
  AValue:=AValue.AsReference.Value;
if AValue.IsBytes then
  AFunc.Value.AsInteger:=AValue.ByteSize
else
  ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
end;

//Bytes.Append(ABytesVar,ABytesValue);
procedure TQExpBytesFunctions.DoAppendBytes(ASender: TObject);
var
  AFunc:TQFunction;
  AValue,ABytes:TQValue;
  S:AnsiString;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsReference then
  AValue:=AValue.AsReference.Value;
ABytes:=AFunc.Parameters[1].Value;
if ABytes.IsReference then
  ABytes:=ABytes.AsReference.Value;
if AValue.IsBytes then
  begin
  SetLength(S,AValue.ByteSize+ABytes.ByteSize);
  CopyMemory(PAnsiChar(S),AValue.AsBytes,AValue.ByteSize);
  CopyMemory(PAnsiChar(S)+AValue.ByteSize,ABytes.AsBytes,ABytes.ByteSize);
  AValue.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
  end
else
  ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
end;
//FromHex(AHexStr)
procedure TQExpBytesFunctions.DoFromHex(ASender: TObject);
var
  AFunc:TQFunction;
  S:AnsiString;
begin
AFunc:=ASender as TQFunction;
S:=StrToBytes(AFunc.Parameters[0].Value.AsString);
AFunc.Value.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
end;
//FromStr(AStr,AEncode,ABOMNeeded)
procedure TQExpBytesFunctions.DoFromStr(ASender: TObject);
var
  AFunc:TQFunction;
  AStr:WideString;
  AEncode:Integer;
  ABOMNeeded:Boolean;
  S:AnsiString;
  L:Integer;
  p:PAnsiChar;
begin
AFunc:=ASender as TQFunction;
AStr:=AFunc.Parameters[0].Value.AsString;
AEncode:=AFunc.Parameters[1].Value.AsInteger;
ABOMNeeded:=AFunc.Parameters[2].Value.AsBoolean;
if ABOMNeeded then
  begin
  if AEncode=CP_UTF8 then
    begin
    SetLength(S,WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),nil,0,nil,nil)+3);
    p:=PAnsiChar(S);
    p[0]:=#$EF;
    p[1]:=#$BB;
    p[2]:=#$BF;
    Inc(p,3);
    WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),p,Length(S)-3,nil,nil);
    AFunc.Value.SetAsBytes(PByte(p-3),Length(S));
    end
  else if AEncode=1200 then//CP_UTF16
    begin
    AStr:=#$FFFE+AStr;
    AFunc.Value.SetAsBytes(PByte(PWideChar(AStr)),Length(AStr) shl 1);
    end
  else
    begin
    SetLength(S,WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),nil,0,nil,nil));
    WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),PAnsiChar(S),Length(S),nil,nil);
    AFunc.Value.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
    end;
  end
else
  begin
  if AEncode=1200 then
    AFunc.Value.SetAsBytes(PByte(PWideChar(AStr)),Length(AStr) shl 1)
  else
    begin
    SetLength(S,WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),nil,0,nil,nil));
    WideCharToMultiByte(AEncode,0,PWideChar(AStr),Length(AStr),PAnsiChar(S),Length(S),nil,nil);
    AFunc.Value.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
    end;
  end;
end;
//Bytes.Insert(AVar,AIndex,ABytes)
procedure TQExpBytesFunctions.DoInsertBytes(ASender: TObject);
var
  AFunc:TQFunction;
  ABytes,AValue:TQValue;
  AIndex:Integer;
  S:AnsiString;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsVar then
  begin
  AValue:=AValue.AsReference.Value;
  if AValue.IsBytes then
    begin
    AIndex:=AFunc.Parameters[1].Value.AsInteger;
    if (AIndex>=0) and (AIndex<=AValue.ByteSize) then
      begin
      ABytes:=AFunc.Parameters[2].Value;
      if ABytes.IsReference then
        ABytes:=ABytes.AsReference.Value;
      SetLength(S,AValue.ByteSize+ABytes.ByteSize);
      if AIndex>0 then
        CopyMemory(PAnsiChar(S),AValue.AsBytes,AIndex);
      CopyMemory(PAnsiChar(S)+AIndex,ABytes.AsBytes,ABytes.ByteSize);
      if AIndex<AValue.ByteSize then
        CopyMemory(PAnsiChar(S)+AIndex+ABytes.ByteSize,PAnsiChar(ABytes.AsBytes)+AIndex+ABytes.ByteSize,AValue.ByteSize-AIndex);
      AValue.SetAsBytes(PByte(PAnsiChar(S)),Length(S));
      AFunc.Value.Assign(AValue);
      end
    else
      ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,['Bytes.Insert','AIndex']));
    end
  else
    ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
  end
else
  ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
end;

procedure TQExpBytesFunctions.DoLoadBytes(ASender: TObject);
var
  AFunc:TQFunction;
  AStream:TMemoryStream;
  AFileName:WideString;
begin
AFunc:=ASender as TQFunction;
AFileName:=AFunc.Parameters[0].Value.AsString;
if FileExists(AFileName) then
  begin
  AStream:=TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    AFunc.Value.SetAsBytes(PByte(AStream.Memory),AStream.Size);
  finally
    AStream.Free;
  end;
  end
else
  ParserError(EPARSER_BAD_TYPE,EMSG_BAD_TYPE);
end;

procedure TQExpBytesFunctions.DoReadText(ASender: TObject);
var
  AFunc:TQFunction;
  AValue:TQValue;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
if AValue.IsReference then
  AValue:=AValue.AsReference.Value;
if AValue.IsBytes then
  AFunc.Value.AsString:=DecodeText(PAnsiChar(AValue.AsBytes),AValue.ByteSize);
end;

procedure TQExpBytesFunctions.DoSaveBytes(ASender: TObject);
var
  AFunc:TQFunction;
  AStream:TMemoryStream;
  AFileName:WideString;
  AValue:TQValue;
begin
AFunc:=ASender as TQFunction;
AFileName:=AFunc.Parameters[0].Value.AsString;
AValue:=AFunc.Parameters[1].Value;
if AValue.IsReference then
  AValue:=AValue.AsReference.Value;
AStream:=TMemoryStream.Create;
try
  AStream.Write(AValue.AsBytes^,AValue.ByteSize);
  AStream.SaveToFile(AFileName);
finally
  AStream.Free;
end;
end;

procedure TQExpBytesFunctions.DoSetByte(ASender: TObject);
var
  AFunc:TQFunction;
  AValue:TQValue;
  AIndex,AByteVal:Integer;
begin
AFunc:=ASender as TQFunction;
AValue:=AFunc.Parameters[0].Value;
AIndex:=AFunc.Parameters[1].Value.AsInteger;
AByteVal:=AFunc.Parameters[2].Value.AsInteger;
if AValue.IsVar then
  AValue:=AValue.AsReference.Value;
if AValue.IsBytes then
  begin
  if (AIndex<AValue.ByteSize) and (AIndex>=0) then
    begin
    if AByteVal in [0..255] then
      PByte(PAnsiChar(AValue.AsBytes)+AIndex)^:=AByteVal
    else
      ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,['SetByte','AValue']));
    end
  else
    ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,['SetByte','AIndex']));
  end;
end;

procedure TQExpBytesFunctions.Register(AParser: TQExprParser);
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    RegisterFunction(AParser,'Bytes.FromHex',DoFromHex,'AByteStr');
    RegisterFunction(AParser,'Bytes.Insert',DoInsertBytes,'AVar,AIndex,AInserted');
    RegisterFunction(AParser,'Bytes.Append',DoAppendBytes,'AVar,AInserted');
    RegisterFunction(AParser,'Bytes.FromStr',DoFromStr,'AStr,ATargetEncode,ABOMNeeded');
    AParser.AddConst('Charset_Utf8',CP_UTF8);
    AParser.AddConst('Charset_Utf16',1200);
    AParser.AddConst('Charset_Default',CP_ACP);
    RegisterFunction(AParser,'Bytes.SaveToFile',DoSaveBytes,'AFileName,ABytes');
    RegisterFunction(AParser,'Bytes.LoadFromFile',DoLoadBytes,'AFileName');
    RegisterFunction(AParser,'Bytes.SetByte',DoSetByte,'AVar,AIndex,AValue');
    RegisterFunction(AParser,'Bytes.GetByte',DoGetByte,'AVar,AIndex');
    RegisterFunction(AParser,'Bytes.ByteSize',DoGetByteSize,'AVar');
    RegisterFunction(AParser,'Bytes.ReadText',DoReadText,'AVar');
  finally
    AParser.EndRegister;
  end;
  end;
end;

{ TUtilFunctions }

constructor TUtilFunctions.Create;
begin
inherited;
end;

destructor TUtilFunctions.Destroy;
begin

  inherited;
end;

procedure TUtilFunctions.DoMsgBox(ASender: TObject);
var
  AFunc:TQFunction;
  ATitle,AMsg:WideString;
  AButtons:Integer;
  ATimeout:Integer;
begin
AFunc:=ASender as TQFunction;
AMsg:=AFunc.Parameters[0].Value.AsString;
if AFunc.Parameters[1].Value.IsNull then
  ATitle:=Forms.Application.Title
else
  ATitle:=AFunc.Parameters[1].Value.AsString;
if AFunc.Parameters[2].Value.IsNull then
  AButtons:=MB_OK
else
  AButtons:=AFunc.Parameters[2].Value.AsInteger;
if AFunc.Parameters[3].Value.IsNull then
  ATimeout:=-1
else
  ATimeout:=AFunc.Parameters[3].Value.AsInteger;
AFunc.Value.AsInteger:=MessageBoxTimeoutW(Forms.Application.Handle,PWideChar(AMsg),PWideChar(ATitle),AButtons,0,ATimeout);
end;

procedure TUtilFunctions.DoBreak(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Owner.FBreakCurrent:=True;
end;

procedure TUtilFunctions.DoChar(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsString:=WideChar(AFunc.Parameters[0].Value.AsInteger);
end;

procedure TUtilFunctions.DoDateTime(ASender: TObject);
begin
TQFunction(ASender).Value.AsDateTime:=TQFunction(ASender).Parameters[0].Value.AsDateTime;
end;

procedure TUtilFunctions.DoString(ASender: TObject);
begin
TQFunction(ASender).Value.AsString:=TQFunction(ASender).Parameters[0].Value.AsString;
end;

procedure TUtilFunctions.DoWhile(ASender: TObject);
var
  AFunc:TQFunction;
  ACondition,AExpr,APrior:TQValue;
  ALastIP:Cardinal;
begin
AFunc:=ASender as TQFunction;
ACondition:=AFunc.Parameters[0].Value;
ALastIP:=AFunc.Owner.FIP;
APrior:=nil;
while ACondition.AsBoolean and (not AFunc.Owner.FBreakCurrent) do
  begin
  Inc(AFunc.Owner.FIP);
  AExpr:=AFunc.Parameters[1].Value;
  while AExpr.IsReference do
    AExpr:=AExpr.AsReference.Value;
  if not AExpr.IsNull then
    APrior:=AExpr;
  end;
AFunc.Owner.FBreakCurrent:=False;
AFunc.Owner.FIP:=ALastIP;
if Assigned(APrior) then
  AFunc.Value.Assign(APrior);
end;

procedure TUtilFunctions.DoEval(ASender: TObject);
var
  AFunc:TQFunction;
  AScript:TQExprParser;
begin
AFunc:=ASender as TQFunction;
AScript:=TQExprParserClass(AFunc.Owner.ClassType).Create;
try
  AScript.Data:=AFunc.Owner;
  AScript.OnVarNeeded:=DoEvalVarNeeded;
  AScript.OnFunctionNeeded:=DoEvalFunctionNeeded;
  AScript.AfterExecStatement:=AFunc.Owner.AfterExecStatement;
  AScript.BeforeExecStatement:=AFunc.Owner.BeforeExecStatement;
  AScript.Parse(AFunc.Parameters[0].Value.AsString);
  AFunc.Value.Assign(AScript.Value);
finally
  AScript.Free;
end;
end;

procedure TUtilFunctions.DoEvalFunctionNeeded(ASender: TQExprParser;
  AName: WideString; var AResult: TQVar);
begin
AResult:=TQExprParser(ASender.Data).FunctionByName(AName);
end;

procedure TUtilFunctions.DoEvalVarNeeded(ASender: TQExprParser;
  AName: WideString; var AResult: TQVar);
begin
AResult:=TQEXprParser(ASender.Data).VarByName(AName);
end;

procedure TUtilFunctions.DoExit(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
//需要找到当前活动的解释器对象
AFunc.Owner.FLineNo:=$FFFFFFFF;
AFunc.Value.AsInteger:=AFunc.Parameters[0].Value.AsInteger;
end;

procedure TUtilFunctions.DoFloat(ASender: TObject);
begin
TQFunction(ASender).Value.AsFloat:=TQFunction(ASender).Parameters[0].Value.AsFloat;
end;

procedure TUtilFunctions.DoGoto(ASender: TObject);
var
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
//需要找到当前活动的解释器对象
AFunc.Owner.FLineNo:=AFunc.Parameters[0].Value.AsInteger;
end;

procedure TUtilFunctions.DoIfThen(ASender: TObject);
var
  P1,P2,P3:TQValue;
  AFunc:TQFunction;
begin
AFunc:=ASender as TQFunction;
P1:=AFunc.Parameters[0].Value;
P2:=AFunc.Parameters[1].Value;
P3:=AFunc.Parameters[2].Value;
if P1.AsBoolean then
  AFunc.Value.Assign(P2)
else
  AFunc.Value.Assign(P3);
end;

procedure TUtilFunctions.DoInt64(ASender: TObject);
begin
TQFunction(ASender).Value.AsInt64:=TQFunction(ASender).Parameters[0].Value.AsInt64;
end;

procedure TUtilFunctions.DoInteger(ASender: TObject);
begin
TQFunction(ASender).Value.AsInteger:=TQFunction(ASender).Parameters[0].Value.AsInteger;
end;

procedure TUtilFunctions.Register(AParser: TQExprParser);
begin
if Assigned(AParser) then
  begin
  AParser.BeginRegister;
  try
    RegisterFunction(AParser,'Int',DoInteger,'AExp');
    RegisterFunction(AParser,'Int64',DoInt64,'AExp');
    RegisterFunction(AParser,'Float',DoFloat,'AExp');
    RegisterFunction(AParser,'DateTime',DoDateTime,'AExp');
    RegisterFunction(AParser,'String',DoString,'AExp');
    RegisterFunction(AParser,'Char',DoChar,'AUnicode');
    RegisterFunction(AParser,'Eval',DoEval,'AExp');
    RegisterFunction(AParser,'MessageBox',DoMsgBox,'AMsg,[ATitle],[AButtons],[ATimeout]');
    AParser.AddConst('MB_OK',MB_OK);
    AParser.AddConst('MB_OKCANCEL',MB_OKCANCEL);
    AParser.AddConst('MB_ABORTRETRYIGNORE',MB_ABORTRETRYIGNORE);
    AParser.AddConst('MB_YESNOCANCEL',MB_YESNOCANCEL);
    AParser.AddConst('MB_YESNO',MB_YESNO);
    AParser.AddConst('MB_RETRYCANCEL',MB_RETRYCANCEL);
    AParser.AddConst('MB_ICONHAND',MB_ICONHAND);
    AParser.AddConst('MB_ICONQUESTION',MB_ICONQUESTION);
    AParser.AddConst('MB_ICONEXCLAMATION',MB_ICONEXCLAMATION);
    AParser.AddConst('MB_ICONASTERISK',MB_ICONASTERISK);
    AParser.AddConst('MB_USERICON',MB_USERICON);
    AParser.AddConst('MB_ICONWARNING',MB_ICONWARNING);
    AParser.AddConst('MB_ICONERROR',MB_ICONERROR);
    AParser.AddConst('MB_ICONINFORMATION',MB_ICONINFORMATION);
    AParser.AddConst('MB_ICONSTOP',MB_ICONSTOP);
    AParser.AddConst('MB_DEFBUTTON1',MB_DEFBUTTON1);
    AParser.AddConst('MB_DEFBUTTON2',MB_DEFBUTTON2);
    AParser.AddConst('MB_DEFBUTTON3',MB_DEFBUTTON3);
    AParser.AddConst('MB_DEFBUTTON4',MB_DEFBUTTON4);
    AParser.AddConst('MB_APPLMODAL',MB_APPLMODAL);
    AParser.AddConst('MB_SYSTEMMODAL',MB_SYSTEMMODAL);
    AParser.AddConst('MB_TASKMODAL',MB_TASKMODAL);
    AParser.AddConst('MB_HELP',MB_HELP);
    AParser.AddConst('MB_NOFOCUS',MB_NOFOCUS);
    AParser.AddConst('MB_SETFOREGROUND',MB_SETFOREGROUND);
    AParser.AddConst('MB_DEFAULT_DESKTOP_ONLY',MB_DEFAULT_DESKTOP_ONLY);
    AParser.AddConst('MB_TOPMOST',MB_TOPMOST);
    AParser.AddConst('MB_RIGHT',MB_RIGHT);
    AParser.AddConst('MB_RTLREADING',MB_RTLREADING);
    AParser.AddConst('MB_SERVICE_NOTIFICATION',MB_SERVICE_NOTIFICATION);
    AParser.AddConst('MB_SERVICE_NOTIFICATION_NT3X',MB_SERVICE_NOTIFICATION_NT3X);
    AParser.AddConst('IDOK',IDOK);
    AParser.AddConst('ID_OK',ID_OK);
    AParser.AddConst('IDCANCEL',IDCANCEL);
    AParser.AddConst('ID_CANCEL',ID_CANCEL);
    AParser.AddConst('IDABORT',IDABORT);
    AParser.AddConst('ID_ABORT',ID_ABORT);
    AParser.AddConst('IDRETRY',IDRETRY);
    AParser.AddConst('ID_RETRY',ID_RETRY);
    AParser.AddConst('IDIGNORE',IDIGNORE);
    AParser.AddConst('ID_IGNORE',ID_IGNORE);
    AParser.AddConst('IDYES',IDYES);
    AParser.AddConst('ID_YES',ID_YES);
    AParser.AddConst('IDNO',IDNO);
    AParser.AddConst('ID_NO',ID_NO);
    AParser.AddConst('IDCLOSE',IDCLOSE);
    AParser.AddConst('ID_CLOSE',ID_CLOSE);
    AParser.AddConst('IDHELP',IDHELP);
    AParser.AddConst('ID_HELP',ID_HELP);
    AParser.AddConst('IDTRYAGAIN',IDTRYAGAIN);
    AParser.AddConst('IDCONTINUE',IDCONTINUE);
    RegisterFunction(AParser,'IfThen',DoIfThen,'ACondtion,AExp1,AExp2');
    RegisterFunction(AParser,'Goto',DoGoto,'ALineNo');
    RegisterFunction(AParser,'Exit',DoExit,'ACode');
    RegisterFunction(AParser,'While',DoWhile,'ACondtion,AExp');
    RegisterFunction(AParser,'Break',DoBreak,'');
  finally
    AParser.EndRegister;
  end;
  end;
end;

{ EParserError }

constructor EParserError.Create(ACode: Integer; AMsg: String);
begin
FErrorCode:=ACode;
inherited Create(AMsg);
end;

{ TQPasParser }

function TQPasParser.OperatorType(var p: PWideChar): TQExprOperator;
begin
Result:=oprNone;
if p[0]='+' then
  begin
  Inc(p);
  Result:=oprAdd;
  end
else if p[0]='-' then
  begin
  Inc(p);
  Result:=oprSub;
  end
else if p[0]='*' then
  begin
  Inc(p);
  Result:=oprMul;
  end
else if p[0]='/' then
  begin
  Inc(p);
  Result:=oprDiv;
  end
else if ((p[0]='d') or (p[0]='D')) and ((p[1]='i') or (p[1]='I')) and ((p[2]='v') or (p[2]='V')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,3);
    Result:=oprDivTrunc;
    end;
  end
else if ((p[0]='m') or (p[0]='M')) and ((p[1]='o') or (p[1]='O')) and ((p[2]='d') or (p[2]='D')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,3);
    Result:=oprMod;
    end;
  end
else if ((p[0]='a') or (p[0]='A')) and ((p[1]='n') or (p[1]='N')) and ((p[2]='d') or (p[2]='D')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,3);
    Result:=oprBitAnd;
    end;
  end
else if ((p[0]='o') or (p[0]='O')) and ((p[1]='r') or (p[1]='R')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,2);
    Result:=oprBitOr;
    end;
  end
else if ((p[0]='n') or (p[0]='N')) and ((p[1]='o') or (p[1]='O')) and ((p[2]='t') or (p[2]='T')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,3);
    Result:=oprBitNot;
    end;
  end
else if ((p[0]='x') or (p[0]='X')) and ((p[1]='o') or (p[1]='O')) and ((p[2]='r') or (p[2]='R')) then
  begin
  if CharIn(p[3],#9#10#13' ') then
    begin
    Inc(p,3);
    Result:=oprBitXor;
    end;
  end
else if p[0]='<' then
  begin
  if p[1]='=' then
    begin
    Result:=oprLessThanEqual;
    Inc(p,2);
    end
  else
    begin
    Result:=oprLessThan;
    Inc(p);
    end
  end
else if p[0]='>' then
  begin
  if p[1]='=' then
    begin
    Result:=oprGreatThanEqual;
    Inc(p,2);
    end
  else
    begin
    Result:=oprGreatThan;
    Inc(p);
    end;
  end
else if p[0]='=' then
  begin
  Result:=oprEqual;
  Inc(p);
  end
else if ((p[0]='s') or (p[0]='S')) or ((p[1]='h') or (p[1]='H')) then
  begin
  if (p[2]='r') or (p[2]='R') then
    begin
    if CharIn(p^,#9#10#13' ') then
      begin
      Inc(p,3);
      Result:=oprRShift;
      end
    end
  else if (p[2]='l') or (p[2]='L') then
    begin
    if CharIn(p^,#9#10#13' ') then
      begin
      Inc(p,3);
      Result:=oprLShift;
      end
    end;
  end
else if p[0]='(' then
  begin
  Inc(p);
  Result:=oprBlockStart;
  end
else if p[0]=')' then
  begin
  Result:=oprBlockEnd;
  Inc(p);
  end
else if p[0]=';' then
  begin
  Result:=oprStatementEnd;
  Inc(p);
  end
else if p[0]=',' then
  begin
  Result:=oprComma;
  Inc(p);
  end
else if (p[0]=':') and (p[1]='=') then
  begin
  Result:=oprAssign;
  Inc(p,2);
  end;
while CharIn(p^ ,#9#10#13' ') do
  Inc(p);
end;

{ TQScriptFunction }

procedure TQScriptFunction.Assign(AVar: TQVar);
var
  I:Integer;
  AParam:TQParameter;
  V:TQVar;
begin
inherited;
if AVar is TQScriptFunction then
  begin
  for I := 0 to ParamCount - 1 do
    begin
    AParam:=Parameters[I];
    V:=TQVar.Create(Owner);
    V.Name:=AParam.Name;
    V.OnGetValue:=DoGetParamValue;
    V.Data:=AParam;
    FExprParser.Add(V);
    end;
  FExprParser.Parse(TQScriptFunction(AVar).FExprParser.Text);
  end;
end;


constructor TQScriptFunction.Create(AOwner: TQExprParser);
var
  AFunc:TQFunction;
begin
inherited;
FExprParser:=TQExprParserClass(AOwner.ClassType).Create;
FExprParser.OnVarNeeded:=DoVarNeeded;
FExprParser.OnFunctionNeeded:=DoFunctionNeeded;
FExprParser.BeforeExecStatement:=Owner.BeforeExecStatement;
FExprParser.AfterExecStatement:=Owner.AfterExecStatement;
AFunc:=FExprParser.AddFunction('Params',DoGetParams);
AFunc.AddParam('AIndex',true);
FExprParser.AddFunction('ParamCount',DoGetParamCount);
end;

destructor TQScriptFunction.Destroy;
begin
FExprParser.Free;
inherited;
end;

procedure TQScriptFunction.DoFunctionNeeded(ASender: TQExprParser;
  AName: WideString; var AResult: TQVar);
begin
AResult:=Owner.FunctionByName(AName);
end;

procedure TQScriptFunction.DoGetParamCount(ASender: TObject);
var
  AFunc,AParent:TQFunction;
begin
AFunc:=ASender as TQFunction;
AFunc.Value.AsInteger:=ParamCount;
end;

procedure TQScriptFunction.DoGetParams(ASender: TObject);
var
  AFunc:TQFunction;
  AIndex:Integer;
begin
AFunc:=ASender as TQFunction;
AIndex:=AFunc.Parameters[0].Value.AsInteger;
if (AIndex<0) or (AIndex>ParamCount) then
  ParserError(EPARSER_OUT_OF_RANGE,Format(EMSG_OUT_OF_RANGE,['Params','AIndex']))
else
  AFunc.Value.Assign(Parameters[AIndex].Value)
end;

procedure TQScriptFunction.DoGetParamValue(ASender: TObject);
var
  AVar:TQVar;
begin
AVar:=ASender as TQVar;
AVar.Value.Assign(TQParameter(AVar.Data).Value);
end;

procedure TQScriptFunction.DoVarNeeded(ASender: TQExprParser; AName: WideString;
  var AResult: TQVar);
begin
AResult:=Owner.VarByName(AName);
end;

function TQScriptFunction.GetValue: TQValue;
begin
FExprParser.Calc;
if FExprParser.FBreakCurrent then
  Owner.FBreakCurrent:=True;
Result:=FExprParser.Value;
end;

procedure TQScriptFunction.Parse(var p:PWideChar);
var
  AName:WideString;
  AParam:TQParameter;
  AVar:TQVar;
begin
AName:=DecodeIdent(p);
if Length(AName)=0 then
  Name:=Owner.NextTempName('SF')
else
  Name:=AName;
if p^<>'(' then
  ParserError(EPARSER_NEED_CHAR,Format(EMSG_NEED_CHAR,['(']));
Inc(p);
while CharIn(p^,#9#10#13' ') do
  Inc(p);
//解析参数,如果是...，则意味着是可变参数
while (p^<>#0) and (p^<>')') do
  begin
  AName:=DecodeIdent(p);
  if p^=',' then
    Inc(p);
  while CharIn(p^,#9#10#13' ') do
    Inc(p);
  if AName<>'...' then
    begin
    AParam:=AddParam(AName,True);
    AVar:=TQVar.Create(FExprParser);
    AVar.Name:=AName;
    AVar.OnGetValue:=DoGetParamValue;
    AVar.Data:=AParam;
    FExprParser.Add(AVar);
    end
  else if p^<>')' then
    ParserError(EPARSER_NEED_CHAR,Format(EMSG_NEED_CHAR,[')']))
  else
    VarParams:=True;
  end;
if p^<>')' then
  ParserError(EPARSER_NEED_CHAR,Format(EMSG_NEED_CHAR,[')']));
Inc(p);
while CharIn(p^,#9#10#13' ') do
  Inc(p);
if p^<>'{' then
  ParserError(EPARSER_NEED_CHAR,Format(EMSG_NEED_CHAR,['{']));
Inc(p);
ParseBlock(p);
Owner.Locals.Add(Self);
end;

procedure TQScriptFunction.ParseBlock(var p: PWideChar);
var
  ps:PWideChar;
  ALeftCount:Integer;
begin
while CharIn(p^,#9#10#13' ') do
  Inc(p);
ps:=p;
ALeftCount:=1;
while (p^<>#0) and (ALeftCount>0) do
  begin
  if p^='{' then
    Inc(ALeftCount)
  else if p^='}' then
    begin
    Dec(ALeftCount);
    if ALeftCount=0 then
      continue;
    end;
  Inc(p);
  end;
if p^<>'}' then
  ParserError(EPARSER_NEED_CHAR,Format(EMSG_NEED_CHAR,['}']));
FExprParser.Parse(System.Copy(ps,0,p-ps));
Inc(p);
while CharIn(p^,#9#10#13' ') do
  Inc(p);
end;

{ TQExpGlobal }

procedure TQExpGlobal.BeginRegister;
begin
FLocker.Enter;
end;

constructor TQExpGlobal.Create;
begin
inherited Create;
FLocker:=TCriticalSection.Create;
end;

destructor TQExpGlobal.Destroy;
begin
FLocker.Free;
inherited;
end;

procedure TQExpGlobal.EndRegister;
begin
FLocker.Leave;
end;

function TQExpGlobal.FunctionByName(const AName: WideString): TQFunction;
begin
FLocker.Enter;
try
  Result:=inherited FunctionByName(AName);
finally
  FLocker.Leave;
end;
end;

function TQExpGlobal.VarByName(const AName: WideString): TQVar;
begin
FLocker.Enter;
try
  Result:=inherited VarByName(AName);
finally
  FLocker.Leave;
end;
end;

function TQExpGlobal.VarByPath(AName: WideString): TQVar;
begin
FLocker.Enter;
try
  Result:=inherited VarByPath(AName);
finally
  FLocker.Leave;
end;
end;

procedure TestCase_CNExp;
var
  AVar:TQVar;
  AExpr:TQExprParser;
begin
AExpr:=TQExprParser.Create;
AVar:=QExpGlobal.FunctionByName('IfThen').Copy(AExpr);
AVar.Name:='判断';
AExpr.Add(AVar);
AVar:=QExpGlobal.FunctionByName('While').Copy(AExpr);
AVar.Name:='循环';
AExpr.Add(AVar);
AVar:=QExpGlobal.FunctionByName('Break').Copy(AExpr);
AVar.Name:='中断';
AExpr.Add(AVar);
//中文表达式
AExpr.Parse(
  '计数器=0;'#13#10+
  '循环(计数器<100,'#13#10+
  ' 计数器++);'#13#10
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  '计数器=0;'#13#10+
  '循环(计数器<100,'#13#10+
  ' {'#13#10+
  ' 判断(计数器==50,中断(),计数器++);'#13#10+
  ' }'#13#10+
  ' );'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=50);
AExpr.Free;
end;

//测试用例
procedure QExp_TestCase;
var
  AExpr:TQExprParser;
begin
AExpr:=TQExprParser.Create;
//表达式测试用例

//C1:简单赋值

AExpr.Parse('a=100;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=100);

//C2:简单四则运算 +,-,*,/,**,%,\

AExpr.Parse('a=4+5;b=5-2;c=4*5;d=4/5;e=4.0/5;f=2**3;g=5%3;h=5.1\3');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=9);
assert(AExpr.VarByName('b').Value.AsInteger=3);
assert(AExpr.VarByName('c').Value.AsInteger=20);
assert(AExpr.VarByName('d').Value.AsInteger=0);
assert(SameValue(AExpr.VarByName('e').Value.AsFloat,0.8));
assert(SameValue(AExpr.VarByName('f').Value.AsFloat,8));
assert(AExpr.VarByName('g').Value.AsInteger=2);
assert(AExpr.VarByName('h').Value.AsInteger=1);
//C3:自运算符,++/--

AExpr.Parse('a=1;a++;a--;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=1);
AExpr.Parse('a=1;++a;--a;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=1);
//C4:比较运算符
//C4.1:<
AExpr.Parse('a=1;b=2;a<b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b<a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.2:<=
AExpr.Parse('a=1;b=2;a<=b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b<=a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.3:>
AExpr.Parse('a=1;b=2;b>a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;a>b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C4.4:>=
AExpr.Parse('a=1;b=2;a>=b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=2;b>=a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C4.5:==
AExpr.Parse('a=1;b=2;a==b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=1;b=1;a==b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C5:位运算符
//C5.1:|
AExpr.Parse('a=1;b=2;c=a|b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=3);
//C5.2:&
AExpr.Parse('a=1;b=2;c=a&b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=0);
//C5.3:^
AExpr.Parse('a=1;b=2;c=a^b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=3);
//C5.5:~
AExpr.Parse('a=1;b=~a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=-2);
//C6:逻辑运算
//C6.1:&&
AExpr.Parse('a=1;b=2;a&&b;');//任何非0整数值都被认为是true，所以a&&b=true
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=2;a&&b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);

//C6.2:||
AExpr.Parse('a=1;b=2;a||b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=2;a||b;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;b=0;a||b;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
//C6.3:!
AExpr.Parse('a=1;!a;');
AExpr.Calc;
assert(not AExpr.Result.Value.AsBoolean);
AExpr.Parse('a=0;!a;');
AExpr.Calc;
assert(AExpr.Result.Value.AsBoolean);
//C7:移位运算符
//C7.1:<<
AExpr.Parse('a=1;a<<1;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=2);
//C7.2:>>
AExpr.Parse('a=2;a>>1;');
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=1);
//C8:自赋值运算符
//C8.1:+=,-=,*=,/=,\=,%=
AExpr.Parse('a=1;a+=1;b=2;b-=1;c=3;c*=2;d=4;d/=2;e=5.0;e\=2;f=5;f%=2;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=2);
assert(AExpr.VarByName('b').Value.AsInteger=1);
assert(AExpr.VarByName('c').Value.AsInteger=6);
assert(AExpr.VarByName('d').Value.AsInteger=2);
assert(AExpr.VarByName('e').Value.AsInteger=2);
assert(AExpr.VarByName('f').Value.AsInteger=1);
AExpr.Calc;
//C8.2:&=,|=,^=,<<=,>>=
AExpr.Parse('a=1;a&=2;b=1;b|=2;c=1;c^=2;d=1;d<<=1;e=2;e>>=1;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsInteger=0);
assert(AExpr.VarByName('b').Value.AsInteger=3);
assert(AExpr.VarByName('c').Value.AsInteger=3);
assert(AExpr.VarByName('d').Value.AsInteger=2);
assert(AExpr.VarByName('e').Value.AsInteger=1);
//C9:括号提升优先级
AExpr.Parse('a=1;b=2;c=a+1*b;');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(AExpr.Value.AsInteger=3);
AExpr.Parse('a=1;b=2;c=(a+2)*b;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=6);
//C10:复杂连续表达式
AExpr.Parse('a=2*3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=10);
AExpr.Parse('a=1+3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=8);
AExpr.Parse('a=1+3*4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=13);
AExpr.Parse('a=1+2*3+4;');
AExpr.Calc;
assert(AExpr.Value.AsInteger=11);
//C11:函数调用
//C11.1:可变参数函数调用
AExpr.Parse('a=avg(1,3,5,7);');
AExpr.Calc;
assert(AExpr.Value.AsInteger=4);
//C11.2:参数中含表达式调用
AExpr.Parse('a=avg(1+3,5+7,9+11);');
//ShowMessage(AExpr.CompiledText);
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,(4+12+20)/3));
//C12:跳转
//C12.1:Goto
AExpr.Parse(
  'a=0;'+
  'a++;'+
  'ifthen(a<100,Goto(1),Goto(3));'+
  'Result=a;'
  );
AExpr.Calc;
assert(AExpr.Result.Value.AsInteger=100);
//C12.2:Exit
AExpr.Parse(
  'a=0;' +
  'ifthen(a<50,exit(1),exit(2));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=1);
AExpr.Parse(
  'a=50;' +
  'ifthen(a<50,exit(1),exit(2));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=2);
//C13.布尔字符串
AExpr.Parse('a=True;b=False;c=TRUE;d=FALSE;');
AExpr.Calc;
assert(AExpr.VarByName('a').Value.AsBoolean);
assert(not AExpr.VarByName('b').Value.AsBoolean);
assert(AExpr.VarByName('c').Value.AsBoolean);
assert(not AExpr.VarByName('d').Value.AsBoolean);
//C13.Other
//C13.1:String.Length
AExpr.Parse('Length("1234这是什么")/100.323');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,8/100.323));
//C13.2:String.SubString
AExpr.Parse('IfThen((13>=1)&&(13<=2),SubString("这是什么呀",5,4),"没有");');
AExpr.Calc;
assert(AExpr.Value.AsString='没有');
AExpr.Parse('IfThen(Pos("bc","abc",False)<>0,SubString("这是什么呀",5,4),"没有");');
AExpr.Calc;
assert(AExpr.Value.AsString='呀');
//C13.3:Round
AExpr.Parse('round(1.245,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.25));
AExpr.Parse('round(467.85,-1)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,467.9));
AExpr.Parse('round(1.255,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.26));
AExpr.Parse('round(225.5,2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,200));
AExpr.Parse('round(1.3333,-2)');
AExpr.Calc;
assert(SameValue(AExpr.Value.AsFloat,1.33));
//C13.4:Like
AExpr.Parse('Like("Hello,World","%ll_,%l%",True);');
AExpr.Calc;
assert(AExpr.Value.AsBoolean);
AExpr.Parse('Like("Hello,World","%ll_,%b%",True);');
AExpr.Calc;
assert(not AExpr.Value.AsBoolean);
//C13.4:Regex
{$IFDEF QEXP_REGEX}
//match
AExpr.Parse('RegexMatch("My home work is done by myself.","/my/i",1);');
AExpr.Calc;
assert(AExpr.Value.AsString='My');
//matchnext
AExpr.Parse('RegexMatch("My home work is done by myself.","/my/i",1);RegexMatchNext();');
AExpr.Calc;
assert(AExpr.Value.AsString='my');
//replace
AExpr.Parse('RegexReplace("My home work is done by myself.","/my/i","Your",1);');
AExpr.Calc;
assert(AExpr.Value.AsString='Your home work is done by myself.');
AExpr.Parse('RegexReplace("My home work is done by myself.","/my/i","Your",-1);');
AExpr.Calc;
assert(AExpr.Value.AsString='Your home work is done by Yourself.');
//split
AExpr.Parse('RegexSplit("My home is china.","\\s",-1,false,false);s1=RegexSplitText(0);s2=RegexSplitText(1);s3=RegexSplitText(2);s4=RegexSplitText(3);');
AExpr.Calc;
assert(AExpr.VarByName('s1').Value.AsString='My');
assert(AExpr.VarByName('s2').Value.AsString='home');
assert(AExpr.VarByName('s3').Value.AsString='is');
assert(AExpr.VarByName('s4').Value.AsString='china.');
{$ENDIF}
AExpr.Parse(
  'function Add(x,y)'#13#10+
  '{'#13#10+
  'Result=x+y;'#13#10+
  '}'#13#10+
  'a=100;'#13#10+
  'b=200;'#13#10+
  'Result=Add(a+1,b+2);'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=303);
AExpr.Parse('老猫="坏人";');
assert(AExpr.Value.AsString='坏人');
AExpr.Parse(
  'IfThen(1>2,'#13#10+
  '{1+2;},'#13#10+
  '{1-2;}'#13#10+
  ');');
assert(AExpr.Value.AsInteger=-1);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' a++);'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' IfThen(a>50,Break(),a++));'
  );
AExpr.Calc;
assert(AExpr.Value.AsInteger=51);
AExpr.Parse(
  'a=0;'#13#10+
  'while(a<100,'#13#10+
  ' {'#13#10+
  ' b=0;'#13#10+
  ' while(b<50,Break());'#13#10+
  ' a++;'#13#10+
  ' });'#13#10
  );
AExpr.Calc;
//ShowMessage(AExpr.VarByName('a').Value.AsString);
assert(AExpr.Value.AsInteger=100);
AExpr.Parse(
  'a=Eval("100+2**3");');
AExpr.Calc;
assert(AExpr.Value.AsInteger=108);
AExpr.Free;
TestCase_CNExp;
end;

initialization
  QExpGlobal:=TQExpGlobal.Create;
  DateFunctions:=TDateFunctions.Create;
  MathFunctions:=TMathFunctions.Create;
  OperatorFunctions:=TOperatorFunctions.Create;
  StringFunctions:=TStringFunctions.Create;
  UtilFunctions:=TUtilFunctions.Create;
  BytesFunction:=TQExpBytesFunctions.Create;
finalization
  DateFunctions.Free;
  MathFunctions.Free;
  OperatorFunctions.Free;
  StringFunctions.Free;
  UtilFunctions.Free;
  BytesFunction.Free;
  QExpGlobal.Free;
end.

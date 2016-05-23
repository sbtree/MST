// CodeGear C++Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Qexp.pas' rev: 11.00

#ifndef QexpHPP
#define QexpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Types.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Fmtbcd.hpp>	// Pascal unit
#include <Math.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Syncobjs.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Qexp
{
//-- type declarations -------------------------------------------------------
typedef TMetaClass* TQExprParserClass;

typedef TMetaClass* TQVarClass;

class DELPHICLASS EParserError;
class PASCALIMPLEMENTATION EParserError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
private:
	int FErrorCode;
	
public:
	__fastcall EParserError(int ACode, AnsiString AMsg)/* overload */;
	__property int ErrorCode = {read=FErrorCode, nodefault};
public:
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EParserError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EParserError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EParserError(int Ident, System::TVarRec const * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EParserError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EParserError(const AnsiString Msg, System::TVarRec const * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EParserError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EParserError(System::PResStringRec ResStringRec, System::TVarRec const * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EParserError(void) { }
	#pragma option pop
	
};


class DELPHICLASS TQValue;
class DELPHICLASS TQVar;
class PASCALIMPLEMENTATION TQValue : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Fmtbcd::TBcd __fastcall GetAsBcd();
	bool __fastcall GetAsBoolean(void);
	System::TDateTime __fastcall GetAsDateTime(void);
	double __fastcall GetAsFloat(void);
	int __fastcall GetAsInteger(void);
	WideString __fastcall GetAsString();
	void __fastcall SetAsBcd(const Fmtbcd::TBcd &Value);
	void __fastcall SetAsBoolean(const bool Value);
	void __fastcall SetAsDateTime(const System::TDateTime Value);
	void __fastcall SetAsFloat(const double Value);
	void __fastcall SetAsInteger(const int Value);
	void __fastcall SetAsString(const WideString Value);
	__int64 __fastcall GetAsInt64(void);
	void __fastcall SetAsInt64(const __int64 Value);
	void __fastcall DoBeforeChange(void);
	void __fastcall DoChange(void);
	TQVar* __fastcall GetAsReference(void);
	void __fastcall SetAsReference(const TQVar* Value);
	int __fastcall GetDataType(void);
	bool __fastcall GetIsReference(void);
	bool __fastcall GetIsFunction(void);
	bool __fastcall GetIsDateTime(void);
	bool __fastcall GetIsFloat(void);
	bool __fastcall GetIsInteger(void);
	bool __fastcall GetIsNumeric(void);
	bool __fastcall GetIsBoolean(void);
	WideString __fastcall GetText();
	bool __fastcall GetIsString(void);
	bool __fastcall GetIsVar(void);
	System::PByte __fastcall GetAsBytes(void);
	int __fastcall GetByteSize(void);
	bool __fastcall GetIsBytes(void);
	bool __fastcall GetIsNull(void);
	
protected:
	int FDataType;
	AnsiString FValue;
	Classes::TNotifyEvent FBeforeChange;
	Classes::TNotifyEvent FOnChange;
	TQValue* __fastcall GetTargetValue(TQValue* AVal);
	
public:
	__fastcall TQValue(void)/* overload */;
	virtual void __fastcall Assign(const TQValue* ASource);
	virtual void __fastcall Math_Add(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_Sub(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_Multiply(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_Div(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_DivTrunc(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_Mod(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Math_Power(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall ShiftLeft(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall ShiftRight(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Bit_And(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Bit_Or(const TQValue* V2, const TQValue* RetVal);
	virtual void __fastcall Bit_Not(TQValue* RetVal);
	virtual void __fastcall Bit_Xor(const TQValue* V2, const TQValue* RetVal);
	virtual bool __fastcall Logical_And(const TQValue* V2);
	virtual bool __fastcall Logical_Or(const TQValue* V2);
	virtual bool __fastcall Logical_Not(void);
	virtual bool __fastcall Compare_LT(const TQValue* V2);
	virtual bool __fastcall Compare_GT(const TQValue* V2);
	virtual bool __fastcall Compare_EQ(const TQValue* V2);
	virtual bool __fastcall Compare_LE(const TQValue* V2);
	virtual bool __fastcall Compare_GE(const TQValue* V2);
	void __fastcall Clear(void);
	void __fastcall SetAsBytes(System::PByte ABytes, int ALength);
	__property System::PByte AsBytes = {read=GetAsBytes};
	__property int ByteSize = {read=GetByteSize, nodefault};
	__property WideString AsString = {read=GetAsString, write=SetAsString};
	__property bool AsBoolean = {read=GetAsBoolean, write=SetAsBoolean, nodefault};
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, nodefault};
	__property __int64 AsInt64 = {read=GetAsInt64, write=SetAsInt64};
	__property Fmtbcd::TBcd AsBcd = {read=GetAsBcd, write=SetAsBcd};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property System::TDateTime AsDateTime = {read=GetAsDateTime, write=SetAsDateTime};
	__property TQVar* AsReference = {read=GetAsReference, write=SetAsReference};
	__property int DataType = {read=GetDataType, nodefault};
	__property bool IsReference = {read=GetIsReference, nodefault};
	__property bool IsFunction = {read=GetIsFunction, nodefault};
	__property bool IsInteger = {read=GetIsInteger, nodefault};
	__property bool IsNumeric = {read=GetIsNumeric, nodefault};
	__property bool IsFloat = {read=GetIsFloat, nodefault};
	__property bool IsBoolean = {read=GetIsBoolean, nodefault};
	__property bool IsNull = {read=GetIsNull, nodefault};
	__property bool IsDatetime = {read=GetIsDateTime, nodefault};
	__property bool IsString = {read=GetIsString, nodefault};
	__property bool IsVar = {read=GetIsVar, nodefault};
	__property bool IsBytes = {read=GetIsBytes, nodefault};
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent BeforeChange = {read=FBeforeChange, write=FBeforeChange};
	__property WideString Text = {read=GetText};
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TQValue(void) { }
	#pragma option pop
	
};


typedef TQVar* *PQVar;

typedef void __fastcall (__closure *TQVarEnumProc)(TQVar* ASender, TQVar* AChild, int AParam);

#pragma option push -b-
enum TQVarEnumOption { veoVariant, veoFunction, veoTemp, veoNest };
#pragma option pop

typedef Set<TQVarEnumOption, veoVariant, veoNest>  TQVarEnumOptions;

class DELPHICLASS TQExprParser;
class DELPHICLASS TQFunction;
class PASCALIMPLEMENTATION TQVar : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	TQVar* operator[](int AIndex) { return Items[AIndex]; }
	
protected:
	void *FData;
	WideString FName;
	WideString FDisplayName;
	TQVar* FParent;
	Classes::TList* FItems;
	TQValue* FValue;
	bool FReadOnly;
	Classes::TNotifyEvent FOnGetValue;
	Classes::TNotifyEvent FOnValueChange;
	bool FOnFetch;
	unsigned FFetchIP;
	TQExprParser* FOwner;
	bool FMutable;
	virtual TQValue* __fastcall GetValue(void);
	virtual void __fastcall DoValueChanging(System::TObject* ASender);
	virtual void __fastcall DoValueChange(System::TObject* ASender);
	virtual bool __fastcall InternalFind(const WideString AName, int &AIndex);
	virtual int __fastcall GetCount(void);
	virtual TQVar* __fastcall GetItems(int AIndex);
	WideString __fastcall GetPath();
	void __fastcall SetName(WideString Value);
	int __fastcall GetIndex(void);
	void __fastcall ValidateName(WideString S);
	WideString __fastcall GetText();
	WideString __fastcall GetDisplayName();
	void __fastcall SetDisplayName(const WideString Value);
	TQExprParser* __fastcall GetParser(void);
	__fastcall TQVar(void)/* overload */;
	
public:
	__fastcall virtual TQVar(TQExprParser* AOwner)/* overload */;
	__fastcall virtual ~TQVar(void);
	virtual TQVar* __fastcall Add(WideString AName, bool AMutable = false)/* overload */;
	TQFunction* __fastcall AddFunction(WideString AName, Classes::TNotifyEvent AHandler, WideString AFixedParams, bool AVarParams);
	TQVar* __fastcall ForcePath(WideString APath);
	int __fastcall ForEach(TQVarEnumProc AEnumProc, TQVarEnumOptions AOptions, int AParam);
	int __fastcall Enum(Classes::TList* AList, TQVarEnumOptions AOptions);
	virtual void __fastcall Add(TQVar* AVar)/* overload */;
	virtual void __fastcall Assign(TQVar* ASource);
	virtual void __fastcall Delete(int AIndex, bool AFreeNeeded = true)/* overload */;
	void __fastcall Delete(WideString AName, bool AFreeNeeded = true)/* overload */;
	virtual void __fastcall Clear(bool AFreeNeeded = true);
	virtual TQVar* __fastcall Copy(TQExprParser* AOwner);
	TQVar* __fastcall Find(WideString AName);
	TQVar* __fastcall FindByPath(WideString APath);
	int __fastcall IndexOf(WideString AName)/* overload */;
	int __fastcall IndexOf(const TQVar* AVar)/* overload */;
	__property WideString Name = {read=FName, write=SetName};
	__property WideString DisplayName = {read=GetDisplayName, write=SetDisplayName};
	__property WideString Path = {read=GetPath};
	__property TQVar* Parent = {read=FParent};
	__property int Count = {read=GetCount, nodefault};
	__property TQVar* Items[int AIndex] = {read=GetItems/*, default*/};
	__property TQValue* Value = {read=GetValue};
	__property Classes::TNotifyEvent OnValueChange = {read=FOnValueChange, write=FOnValueChange};
	__property Classes::TNotifyEvent OnGetValue = {read=FOnGetValue, write=FOnGetValue};
	__property int Index = {read=GetIndex, nodefault};
	__property void * Data = {read=FData, write=FData};
	__property WideString Text = {read=GetText};
	__property bool Mutable = {read=FMutable, write=FMutable, nodefault};
	__property TQExprParser* Owner = {read=FOwner};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, nodefault};
};


class DELPHICLASS TQExprStatement;
class PASCALIMPLEMENTATION TQExprStatement : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TList* FItems;
	TQExprParser* FOwner;
	int FLineNo;
	int FOffset;
	TQExprStatement* FParent;
	WideString FText;
	TQVar* FResult;
	unsigned __fastcall GetCount(void);
	TQExprStatement* __fastcall GetItems(int AIndex);
	TQVar* __fastcall GetResult(void);
	void __fastcall InternalParse(WideChar * &p, TQVar* AParent);
	
public:
	__fastcall TQExprStatement(TQExprParser* AOwner)/* overload */;
	__fastcall virtual ~TQExprStatement(void);
	void __fastcall Assign(TQExprStatement* ASource);
	TQExprStatement* __fastcall Add(int ALineNo, int AOffset);
	void __fastcall Clear(void);
	void __fastcall Parse(WideChar * &p);
	__property int LineNo = {read=FLineNo, nodefault};
	__property int Offset = {read=FOffset, nodefault};
	__property unsigned Count = {read=GetCount, nodefault};
	__property TQExprStatement* Items[int AIndex] = {read=GetItems};
	__property TQExprParser* Owner = {read=FOwner};
	__property TQExprStatement* Parent = {read=FParent};
	__property TQVar* Result = {read=GetResult};
};


typedef void __fastcall (__closure *TQExprVarNeededEvent)(TQExprParser* ASender, WideString AName, TQVar* &AResult);

typedef void __fastcall (__closure *TQExprExecuteEvent)(TQExprParser* ASender, TQExprStatement* AStatement);

typedef void __fastcall (__closure *TQExprAssignHelper)(TQExprParser* ASender, TQVar* AVar);

#pragma option push -b-
enum TQExprOperator { oprNone, oprComma, oprStatementEnd, oprAssign, oprSelfAdd, oprSelfSub, oprSelfMul, oprSelfDiv, oprSelfDivTrunc, oprSelfMod, oprSelfBitAnd, oprSelfBitOr, oprSelfBitXor, oprSelfLShift, oprSelfRShift, oprAnd, oprOr, oprNot, oprBitAnd, oprBitOr, oprBitXor, oprBitNot, oprEqual, oprNotEqual, oprLessThan, oprLessThanEqual, oprGreatThan, oprGreatThanEqual, oprLShift, oprRShift, oprAdd, oprSub, oprMul, oprDiv, oprDivTrunc, oprMod, oprPower, oprInc, oprDec, oprLineComment, oprBlockComment, oprBraceStart, oprBraceEnd, oprBlockStart, oprBlockEnd, oprMax = 44 };
#pragma option pop

class DELPHICLASS TQFunctions;
class PASCALIMPLEMENTATION TQExprParser : public TQVar 
{
	typedef TQVar inherited;
	
protected:
	TQVar* FResult;
	TQVar* FLocals;
	TQVar* FAliases;
	bool FAborted;
	bool FBreakCurrent;
	bool FAssigning;
	unsigned FVarIndex;
	unsigned FLineNo;
	unsigned FIP;
	WideChar *FParsingText;
	bool FExecuted;
	TQExprStatement* FStatements;
	WideString FText;
	TQExprVarNeededEvent FOnVarNeeded;
	TQExprVarNeededEvent FOnFunctionNeeded;
	TQExprExecuteEvent FAfterExecStatement;
	TQExprExecuteEvent FBeforeExecStatement;
	Classes::TNotifyEvent FBeforeExecute;
	Classes::TNotifyEvent FAfterExecute;
	TQExprAssignHelper FAssignHelper;
	virtual TQExprOperator __fastcall OperatorType(WideChar * &p);
	virtual TQValue* __fastcall GetValue(void);
	TQVar* __fastcall GetResult(void);
	WideString __fastcall GetCompiledText();
	void __fastcall DoVarAssigned(TQVar* AVar);
	
public:
	__fastcall TQExprParser(void)/* overload */;
	__fastcall virtual ~TQExprParser(void);
	HIDESBASE void __fastcall Assign(TQExprParser* ASource);
	TQVar* __fastcall AddConst(const WideString AName, const int AVal)/* overload */;
	TQVar* __fastcall AddConst(const WideString AName, const __int64 AVal)/* overload */;
	TQVar* __fastcall AddConst(const WideString AName, const double AVal)/* overload */;
	TQVar* __fastcall AddConst(const WideString AName, const System::TDateTime AVal)/* overload */;
	TQVar* __fastcall AddConst(const WideString AName, const WideString AVal)/* overload */;
	TQVar* __fastcall AddVar(const WideString AName, Classes::TNotifyEvent AOnExecute)/* overload */;
	HIDESBASE TQFunction* __fastcall AddFunction(const WideString AName, Classes::TNotifyEvent AOnExecute)/* overload */;
	virtual TQFunction* __fastcall FunctionByName(const WideString AName);
	virtual TQVar* __fastcall VarByName(const WideString AName);
	virtual TQVar* __fastcall VarByPath(WideString AName);
	WideString __fastcall NextTempName(WideString ALeader);
	void __fastcall AddAlias(const WideString ANewName, TQVar* ARef);
	void __fastcall RegisterFunctions(TQFunctions* AFunctions);
	void __fastcall Parse(const WideString S);
	void __fastcall Calc(void);
	virtual void __fastcall Clear(bool AFreeNeeded = true);
	virtual void __fastcall BeginRegister(void);
	virtual void __fastcall EndRegister(void);
	void __fastcall SetAssignHelper(TQExprAssignHelper AHelper);
	__property TQVar* Result = {read=GetResult};
	__property TQVar* Locals = {read=FLocals};
	__property WideString CompiledText = {read=GetCompiledText};
	__property bool Aborted = {read=FAborted, write=FAborted, nodefault};
	__property WideString Text = {read=FText, write=Parse};
	__property TQExprVarNeededEvent OnVarNeeded = {read=FOnVarNeeded, write=FOnVarNeeded};
	__property TQExprVarNeededEvent OnFunctionNeeded = {read=FOnFunctionNeeded, write=FOnFunctionNeeded};
	__property TQExprExecuteEvent BeforeExecStatement = {read=FBeforeExecStatement, write=FBeforeExecStatement};
	__property TQExprExecuteEvent AfterExecStatement = {read=FAfterExecStatement, write=FAfterExecStatement};
	__property Classes::TNotifyEvent BeforeExecute = {read=FBeforeExecute, write=FBeforeExecute};
	__property Classes::TNotifyEvent AfterExecute = {read=FAfterExecute, write=FAfterExecute};
};



class DELPHICLASS TQParameter;
class PASCALIMPLEMENTATION TQParameter : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	WideString FName;
	TQValue* FValue;
	TQFunction* FOwner;
	bool FFixed;
	int FIndex;
	
public:
	__fastcall TQParameter(TQFunction* AOwner)/* overload */;
	__fastcall virtual ~TQParameter(void);
	__property WideString Name = {read=FName};
	__property TQValue* Value = {read=FValue};
	__property bool Fixed = {read=FFixed, nodefault};
	__property int Index = {read=FIndex, nodefault};
};


class PASCALIMPLEMENTATION TQFunction : public TQVar 
{
	typedef TQVar inherited;
	
private:
	Classes::TList* FParams;
	bool FVarParams;
	int FFixedParamCount;
	int FMaxParamCount;
	int __fastcall GetParamCount(void);
	TQParameter* __fastcall GetParameters(int AIndex);
	WideString __fastcall GetDeclareText();
	WideString __fastcall GetCallText();
	void __fastcall ClearParams(void);
	
public:
	__fastcall virtual TQFunction(TQExprParser* AOwner)/* overload */;
	__fastcall virtual ~TQFunction(void);
	virtual void __fastcall Assign(TQVar* AVar);
	TQParameter* __fastcall AddParam(const WideString AName, bool AFixed = true);
	void __fastcall ClearVarParams(void);
	TQParameter* __fastcall ParamByName(const WideString AName);
	TQParameter* __fastcall FindParam(const WideString AName);
	void __fastcall Call(void);
	__property TQParameter* Parameters[int AIndex] = {read=GetParameters};
	__property int ParamCount = {read=GetParamCount, nodefault};
	__property WideString DeclareText = {read=GetDeclareText};
	__property WideString CallText = {read=GetCallText};
	__property bool VarParams = {read=FVarParams, write=FVarParams, nodefault};
	__property int FixedParamCount = {read=FFixedParamCount, nodefault};
	__property int MaxParamCount = {read=FMaxParamCount, write=FMaxParamCount, nodefault};
};


class DELPHICLASS TQOperatorFunction;
class PASCALIMPLEMENTATION TQOperatorFunction : public TQFunction 
{
	typedef TQFunction inherited;
	
private:
	TQExprOperator FBindOperator;
	
public:
	__fastcall virtual TQOperatorFunction(TQExprParser* AOwner)/* overload */;
	virtual void __fastcall Assign(TQVar* AVar);
	__property TQExprOperator BindOperator = {read=FBindOperator, write=FBindOperator, nodefault};
public:
	#pragma option push -w-inl
	/* TQFunction.Destroy */ inline __fastcall virtual ~TQOperatorFunction(void) { }
	#pragma option pop
	
};


class DELPHICLASS TQScriptFunction;
class PASCALIMPLEMENTATION TQScriptFunction : public TQFunction 
{
	typedef TQFunction inherited;
	
protected:
	TQExprParser* FExprParser;
	virtual TQValue* __fastcall GetValue(void);
	void __fastcall DoVarNeeded(TQExprParser* ASender, WideString AName, TQVar* &AResult);
	void __fastcall DoFunctionNeeded(TQExprParser* ASender, WideString AName, TQVar* &AResult);
	void __fastcall Parse(WideChar * &p);
	void __fastcall ParseBlock(WideChar * &p);
	void __fastcall DoGetParamValue(System::TObject* ASender);
	void __fastcall DoGetParams(System::TObject* ASender);
	void __fastcall DoGetParamCount(System::TObject* ASender);
	
public:
	__fastcall virtual TQScriptFunction(TQExprParser* AOwner)/* overload */;
	__fastcall virtual ~TQScriptFunction(void);
	virtual void __fastcall Assign(TQVar* AVar);
};


class DELPHICLASS TQPasParser;
class PASCALIMPLEMENTATION TQPasParser : public TQExprParser 
{
	typedef TQExprParser inherited;
	
protected:
	virtual TQExprOperator __fastcall OperatorType(WideChar * &p);
public:
	#pragma option push -w-inl
	/* TQExprParser.Create */ inline __fastcall TQPasParser(void)/* overload */ : TQExprParser() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TQExprParser.Destroy */ inline __fastcall virtual ~TQPasParser(void) { }
	#pragma option pop
	
};


class DELPHICLASS TQExpGlobal;
class PASCALIMPLEMENTATION TQExpGlobal : public TQExprParser 
{
	typedef TQExprParser inherited;
	
protected:
	Syncobjs::TCriticalSection* FLocker;
	
public:
	__fastcall TQExpGlobal(void)/* overload */;
	__fastcall virtual ~TQExpGlobal(void);
	virtual void __fastcall BeginRegister(void);
	virtual void __fastcall EndRegister(void);
	virtual TQVar* __fastcall VarByName(const WideString AName);
	virtual TQVar* __fastcall VarByPath(WideString AName);
	virtual TQFunction* __fastcall FunctionByName(const WideString AName);
};


class PASCALIMPLEMENTATION TQFunctions : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	TQFunction* __fastcall RegisterFunction(TQVar* AParser, WideString AName, Classes::TNotifyEvent AHandler, WideString AFixedParams, bool AVarParams = false);
	
public:
	__fastcall TQFunctions(void)/* overload */;
	__fastcall virtual ~TQFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser) = 0 ;
};


#pragma option push -b-
enum TDatePart { dpNone, dpYear, dpMonth, dpDay, dpHour, dpMinute, dpSecond, dpMSecond };
#pragma option pop

#pragma option push -b-
enum TDateRange { drNone, drCentury, drYear, drQuarter, drMonth, drDay, drHour, drMinute, drSecond };
#pragma option pop

class DELPHICLASS TDateFunctions;
class PASCALIMPLEMENTATION TDateFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
protected:
	TDatePart __fastcall DatePartOfName(const WideString AName);
	TDateRange __fastcall DateRangeOfName(const WideString AName);
	void __fastcall DoDateAdd(System::TObject* ASender);
	void __fastcall DoDateDiff(System::TObject* ASender);
	void __fastcall DoDatePart(System::TObject* ASender);
	void __fastcall DoNow(System::TObject* ASender);
	void __fastcall DoDate(System::TObject* ASender);
	void __fastcall DoTime(System::TObject* ASender);
	void __fastcall DoMonthDays(System::TObject* ASender);
	void __fastcall DoIsLeapYear(System::TObject* ASender);
	void __fastcall DoEncodeDate(System::TObject* ASender);
	void __fastcall DoEncodeTime(System::TObject* ASender);
	void __fastcall DoToday(System::TObject* ASender);
	void __fastcall DoYesterday(System::TObject* ASender);
	void __fastcall DoTomorrow(System::TObject* ASender);
	void __fastcall DoIsToday(System::TObject* ASender);
	void __fastcall DoDateStart(System::TObject* ASender);
	void __fastcall DoDateEnd(System::TObject* ASender);
	void __fastcall DoDateReplace(System::TObject* ASender);
	TDatePart __fastcall NeedDatePart(TQValue* AValue);
	TDateRange __fastcall NeedDateRange(TQValue* AValue);
	
public:
	__fastcall TDateFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
public:
	#pragma option push -w-inl
	/* TQFunctions.Destroy */ inline __fastcall virtual ~TDateFunctions(void) { }
	#pragma option pop
	
};


class DELPHICLASS TMathFunctions;
class PASCALIMPLEMENTATION TMathFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
protected:
	void __fastcall DoAvg(System::TObject* ASender);
	void __fastcall DoSum(System::TObject* ASender);
	void __fastcall DoMax(System::TObject* ASender);
	void __fastcall DoMin(System::TObject* ASender);
	void __fastcall DoCount(System::TObject* ASender);
	void __fastcall DoStdDev(System::TObject* ASender);
	void __fastcall DoRound(System::TObject* ASender);
	void __fastcall DoBankRound(System::TObject* ASender);
	
public:
	__fastcall TMathFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
public:
	#pragma option push -w-inl
	/* TQFunctions.Destroy */ inline __fastcall virtual ~TMathFunctions(void) { }
	#pragma option pop
	
};


class DELPHICLASS TOperatorFunctions;
class PASCALIMPLEMENTATION TOperatorFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
protected:
	void __fastcall DoAssign(System::TObject* ASender);
	void __fastcall DoMathAdd(System::TObject* ASender);
	void __fastcall DoMathSub(System::TObject* ASender);
	void __fastcall DoMathMultiply(System::TObject* ASender);
	void __fastcall DoMathDiv(System::TObject* ASender);
	void __fastcall DoMathMod(System::TObject* ASender);
	void __fastcall DoMathPower(System::TObject* ASender);
	void __fastcall DoBitAnd(System::TObject* ASender);
	void __fastcall DoBitOr(System::TObject* ASender);
	void __fastcall DoBitNot(System::TObject* ASender);
	void __fastcall DoBitXor(System::TObject* ASender);
	void __fastcall DoLogicalAnd(System::TObject* ASender);
	void __fastcall DoLogicalOr(System::TObject* ASender);
	void __fastcall DoLogicalNot(System::TObject* ASender);
	void __fastcall DoCompareLT(System::TObject* ASender);
	void __fastcall DoCompareGT(System::TObject* ASender);
	void __fastcall DoCompareEQ(System::TObject* ASender);
	void __fastcall DoCompareNE(System::TObject* ASender);
	void __fastcall DoCompareLE(System::TObject* ASender);
	void __fastcall DoCompareGE(System::TObject* ASender);
	void __fastcall DoShiftLeft(System::TObject* ASender);
	void __fastcall DoShiftRight(System::TObject* ASender);
	void __fastcall DoDivTrunc(System::TObject* ASender);
	void __fastcall DoAddToSelf(System::TObject* ASender);
	void __fastcall DoSubToSelf(System::TObject* ASender);
	void __fastcall DoMulToSelf(System::TObject* ASender);
	void __fastcall DoDivToSelf(System::TObject* ASender);
	void __fastcall DoModToSelf(System::TObject* ASender);
	void __fastcall DoAndToSelf(System::TObject* ASender);
	void __fastcall DoOrToSelf(System::TObject* ASender);
	void __fastcall DoXorToSelf(System::TObject* ASender);
	void __fastcall DoLShiftToSelf(System::TObject* ASender);
	void __fastcall DoRShiftToSelf(System::TObject* ASender);
	void __fastcall DoDivTruncToSelf(System::TObject* ASender);
	void __fastcall DoIncrement(System::TObject* ASender);
	void __fastcall DoDecrement(System::TObject* ASender);
	
public:
	__fastcall TOperatorFunctions(void)/* overload */;
	__fastcall virtual ~TOperatorFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
};


class DELPHICLASS TStringFunctions;
class PASCALIMPLEMENTATION TStringFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
private:
	void __fastcall DoLeft(System::TObject* ASender);
	void __fastcall DoRight(System::TObject* ASender);
	void __fastcall DoUpper(System::TObject* ASender);
	void __fastcall DoLower(System::TObject* ASender);
	void __fastcall DoStrCmp(System::TObject* ASender);
	void __fastcall DoQuotedStr(System::TObject* ASender);
	void __fastcall DoDequoteStr(System::TObject* ASender);
	void __fastcall DoPos(System::TObject* ASender);
	void __fastcall DoLength(System::TObject* ASender);
	void __fastcall DoSubString(System::TObject* ASender);
	void __fastcall DoLike(System::TObject* ASender);
	void __fastcall DoStartWith(System::TObject* ASender);
	void __fastcall DoEndWith(System::TObject* ASender);
	void __fastcall DoDecodeToken(System::TObject* ASender);
	
public:
	__fastcall TStringFunctions(void)/* overload */;
	__fastcall virtual ~TStringFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
};


class DELPHICLASS TQExpBytesFunctions;
class PASCALIMPLEMENTATION TQExpBytesFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
protected:
	void __fastcall DoGetByte(System::TObject* ASender);
	void __fastcall DoSetByte(System::TObject* ASender);
	void __fastcall DoGetByteSize(System::TObject* ASender);
	void __fastcall DoFromHex(System::TObject* ASender);
	void __fastcall DoInsertBytes(System::TObject* ASender);
	void __fastcall DoAppendBytes(System::TObject* ASender);
	void __fastcall DoSaveBytes(System::TObject* ASender);
	void __fastcall DoLoadBytes(System::TObject* ASender);
	void __fastcall DoFromStr(System::TObject* ASender);
	void __fastcall DoReadText(System::TObject* ASender);
	
public:
	__fastcall TQExpBytesFunctions(void)/* overload */;
	__fastcall virtual ~TQExpBytesFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
};


class DELPHICLASS TUtilFunctions;
class PASCALIMPLEMENTATION TUtilFunctions : public TQFunctions 
{
	typedef TQFunctions inherited;
	
private:
	void __fastcall DoInteger(System::TObject* ASender);
	void __fastcall DoInt64(System::TObject* ASender);
	void __fastcall DoFloat(System::TObject* ASender);
	void __fastcall DoDateTime(System::TObject* ASender);
	void __fastcall DoString(System::TObject* ASender);
	void __fastcall DoChar(System::TObject* ASender);
	void __fastcall DoMsgBox(System::TObject* ASender);
	void __fastcall DoIfThen(System::TObject* ASender);
	void __fastcall DoGoto(System::TObject* ASender);
	void __fastcall DoExit(System::TObject* ASender);
	void __fastcall DoWhile(System::TObject* ASender);
	void __fastcall DoBreak(System::TObject* ASender);
	void __fastcall DoEval(System::TObject* ASender);
	void __fastcall DoEvalVarNeeded(TQExprParser* ASender, WideString AName, TQVar* &AResult);
	void __fastcall DoEvalFunctionNeeded(TQExprParser* ASender, WideString AName, TQVar* &AResult);
	
public:
	__fastcall TUtilFunctions(void)/* overload */;
	__fastcall virtual ~TUtilFunctions(void);
	virtual void __fastcall Register(TQExprParser* AParser);
};


#pragma option push -b-
enum TOperatorParamPosition { oppNone, oppLeft, oppRight, oppBoth, oppAny };
#pragma option pop

#pragma pack(push,4)
struct TQOperatorInfo
{
	
public:
	WideString Text;
	int Pri;
	TOperatorParamPosition ParamPos;
	TQOperatorFunction* OnExecute;
} ;
#pragma pack(pop)

typedef TQOperatorInfo QExp__91[45];

//-- var, const, procedure ---------------------------------------------------
static const Shortint VARTYPE_NULL = 0x0;
static const Shortint VARTYPE_STRING = 0x1;
static const Shortint VARTYPE_BYTES = 0x2;
static const int VARTYPE_FUNCTION = 0x10000001;
static const int VARTYPE_VARNAME = 0x10000002;
static const int VARTYPE_NUMERIC_MASK = 0x20000000;
static const int VARTYPE_INTEGER_MASK = 0x1000000;
static const int VARTYPE_HEX_MASK = 0x2000000;
static const int VARTYPE_FLOAT_MASK = 0x4000000;
static const int VARTYPE_BCD_MASK = 0x8000000;
static const int VARTYPE_DATETIME_MASK = 0x100000;
static const int VARTYPE_BOOLEAN = 0x21000000;
static const int VARTYPE_INTEGER = 0x21000001;
static const int VARTYPE_INT64 = 0x21000002;
static const int VARTYPE_CHEX = 0x23000001;
static const int VARTYPE_DHEX = 0x23000002;
static const int VARTYPE_FLOAT = 0x24000001;
static const int VARTYPE_DATE = 0x24100002;
static const int VARTYPE_TIME = 0x24100003;
static const int VARTYPE_DATETIME = 0x24100005;
static const int VARTYPE_NUMERIC = 0x28000009;
extern PACKAGE TQOperatorInfo Operators[45];
extern PACKAGE TQExprParser* QExpGlobal;
static const int EPARSER_USER = 0x10000000;
static const Shortint EPARSER_CHAR_UNEXPECT = 0x1;
static const Shortint EPARSER_UNSUPPORT_OPRERATOR = 0x2;
static const Shortint EPARSER_DIVBYZERO = 0x3;
static const Shortint EPARSER_NOMEAN_VALUE = 0x4;
static const Shortint EPARSER_NAME_EXISTS = 0x5;
static const Shortint EPARSER_NAME_EMPTY = 0x6;
static const Shortint EPARSER_NAME_DOT = 0x7;
static const Shortint EPARSER_PARAM_MISSED = 0x8;
static const Shortint EPARSER_BAD_DATEPART = 0x9;
static const Shortint EPARSER_BAD_MONTH = 0xa;
static const Shortint EPARSER_BAD_DATERANGE = 0xb;
static const Shortint EPARSER_PARAM_ATLEASTONE = 0xc;
static const Shortint EPARSER_LEFT_VAR_NEEDED = 0xd;
static const Shortint EPARSER_BAD_TOKEN = 0xe;
static const Shortint EPARSER_PARAM_TOOMANY = 0xf;
static const Shortint EPARSER_PARAM_NOT_END = 0x10;
static const Shortint EPARSER_FUNC_MISSED = 0x11;
static const Shortint EPARSER_OPERATOR_NOTIMPL = 0x12;
static const Shortint EPARSER_OPERATOR_NOPARAM = 0x13;
static const Shortint EPARSER_OPERATOR_PARAM_NEEDINTEGER = 0x14;
static const Shortint EPARSER_CIRCULAR_REF = 0x15;
static const Shortint EPARSER_OUT_OF_RANGE = 0x16;
static const Shortint EPARSER_MIN_MAX = 0x17;
static const Shortint EPARSER_BAD_TYPE_VALUE = 0x18;
static const Shortint EPARSER_MAX_UNSUPPORT = 0x19;
static const Shortint EPARSER_MIN_UNSUPPORT = 0x1a;
static const Shortint EPARSER_VAR_NEEDED = 0x1b;
static const Shortint EPARSER_BAD_MATCH_STATE = 0x1c;
static const Shortint EPARSER_BAD_REGEX_EXP = 0x1d;
static const Shortint EPARSER_PARAM_TOOFEW = 0x1e;
static const Shortint EPARSER_BAD_NAME = 0x1f;
static const Shortint EPARSER_NEED_CHAR = 0x20;
static const Shortint EPARSER_CONST_READONLY = 0x21;
static const Shortint EPARSER_BAD_TYPE = 0x22;
static const Shortint EPARSER_BAD_PARAM_ORDER = 0x23;
static const Shortint EPARSER_FUNCTION_NEEDCALL = 0x24;
static const Shortint EPARSER_NEED_END = 0x25;
extern PACKAGE System::ResourceString _EMSG_CHAR_UNEXPECT;
#define Qexp_EMSG_CHAR_UNEXPECT System::LoadResourceString(&Qexp::_EMSG_CHAR_UNEXPECT)
extern PACKAGE System::ResourceString _EMSG_UNSUPPORT_OPERATOR;
#define Qexp_EMSG_UNSUPPORT_OPERATOR System::LoadResourceString(&Qexp::_EMSG_UNSUPPORT_OPERATOR)
extern PACKAGE System::ResourceString _EMSG_DIVBYZERO;
#define Qexp_EMSG_DIVBYZERO System::LoadResourceString(&Qexp::_EMSG_DIVBYZERO)
extern PACKAGE System::ResourceString _EMSG_NOMEAN_VALUE;
#define Qexp_EMSG_NOMEAN_VALUE System::LoadResourceString(&Qexp::_EMSG_NOMEAN_VALUE)
extern PACKAGE System::ResourceString _EMSG_NAME_EXISTS;
#define Qexp_EMSG_NAME_EXISTS System::LoadResourceString(&Qexp::_EMSG_NAME_EXISTS)
extern PACKAGE System::ResourceString _EMSG_NAME_EMPTY;
#define Qexp_EMSG_NAME_EMPTY System::LoadResourceString(&Qexp::_EMSG_NAME_EMPTY)
extern PACKAGE System::ResourceString _EMSG_NAME_DOT;
#define Qexp_EMSG_NAME_DOT System::LoadResourceString(&Qexp::_EMSG_NAME_DOT)
extern PACKAGE System::ResourceString _EMSG_PARAM_MISSED;
#define Qexp_EMSG_PARAM_MISSED System::LoadResourceString(&Qexp::_EMSG_PARAM_MISSED)
extern PACKAGE System::ResourceString _EMSG_BAD_DATEPART;
#define Qexp_EMSG_BAD_DATEPART System::LoadResourceString(&Qexp::_EMSG_BAD_DATEPART)
extern PACKAGE System::ResourceString _EMSG_BAD_MONTH;
#define Qexp_EMSG_BAD_MONTH System::LoadResourceString(&Qexp::_EMSG_BAD_MONTH)
extern PACKAGE System::ResourceString _EMSG_BAD_DATERANGE;
#define Qexp_EMSG_BAD_DATERANGE System::LoadResourceString(&Qexp::_EMSG_BAD_DATERANGE)
extern PACKAGE System::ResourceString _EMSG_PARAM_ATLEASTONE;
#define Qexp_EMSG_PARAM_ATLEASTONE System::LoadResourceString(&Qexp::_EMSG_PARAM_ATLEASTONE)
extern PACKAGE System::ResourceString _EMSG_LEFT_VAR_NEEDED;
#define Qexp_EMSG_LEFT_VAR_NEEDED System::LoadResourceString(&Qexp::_EMSG_LEFT_VAR_NEEDED)
extern PACKAGE System::ResourceString _EMSG_BAD_TOKEN;
#define Qexp_EMSG_BAD_TOKEN System::LoadResourceString(&Qexp::_EMSG_BAD_TOKEN)
extern PACKAGE System::ResourceString _EMSG_PARAM_TOOMANY;
#define Qexp_EMSG_PARAM_TOOMANY System::LoadResourceString(&Qexp::_EMSG_PARAM_TOOMANY)
extern PACKAGE System::ResourceString _EMSG_PARAM_NOT_END;
#define Qexp_EMSG_PARAM_NOT_END System::LoadResourceString(&Qexp::_EMSG_PARAM_NOT_END)
extern PACKAGE System::ResourceString _EMSG_FUNC_MISSED;
#define Qexp_EMSG_FUNC_MISSED System::LoadResourceString(&Qexp::_EMSG_FUNC_MISSED)
extern PACKAGE System::ResourceString _EMSG_OPERATOR_NOTIMPL;
#define Qexp_EMSG_OPERATOR_NOTIMPL System::LoadResourceString(&Qexp::_EMSG_OPERATOR_NOTIMPL)
extern PACKAGE System::ResourceString _EMSG_OPERATOR_NOPARAM;
#define Qexp_EMSG_OPERATOR_NOPARAM System::LoadResourceString(&Qexp::_EMSG_OPERATOR_NOPARAM)
extern PACKAGE System::ResourceString _EMSG_OPERATOR_PARAM_NEEDINTEGER;
#define Qexp_EMSG_OPERATOR_PARAM_NEEDINTEGER System::LoadResourceString(&Qexp::_EMSG_OPERATOR_PARAM_NEEDINTEGER)
extern PACKAGE System::ResourceString _EMSG_CIRCULAR_REF;
#define Qexp_EMSG_CIRCULAR_REF System::LoadResourceString(&Qexp::_EMSG_CIRCULAR_REF)
extern PACKAGE System::ResourceString _EMSG_OUT_OF_RANGE;
#define Qexp_EMSG_OUT_OF_RANGE System::LoadResourceString(&Qexp::_EMSG_OUT_OF_RANGE)
extern PACKAGE System::ResourceString _EMSG_MIN_MAX;
#define Qexp_EMSG_MIN_MAX System::LoadResourceString(&Qexp::_EMSG_MIN_MAX)
extern PACKAGE System::ResourceString _EMSG_BAD_TYPE_VALUE;
#define Qexp_EMSG_BAD_TYPE_VALUE System::LoadResourceString(&Qexp::_EMSG_BAD_TYPE_VALUE)
extern PACKAGE System::ResourceString _EMSG_MAX_UNSUPPORT;
#define Qexp_EMSG_MAX_UNSUPPORT System::LoadResourceString(&Qexp::_EMSG_MAX_UNSUPPORT)
extern PACKAGE System::ResourceString _EMSG_MIN_UNSUPPORT;
#define Qexp_EMSG_MIN_UNSUPPORT System::LoadResourceString(&Qexp::_EMSG_MIN_UNSUPPORT)
extern PACKAGE System::ResourceString _EMSG_VAR_NEEDED;
#define Qexp_EMSG_VAR_NEEDED System::LoadResourceString(&Qexp::_EMSG_VAR_NEEDED)
extern PACKAGE System::ResourceString _EMSG_BAD_MATCH_STATE;
#define Qexp_EMSG_BAD_MATCH_STATE System::LoadResourceString(&Qexp::_EMSG_BAD_MATCH_STATE)
extern PACKAGE System::ResourceString _EMSG_BAD_REGEX_EXP;
#define Qexp_EMSG_BAD_REGEX_EXP System::LoadResourceString(&Qexp::_EMSG_BAD_REGEX_EXP)
extern PACKAGE System::ResourceString _EMSG_PARAM_TOOFEW;
#define Qexp_EMSG_PARAM_TOOFEW System::LoadResourceString(&Qexp::_EMSG_PARAM_TOOFEW)
extern PACKAGE System::ResourceString _EMSG_BAD_NAME;
#define Qexp_EMSG_BAD_NAME System::LoadResourceString(&Qexp::_EMSG_BAD_NAME)
extern PACKAGE System::ResourceString _EMSG_NEED_CHAR;
#define Qexp_EMSG_NEED_CHAR System::LoadResourceString(&Qexp::_EMSG_NEED_CHAR)
extern PACKAGE System::ResourceString _EMSG_CONST_READONLY;
#define Qexp_EMSG_CONST_READONLY System::LoadResourceString(&Qexp::_EMSG_CONST_READONLY)
extern PACKAGE System::ResourceString _EMSG_BAD_TYPE;
#define Qexp_EMSG_BAD_TYPE System::LoadResourceString(&Qexp::_EMSG_BAD_TYPE)
extern PACKAGE System::ResourceString _EMSG_BAD_PARAM_ORDER;
#define Qexp_EMSG_BAD_PARAM_ORDER System::LoadResourceString(&Qexp::_EMSG_BAD_PARAM_ORDER)
extern PACKAGE System::ResourceString _EMSG_FUNCTION_NEEDCALL;
#define Qexp_EMSG_FUNCTION_NEEDCALL System::LoadResourceString(&Qexp::_EMSG_FUNCTION_NEEDCALL)
extern PACKAGE System::ResourceString _EMSG_NEED_END;
#define Qexp_EMSG_NEED_END System::LoadResourceString(&Qexp::_EMSG_NEED_END)
extern PACKAGE void __fastcall ParserError(int ACode, AnsiString AMsg);
extern PACKAGE bool __fastcall CharIn(const WideChar c, WideChar * s);
extern PACKAGE bool __fastcall DecodeQuotedStr(WideChar * &p, WideString &AResult);
extern PACKAGE WideString __fastcall EscapeText(WideString S);
extern PACKAGE WideString __fastcall UnescapeText(WideString S);
extern PACKAGE WideString __fastcall ExtractVarPath(WideString ANamePath);
extern PACKAGE WideString __fastcall ExtractVarName(WideString ANamePath);
extern PACKAGE WideString __fastcall BytesToStr(System::PByte ABytes, int AByteSize);
extern PACKAGE AnsiString __fastcall StrToBytes(WideString S);
extern PACKAGE int __fastcall DetectTextType(WideChar * p);
extern PACKAGE System::TDateTime __fastcall DetectDateTime(WideChar * p);
extern PACKAGE WideString __fastcall DecodeToken(WideChar * &p, WideChar * ADelimiters, WideChar AQuoteChar, bool AIgnoreSpace = true);
extern PACKAGE WideString __fastcall DecodeText(char * p, int l);
extern PACKAGE void __fastcall QExp_TestCase(void);

}	/* namespace Qexp */
using namespace Qexp;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Qexp

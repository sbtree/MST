//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdio.h>

#pragma hdrstop

#include "main.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
FParser=new TQExprParser;
}
void __fastcall TForm1::LogResult()
{
TList *AList=new TList;
String S;
TQVar *AVar;
try
	{
	if (FParser->Result->Enum(AList,TQVarEnumOptions()<<veoVariant<<veoNest))
		{
		S="Result:";
		for(int i=0;i<AList->Count;i++)
			{
			AVar=(TQVar*)AList->Items[i];
			S+=AVar->Name+"=";
			if(AVar->Value->IsNull)
				S+="<NULL>,";
			else
				S+=AVar->Value->AsString+",";
			}
		}
	else
		S="Result:"+FParser->Value->AsString;
	S=S+"\r\nLocals:";
	AList->Clear();
	FParser->Locals->Enum(AList,TQVarEnumOptions()<<veoVariant<<veoNest);
	for(int i=0;i<AList->Count;i++)
		{
		AVar=(TQVar*)AList->Items[i];
		S+=AVar->Name+"=";
		if(AVar->Value->IsNull)
			S+="<NULL>,";
		else
			S+=AVar->Value->AsString+",";
        }
	lblResult->Caption=S.SubString(1,S.Length()-1);
	}
__finally
	{
	delete AList;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
mmScript->Text="a=1;\r\n"
	"a+=1;\r\n"
	"b=2;\r\n"
	"b-=1;\r\n"
	"c=3;\r\n"
	"c*=2;\r\n"
	"d=4;\r\n"
	"d/=2;\r\n"
	"e=5.0;\r\n"
	"e\=2;\r\n"
	"f=5;\r\n"
	"f%=2;\r\n"
	"g=1;\r\n"
	"g&=2;\r\n"
	"h=1;\r\n"
	"h|=2;\r\n"
	"i=1;\r\n"
	"i^=2;\r\n"
	"j=1;\r\n"
	"j<<=1;\r\n"
	"k=4;\r\n"
	"k>>1;";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormDestroy(TObject *Sender)
{
delete FParser;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
mmScript->Text="a=4+5;\r\nb=5-2;\r\nc=4*5;\r\nd=4/5;\r\ne=4.0/5;\r\nf=2**3;\r\ng=5%3;\r\nh=5.1\\3;\r\ni=0;\r\ni++;\r\nj=1;\r\nj--;\r\nk=1;\r\n++k;\r\nl=1;\r\n--l;";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------


void __fastcall TForm1::Button3Click(TObject *Sender)
{
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
mmScript->Text="a=1;\r\nb=2;\r\nc=2;\r\nd=a<b;\r\n"
	"e=b<a;\r\n"
	"f=a==b;\r\n"
	"g=a>=b;\r\n"
	"h=a<=b;\r\n"
	"i=b==c;\r\n"
	"j=a!=b;\r\n"
	"k=a<>b;\r\n";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button5Click(TObject *Sender)
{
mmScript->Text="a=1;\r\nb=2;\r\n"
	"c=a|b;\r\n"
	"d=a&b;\r\n"
	"e=a^b;\r\n"
	"f=~a;\r\n"
	"g=a<<2;\r\n"
	"h=b>>1;\r\n";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button6Click(TObject *Sender)
{
mmScript->Text="a=True;\r\nb=False;\r\n"
	"c=a&&b;\r\n"
	"d=a||b;\r\n"
	"e=!a;\r\n"
	"f=!b;\r\n"
	"g=1;\r\n"
	"h=0;\r\n"
	"i=a&&b;\r\n"
	"j=a||b;\r\n"
	"k=!a;\r\n"
	"l=!b;";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button7Click(TObject *Sender)
{
mmScript->Text="a=1;\r\n"
	"b=2;\r\n"
	"c=(a+2)*b+5*4-3;";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button8Click(TObject *Sender)
{
mmScript->Text="a=Like('Hello,World','%ll_,%l%',True);";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button9Click(TObject *Sender)
{
mmScript->Text="function add(x,y)\r\n"
	"{\r\n"
	"Result=x+y;\r\n"
	"}\r\n"
	"a=add(1,3);";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button10Click(TObject *Sender)
{
mmScript->Text="function 求合(x,y)\r\n"
	"{\r\n"
	"Result=x+y;\r\n"
	"}\r\n"
	"合=求合(1,3);";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button11Click(TObject *Sender)
{
mmScript->Text="a={\r\n"
	"c=1;\r\n"
	"c<<4;\r\n"
	"}\r\n";
FParser->Parse(mmScript->Text);
LogResult();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button12Click(TObject *Sender)
{
TList *AList=new TList;
try
	{
	QExpGlobal->Enum(AList,TQVarEnumOptions()<<veoFunction<<veoNest);
	String S,T;
	TQVar *AVar;;
	int L=0;
	for(int i=0;i<AList->Count;i++)
		{
		AVar=(TQVar *)AList->Items[i];
		if (dynamic_cast<TQFunction *>(AVar))
			{
			S=S+IntToStr(L)+"."+((TQFunction *)AVar)->DeclareText+"\r\n";
			L++;
			}
		}
	TForm *F=new TForm(Application);
	TMemo *AMemo=new TMemo(F);
	AMemo->Parent=F;
	AMemo->Align=alClient;
	AMemo->ScrollBars=ssBoth;
	AMemo->Text=S;
	F->Caption="Internal Functions";
	F->Position=poMainFormCenter;
	F->ShowModal();
	delete F;
	}
__finally
	{
	delete AList;
    }
}
//---------------------------------------------------------------------------


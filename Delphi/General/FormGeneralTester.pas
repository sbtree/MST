unit FormGeneralTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataBuffer, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmGeneralTester = class(TForm)
    btnTest: TButton;
    imgTest: TImage;
    trvDevices: TTreeView;
    btnCreateTree: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCreateTreeClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmGeneralTester: TfrmGeneralTester;

implementation

{$R *.dfm}

procedure TfrmGeneralTester.btnCreateTreeClick(Sender: TObject);
var
  MyTreeNode1, MyTreeNode2: TTreeNode;
begin
  with trvDevices.Items do
  begin
    Clear; { remove any existing nodes }
    MyTreeNode1 := Add(nil, 'RootTreeNode1'); { Add a root node }
    { Add a child node to the node just added }
    AddChild(MyTreeNode1,'ChildNode1');

    {Add another root node}
    MyTreeNode2 := Add(MyTreeNode1, 'RootTreeNode2');
    {Give MyTreeNode2 to a child }
    AddChild(MyTreeNode2,'ChildNode2');

    {Change MyTreeNode2 to ChildNode2 }
    { and add a child node to it}
    MyTreeNode2 := trvDevices.Items[3];
    AddChild(MyTreeNode2,'ChildNode2a');

    {Add another child to ChildNode2, after ChildNode2a }
    Add(MyTreeNode2,'ChildNode2b');

    {add another root node}
    Add(MyTreeNode1, 'RootTreeNode3');
  end;
end;

procedure TfrmGeneralTester.btnTestClick(Sender: TObject);
var chbuffer: TCharBuffer;  t: cardinal; n:integer;
begin
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  //ShowMessage(format('imgTest.ClientRect=[%d, %d, %d, %d]',
  //          [imgTest.ClientRect.Left, imgTest.ClientRect.Top, imgTest.ClientRect.Right, imgTest.ClientRect.Bottom]));
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);

  chbuffer := TCharBuffer.Create();
  chbuffer.Resize(10);
  t:=GetTickCount;
  n:=chbuffer.WriteHex('19084091483758913478faf214f');
  //chbuffer.Resize(14);
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('2.ReadHex=%s',[chbuffer.ReadHex()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryHex=%s', [chbuffer.HistoryHex()]));
  t:=GetTickCount;
  n:=chbuffer.WriteStr('987654321');
  t:=GetTickCount - t;
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('1.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('2.ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('n=%d, t=%d, CountUsed=%d, CountFree=%d', [n,t,chbuffer.CountUsed(), chbuffer.CountFree()]));
  ShowMessage(format('HistoryStr=%s', [chbuffer.HistoryStr()]));


  for n := $30 to $39 do chbuffer.WriteChar(char(n));
  ShowMessage(format('ReadStr=%s',[chbuffer.ReadStr()]));
  ShowMessage(format('HistoryStr=%s',[chbuffer.HistoryStr()]));
  ShowMessage(format('HistoryHex=%s',[chbuffer.HistoryHex()]));

  FreeAndNil(chbuffer);
end;

procedure TfrmGeneralTester.FormCreate(Sender: TObject);
begin
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo_BuR.bmp');
  //imgTest.Picture.LoadFromFile('N:\SW_INBE\DIS2_230_SW3.2\Kundenlogos\Logo-Lenze.bmp');
  //imgTest.Picture.Bitmap.Canvas.StretchDraw (imgTest.ClientRect, imgTest.Picture.Bitmap);
end;

end.

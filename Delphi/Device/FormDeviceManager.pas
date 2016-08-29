unit FormDeviceManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ComCtrls, TextMessage;

type
  TfrmDeviceManager = class(TForm)
    btnFR: TButton;
    trvDevices: TTreeView;
    btnCreateTree: TButton;
    procedure btnFRClick(Sender: TObject);
    procedure btnCreateTreeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    t_msgr : TTextMessenger;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation

{$R *.dfm}
uses DeviceBase, Multimeter;

procedure TfrmDeviceManager.btnCreateTreeClick(Sender: TObject);
var
  MyTreeNode1, MyTreeNode2: TTreeNode; tNodes: TTreeNodes;
begin
  //tNodes := TTreeNodes.Create(Nil);
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
  //trvDevices.Items := tNodes

end;

procedure TfrmDeviceManager.btnFRClick(Sender: TObject);
var dmm: TMultimeter;
begin
  dmm := TMultimeter.Create(self);
  ITextMessengerImpl(dmm).Messenger := t_msgr;
  dmm.Free();
  {fIni := TMemIniFile.Create('FlashRunner.ini');
  fr := TFlashRunner.Create(self);
  
  bOk := fr.ConfigDevice(fIni);
  bOk := fr.Connect();
  bOk := fr.SetDynamicMem(0,12345);
  bOk := fr.RunScript('bt_24v.frs', 30000);
  FreeAndNil(fr);
  FreeAndNil(fIni);}
end;

procedure TfrmDeviceManager.FormCreate(Sender: TObject);
begin
  t_msgr := TTextMessenger.Create();
end;

procedure TfrmDeviceManager.FormDestroy(Sender: TObject);
begin
  t_msgr.Free();
end;

end.

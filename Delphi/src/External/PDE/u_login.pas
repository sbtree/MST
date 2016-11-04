unit u_login;
// =============================================================================
// =============================================================================
//
//     Module name       : $RCSfile: u_login.pas,v $
//     Short description : Login-Fenster
//     Compiler          : Delphi 2007
//     First author      : 2010-10-21 /aha/
//     Actual version    : $Revision 1.0$
//     Copyright         : (c) Metronix GmbH 2010
// -----------------------------------------------------------------------------
//     History           :
// -----------------------------------------------------------------------------
//     Descriptions
// =============================================================================

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  Tfo_login = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ed_user: TEdit;
    ed_password: TEdit;
    but_OK: TBitBtn;
    BitBtn2: TBitBtn;
    procedure but_OKClick(Sender: TObject);
    procedure ed_Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    procedure test_enable_OK_button();
  end;


function do_login ( var s_usr, s_pass : string ) : boolean;


implementation

{$R *.dfm}

var
  s_fo_user            : string;
  s_fo_pass            : string;



// =============================================================================
//    Class        : Tfo_login
//    Function     : but_OKClick : OK-Button geklickt
//
//    Parameter    : Delphi automatic
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010
//    History      : --
// =============================================================================
procedure Tfo_login.but_OKClick(Sender: TObject);
begin
  s_fo_user :=    ed_user.text;
  s_fo_pass :=    ed_password.text;
end;


// =============================================================================
//    Class        : Tfo_login
//    Function     : ed_change : Passwort oder User verändert
//                   ==> prüfen, ob OK-Button noch enabled sein darf
//    Parameter    : Delphi automatic
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-12-02
//    History      : --
// =============================================================================
procedure Tfo_login.ed_Change(Sender: TObject);
begin
  test_enable_OK_button();
end;

// =============================================================================
//    Class        : Tfo_login
//    Function     : enable_OK_button ==> Prüfung, ob OK-Button enabled sein darf
//                   nur wenn bei user und Passwort was steht!
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-12-02
//    History      : --
// =============================================================================
procedure Tfo_login.test_enable_OK_button();
begin
  but_OK.enabled := ( ed_user.Text <>''      ) and
                    ( ed_password.Text <> '' );
end;


// =============================================================================
//    Class        : --
//    Function     : do_login : Login-Dialog abarbeiten
//
//    Parameter    : s_usr    : eingetragener Nutzername
//                   s_pass   : eingetragenes Passwort
//    Return       : TRUE     : Nutzer ist mit OKAY rausgegangen
//                   FALSE    : Nutzer hat abgebrochen
//    Exceptions   : --
//    First author : /aha/ 2010-10-21
//    History      : /aha/ User und Passwort werden übernommen
// =============================================================================
function do_login ( var s_usr, s_pass : string ) : boolean;
var
  fo_login: Tfo_login;

begin

  fo_login := Tfo_login.Create ( application );

  fo_login.ed_user.Text       := s_usr;
  fo_login.ed_password.Text   := s_pass;

  fo_login.test_enable_OK_button();

  if ( fo_login.ShowModal = mrOK ) then
  begin
    s_usr     := s_fo_user;
    s_pass    := s_fo_pass;
    result    := TRUE;
  end
  else
  begin
    s_usr     := '';
    s_pass    := '';
    result := FALSE;
  end;
end;


end.

unit Example.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  CAPTCHA;

type

{ TfrmMain }
  TfrmMain = class(TForm)
    btnVerify: TButton;
    edtInput: TEdit;
    imgCAPTCHA: TImage;
    lblInfo: TLabel;
    procedure btnVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgCAPTCHAClick(Sender: TObject);
  private
    FCAPTCHA: TCAPTCHA;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
, LCLIntf
;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnVerifyClick(Sender: TObject);
begin
  if FCAPTCHA.Validate(edtInput.Text) then
  begin
    ShowMessage('Validation Sucsess');
    FCAPTCHA.RefreshBitmap;
    imgCAPTCHA.Picture.Assign(FCAPTCHA.Image);
  end
  else
    ShowMessage('Validation Fail');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FCAPTCHA:= TCAPTCHA.Create;
  imgCAPTCHA.Picture.Assign(FCAPTCHA.Image);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCAPTCHA);
end;

procedure TfrmMain.imgCAPTCHAClick(Sender: TObject);
begin
  FCAPTCHA.RefreshBitmap;
  imgCAPTCHA.Picture.Assign(FCAPTCHA.Image);
end;

end.


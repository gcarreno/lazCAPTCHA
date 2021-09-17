{ Implements Form Main

Copyright (c) 2021 Gustavo Carreno <guscarreno@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

}
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


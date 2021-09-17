{ Implements CAPTCHA

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
unit CAPTCHA;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, LCLType
, LCLIntf
, Graphics
;

type
{ TCAPTCHA }
  TCAPTCHA = class(TObject)
  type
    TCharCase = (Lower, Upper, Number);
    TCharCases = set of TCharCase;

  const
    cWidth = 300;
    cHeight = 75;
    cCharCaseAll = [Lower, Upper, Number];
    cCharCaseLetter = [Lower, Upper];

  private
    FCAPTCHAString: string;
    FCAPTCHABitmap: TBitmap;
    FCharCase: TCharCases;

    function GenerateCAPTCHAString: string;
    procedure DrawLetter(ch: Char; angle, nextPos: Integer);
    procedure DrawLines(aLineCount: Integer = 15);
  protected
  public
    constructor Create(ACharCase: TCharCases = [Upper, Number]);
    destructor Destroy; override;

    procedure RefreshBitmap;
    function Validate(const AValue: string;
      ACaseSensetive: Boolean = True): Boolean;
    property Image: TBitmap read FCAPTCHABitmap;
  published
  end;

implementation

uses
  GraphType
;

{ TCAPTCHA }

function TCAPTCHA.GenerateCAPTCHAString: string;
const
  NoOfChars = 10;
var
  validChar: String;
  i: Integer;
begin
  Result:= EmptyStr;
  validChar:= EmptyStr;
  if TCharCase.Number in FCharCase then
    validChar:= validChar + '123456789';

  if TCharCase.Lower in FCharCase then
    validChar:= validChar + 'abcdefghijklmnopqrstuvwxyz';

  if TCharCase.Upper in FCharCase then
    validChar:= validChar + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  SetLength(Result, NoOfChars);

  for i:= 1 to NoOfChars do
    Result[i]:= validChar[Random(Length(validChar)) + 1];
end;

procedure TCAPTCHA.DrawLetter(ch: Char; angle, nextPos: Integer);
var
  logFont: TLogFont;
  fontHandle: THandle;
begin
  logFont.lfheight:= 40;
  logFont.lfwidth:= 20;
  logFont.lfweight:= 900;

  logFont.lfEscapement:= angle;
  logFont.lfcharset:= 1;
  logFont.lfoutprecision:= OUT_TT_ONLY_PRECIS;
  logFont.lfquality:= DEFAULT_QUALITY;
  logFont.lfpitchandfamily:= FF_SWISS;
  logFont.lfUnderline:= 0;
  logFont.lfStrikeOut:= 0;

  fontHandle:= CreateFontIndirect(logFont);
  SelectObject(FCAPTCHABitmap.Canvas.Handle, fontHandle);

  SetTextColor(FCAPTCHABitmap.Canvas.Handle, rgb(0, 180, 0));
  SetBKmode(FCAPTCHABitmap.Canvas.Handle, TRANSPARENT);

  SetTextColor(FCAPTCHABitmap.Canvas.Handle, Random(MAXWORD));
  FCAPTCHABitmap.Canvas.TextOut(nextPos, FCAPTCHABitmap.Height div 3, ch);
  DeleteObject(fontHandle);
end;

procedure TCAPTCHA.DrawLines(aLineCount: Integer);
var
  i: Integer;
begin
  for i:= 0 to aLineCount do
  begin
    FCAPTCHABitmap.Canvas.Pen.Color:= Random(MAXWORD);
    FCAPTCHABitmap.Canvas.MoveTo(Random(FCAPTCHABitmap.Width), Random(FCAPTCHABitmap.Height));
    FCAPTCHABitmap.Canvas.LineTo(Random(FCAPTCHABitmap.Width), Random(FCAPTCHABitmap.Height));
  end;
end;

procedure TCAPTCHA.RefreshBitmap;
var
  i: Integer;
begin
  FreeAndNil(FCaptchaBitmap);
  FCAPTCHAString:= GenerateCAPTCHAString;
  FCAPTCHABitmap:= TBitmap.Create;
  FCAPTCHABitmap.PixelFormat:= pf24bit;
  FCAPTCHABitmap.Width:= cWidth;
  FCAPTCHABitmap.Height:= cHeight;
  FCAPTCHABitmap.Canvas.Brush.Color:= clWhite;
  FCAPTCHABitmap.Canvas.FillRect(0,0,cWidth,cHeight);
  for i:= 1 to Length(FCAPTCHAString) do
    DrawLetter(FCAPTCHAString[i], Random(600) + 1, 25 * i - 15);
  DrawLines;
end;

function TCAPTCHA.Validate(const AValue: string;
  ACaseSensetive: Boolean): Boolean;
begin
  if ACaseSensetive then
    Result:= AValue = FCaptchaString
  else
    Result:= SameText(AValue, FCaptchaString);
end;

constructor TCAPTCHA.Create(ACharCase: TCharCases);
begin
  FCharCase := ACharCase;
  Randomize;
  RefreshBitmap;
end;

destructor TCAPTCHA.Destroy;
begin
  FreeAndNil(FCAPTCHABitmap);
  inherited Destroy;
end;

end.


(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  Laentir Valetov
  email:laex@bk.ru
  Mikhail Grigorev
  email:sleuthound@gmail.com
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  Warning: Using Delphi XE2 syntax!
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://ocv.org
  Online docs: http://docs.ocv.org
  Q&A forum:   http://answers.ocv.org
  Dev zone:    http://code.ocv.org
  **************************************************************************************************
*)

{$I OpenCV.inc}
{$IFDEF DELPHI2009_UP}
{$POINTERMATH ON}
{$ENDIF}
unit ocv.utils;

interface

uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  Vcl.Graphics,
{$ELSE}
{$IFDEF MSWINDOWS}
  LCLIntf, LCLType, LMessages,
{$ENDIF MSWINDOWS}
  Graphics,
{$ENDIF HAS_UNITSCOPE}
  types, ocv.core.types_c, GraphType, IntfGraphics, ExtCtrls;

Function hsv2rgb(hue: single): TCvScalar;

procedure IplImage2Bitmap(iplImg: PIplImage; var bitmap:
{$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap{$ELSE}Graphics.TBitmap{$ENDIF});
function cvImage2Bitmap(img: PIplImage): {$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap{$ELSE}Graphics.TBitmap{$ENDIF};

//procedure ipDraw(const x, y: Integer; const _Grab: PIplImage; const Wnd: THandle); overload;
//function CreateRGBBitmap(_Grab: PIplImage): HBITMAP;
procedure ipDraw(cv: TCanvas; img: PIplImage; const rect: TRect;
                 const Stretch: Boolean = true);

function c_str(const Text: String): pCVChar;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: pCvArr): pCvArr; overload;
function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: string): string; overload;
function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: TCvScalar): TCvScalar; overload;

//function BitmapToIplImage(const bitmap:
//{$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap{$ELSE}Graphics.TBitmap{$ENDIF}): PIplImage;
procedure Bitmap2IplImage(iplImg: PIplImage;  bitmap: TBitmap);

function CropIplImage(const src: PIplImage; const roi: TCvRect): PIplImage;

procedure ocvRGBToHSV(const R, G, B: byte; out _H, _S, _V: byte);
procedure ocvHSVToRGB(const _H, _S, _V: byte; out _R, _G, _B: byte);

{$IFDEF DELPHIXE3_UP}

Type
  TStringAnsiHelper = record helper for
    String
    function AsPAnsiChar: PAnsiChar;
  end;
{$ENDIF}

implementation

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  ocv.core_c, Math;

{$IFDEF DELPHIXE3_UP}
{ TStringAnsiHelper }

function TStringAnsiHelper.AsPAnsiChar: PAnsiChar;
begin
  Result := c_str(Self);
end;
{$ENDIF}

procedure ZeroMemory(mem: Pointer; size: Cardinal);
begin
  FillByte(mem, size, 0);
end;

{-----------------------------------------------------------------------------
  Procedure:  Bitmap2IplImage
  Author:     G. De Sanctis
  Date:       15-may-2007
  Arguments:  iplImg: PIplImage; bitmap: TBitmap
  Description: create a new IplImage and convert a Windows bitmap (24 bit) to it
-----------------------------------------------------------------------------}
procedure Bitmap2IplImage(iplImg: PIplImage;  bitmap: TBitmap);
  VAR
    dataByte :  PByteArray;
    RowIn    :  pByteArray;
    lazImg: TLazIntfImage;
BEGIN
  TRY
    assert((iplImg^.Depth = 8) and (iplImg^.NChannels = 3),
                'Not a 24 bit color iplImage!');

    iplimg^.Origin := IPL_ORIGIN_BL;
    iplImg^.ChannelSeq := 'BGR';

    lazImg := TLazIntfImage.Create(bitmap.Width, bitmap.Height);
    lazImg.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
    RowIn  := lazImg.GetDataLineStart(bitmap.height -1 );
    dataByte := pbytearray(iplimg^.ImageData);
    {direct copy of the bitmap row bytes to iplImage rows}
    Move(rowin^, dataByte^, iplimg^.ImageSize);
    lazImg.Free;
  Except
        on E: Exception do
        begin
             raise  Exception.Create('Bitmap2IplImage- error - ' + e.Message);
        end;
  END
END; {Bitmap2IplImage}

function CropIplImage(const src: PIplImage; const roi: TCvRect): PIplImage;
begin
  // Must have dimensions of output image
  Result := cvCreateImage(cvSize(roi.Width, roi.Height), src^.depth, src^.nChannels);
  // Say what the source region is
  cvSetImageROI(src, roi);
  // Do the copy
  cvCopy(src, Result);
  cvResetImageROI(src);
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: pCvArr): pCvArr; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: string): string; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

function ifthen(const Cond: Boolean; const ValueTrue, ValueFalse: TCvScalar): TCvScalar; overload;
begin
  if Cond then
    Result := ValueTrue
  else
    Result := ValueFalse;
end;

// ---------------------------------------------------------------------------
// Ñîçäàíèå API øíîãî áèòìàïà èç èíòåëîâñêîãî RGB èçîáðàæåíèÿ
// ---------------------------------------------------------------------------
//function CreateRGBBitmap(_Grab: PIplImage): HBITMAP;
//
//  function WIDTHBYTES(bits: DWORD): DWORD; {$IFDEF USE_INLINE}inline; {$ENDIF}
//  begin
//    Result := ((((bits) + 31) div 32) * 4);
//  end;
//
//Var
//  lpbi: TBitmapInfo;
//  App: PByte;
//  pBits: Pointer;
//  i, j : Integer;
//  src, dest: pointer;
//begin
//  lpbi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
//  lpbi.bmiHeader.biWidth := _Grab^.Width;
//  lpbi.bmiHeader.biHeight := _Grab^.Height;
//  lpbi.bmiHeader.biPlanes := 1;
//  lpbi.bmiHeader.biBitCount := 24;
//  lpbi.bmiHeader.biCompression := BI_RGB;
//  lpbi.bmiHeader.biSizeImage := WIDTHBYTES(_Grab^.Width * 8) * _Grab^.Height;
//  lpbi.bmiHeader.biXPelsPerMeter := 0;
//  lpbi.bmiHeader.biYPelsPerMeter := 0;
//  lpbi.bmiHeader.biClrUsed := 0;
//  lpbi.bmiHeader.biClrImportant := 0;
//  Result := CreateDIBSection(0, lpbi, DIB_RGB_COLORS, pBits, 0, 0);
//  if Result <> 0 then
//  begin
//    App := pBits;
//
//    if (_Grab^.nChannels = 1) then // Ñåðîå èëè áèíàðíîå
//    begin
//
//      for i := 0 to _Grab^.Height - 1 do
//      begin
//        for j := 0 to _Grab^.Width - 1 do
//        begin
//          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3] := PByte(_Grab^.ImageData)[_Grab^.Width * (i) + j];
//          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3 + 1] := PByte(_Grab^.ImageData)[_Grab^.Width * (i) + j];
//          App[_Grab^.Width * 3 * (_Grab^.Height - i - 1) + j * 3 + 2] := PByte(_Grab^.ImageData)[_Grab^.Width * (i) + j];
//        end;
//      end;
//
//    end;
//
//    if (_Grab^.nChannels = 3) then // Öâåòíîå
//    begin
//      for i := 0 to _Grab^.Height - 1 do
//      begin
//        dest := App + _Grab^.Width * 3 * (_Grab^.Height - i - 1);
//        src  := PByte(_Grab^.ImageData) + _Grab^.Width * 3 * i;
//        //CopyMemory(dest, src, _Grab^.Width * 3);
//        Move(src^, dest^, _Grab^.Width * 3);
//        // Êîïèðóåì ïàìÿòü
//      end;
//
//    end;
//  end;
//end;
//
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// Ôóíêöèÿ âûâîäà èçîáðàæåíèÿ íà HANDLE îêîííîãî êîìïîíåíòà
// ---------------------------------------------------------------------------
//procedure ipDraw(const x, y: Integer; const _Grab: PIplImage; const Wnd: THandle); overload;
//Var
//  hMemDC, dc: HDC;
//  bitmap: HBITMAP;
//begin
//  dc := GetDC(Wnd);
//  hMemDC := CreateCompatibleDC(dc);
//  bitmap := CreateRGBBitmap(_Grab);
//  SelectObject(hMemDC, bitmap);
//  BitBlt(dc, x, y, _Grab^.Width, _Grab^.Height, hMemDC, 0, 0, SRCCOPY);
//  DeleteObject(bitmap);
//  DeleteDC(hMemDC);
//  DeleteDC(dc);
//end;
//
function c_str(const Text: String): pCVChar;
begin
  Result := pCVChar(@(AnsiString(Text)[1]));
end;

Function hsv2rgb(hue: single): TCvScalar;
var
  rgb: array [0 .. 2] of Integer;
  p, sector: Integer;
const
  sector_data: array [0 .. 5, 0 .. 2] of Integer = ((0, 2, 1), (1, 2, 0), (1, 0, 2), (2, 0, 1), (2, 1, 0), (0, 1, 2));
Begin
  hue := hue * 0.033333333333333333333333333333333;
  sector := cvFloor(hue);
  // p := cvRound(255 * (hue - sector));
  if (sector and 1) = 1 then
    p := 255
  else
    p := 0;

  rgb[sector_data[sector][0]] := 255;
  rgb[sector_data[sector][1]] := 0;
  rgb[sector_data[sector][2]] := p;

  Result := cvScalar(rgb[2], rgb[1], rgb[0], 0);
End;

{-----------------------------------------------------------------------------
  Procedure:  IplImage2Bitmap
  Author:     K.Otani
  Date:       25-Oct-2017
  Arguments:  iplImg: PIplImage; bitmap: TBitmap
  Description: Convert IplImage to Bitmap for Raspberry pi
  refer to
  http://lazarus-ccr.sourceforge.net/docs/lcl/graphtype/trawimagedescription.html
-----------------------------------------------------------------------------}
procedure iplImage2Bitmap(iplImg: PIplImage; var bitmap:TBitmap);
var
    ImgPtr      : PByte;
    offset      : Longword;
    x,y         : Integer;
    PxlPtr      : PByte;
    LinePtr     : PByte;
    RawImage    : TRawImage;
    bmp2        : TBitmap;
    BytePerPixel: Integer;
begin
    bmp2:=TBitmap.Create;
    bmp2.Width:=iplImg^.Width;
    bmp2.Height:=iplImg^.Height;

    RawImage:= bmp2.RawImage;
    BytePerPixel := RawImage.Description.BitsPerPixel div 8;

    LinePtr := RawImage.Data;
    ImgPtr:=iplImg^.ImageData;

    if iplImg^.NChannels=3 then
     begin
       for y:=0 to iplImg^.Height-1 do
       begin
          PxlPtr := LinePtr;
          for x:=0 to iplImg^.Width-1 do
            begin
               offset  :=(iplImg^.WidthStep*y)+(x*3);
               PxlPtr^ :=ImgPtr[offset+0]; Inc(PxlPtr);
               PxlPtr^ :=ImgPtr[offset+1]; Inc(PxlPtr);
               PxlPtr^ :=ImgPtr[offset+2]; Inc(PxlPtr);
               if BytePerPixel = 4 then    Inc(PxlPtr);// BytePerPixel = 4bytes (RaspberryPi)
            end;
          // There might be gap at the end of line
          Inc(LinePtr, RawImage.Description.BytesPerLine);
       end;
     end;

     if iplImg^.NChannels=1 then
     begin
       for y:=0 to iplImg^.Height-1 do

         begin
            PxlPtr := LinePtr;
            for x:=0 to iplImg^.Width-1 do
               begin
                 offset  :=(iplImg^.WidthStep*y)+(x);
                 PxlPtr^ :=ImgPtr[offset]; Inc(PxlPtr);
                 PxlPtr^ :=ImgPtr[offset]; Inc(PxlPtr);
                 PxlPtr^ :=ImgPtr[offset]; Inc(PxlPtr);
                 if BytePerPixel = 4 then  Inc(PxlPtr);// BytePerPixel = 4bytes (RaspberryPi)
               end;
            Inc(LinePtr, RawImage.Description.BytesPerLine);
         end;
     end;
     bitmap.Assign(bmp2);
     bmp2.free;
end;

function cvImage2Bitmap(img: PIplImage): {$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap{$ELSE}Graphics.TBitmap{$ENDIF};
var
  // info: string;
  bmp: {$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap{$ELSE}Graphics.TBitmap{$ENDIF};
  deep: Integer;
  i, j, K, wStep, Channels: Integer;
  data: PByteArray;
  pb: PByteArray;
begin
  Result := NIL;
  if (img <> NIL) then
  begin
    bmp := {$IFDEF DELPHIXE2_UP}Vcl.Graphics.TBitmap.Create{$ELSE}Graphics.TBitmap.Create{$ENDIF};
    bmp.Width := img^.Width;
    bmp.Height := img^.Height;
    deep := img^.nChannels * img^.depth;
    case deep of
      8:
        bmp.PixelFormat := pf8bit;
      16:
        bmp.PixelFormat := pf16bit;
      24:
        bmp.PixelFormat := pf24bit;
      32:
        bmp.PixelFormat := pf32bit;
    End;
    wStep := img^.WidthStep;
    Channels := img^.nChannels;
    data := Pointer(img^.ImageData);
    for i := 0 to img^.Height - 1 do
    begin
      pb := bmp.Scanline[i];
      for j := 0 to img^.Width - 1 do
      begin
        for K := 0 to Channels - 1 do
          pb[3 * j + K] := data[i * wStep + j * Channels + K]
      End;
    End;
    Result := bmp;
    // bmp.Free;
  End;
end;

procedure ipDraw(cv: TCanvas; img: PIplImage; const rect: TRect;
                 const Stretch: Boolean = true);
{Lazarus replece for ipDraw() }
var
  bmp: TBitmap;
begin
  bmp:= TBitmap.Create;
  iplImage2Bitmap(img, bmp);
  if Stretch then begin
    cv.StretchDraw(rect, bmp);
  end else begin
    cv.Draw(0,0,bmp);
  end;
  bmp.Destroy;
end;

procedure ocvHSVToRGB(const _H, _S, _V: byte; out _R, _G, _B: byte);
{
  in
  H = Hue. Range is from 0..180. or H < 0 for gray
  S = Satration. Range is 0..255 where 0 is white and 255 is no saturation.
  V = Value. Range is 0..255

  out
  R = 0..255
  G = 0..255
  B = 0..255

  If H < 0 then the result is a gray value R=V, G=V, B=V
}
const
  SectionSize = 60 / 360;
var
  Section: single;
  SectionIndex: Integer;
  F: single;
  p, q, t: single;
  H, S: single;
begin
  H := _H / 180;
  S := _S / 255;

  if H < 0 then
  begin
    _R := _V;
    _G := _R;
    _B := _R;
  end
  else
  begin
    Section := H / SectionSize;
    SectionIndex := Floor(Section);
    F := Section - SectionIndex;
    p := _V * (1 - S);
    q := _V * (1 - S * F);
    t := _V * (1 - S * (1 - F));
    case SectionIndex of
      0:
        begin
          _R := _V;
          _G := Trunc(t);
          _B := Trunc(p);
        end;
      1:
        begin
          _R := Trunc(q);
          _G := _V;
          _B := Trunc(p);
        end;
      2:
        begin
          _R := Trunc(p);
          _G := _V;
          _B := Trunc(t);
        end;
      3:
        begin
          _R := Trunc(p);
          _G := Trunc(q);
          _B := _V;
        end;
      4:
        begin
          _R := Trunc(t);
          _G := Trunc(p);
          _B := _V;
        end;
    else
      _R := _V;
      _G := Trunc(p);
      _B := Trunc(q);
    end;
  end;
end;

procedure ocvRGBToHSV(const R, G, B: byte; out _H, _S, _V: byte);
{
  in
  R = 0..255
  G = 0..255
  B = 0..255

  out
  H = Hue. -1 for grey scale or range 0..180. 0..180 represents 0..360 degrees
  S = Saturation. Range = 0..255. 0 = white, 255 = no saturation.
  V = Value or intensity. Range 0..255
}
var
  rgb: array [0 .. 2] of single;
  MinIndex, MaxIndex: Integer;
  Range: single;
  H, S, V: single;
begin
  rgb[0] := R;
  rgb[1] := G;
  rgb[2] := B;

  MinIndex := 0;
  if G < R then
    MinIndex := 1;

  if B < rgb[MinIndex] then
    MinIndex := 2;

  MaxIndex := 0;
  if G > R then
    MaxIndex := 1;

  if B > rgb[MaxIndex] then
    MaxIndex := 2;

  Range := rgb[MaxIndex] - rgb[MinIndex];

  // Check for a gray level
  if Range = 0 then
  begin
    _H := 0; // Can't determine on greys, so set to -1
    _S := 0; // Gray is at the center;
    _V := R; // could choose R, G, or B because they are all the same value.
  end
  else
  begin
    case MaxIndex of
      0:
        H := (G - B) / Range;
      1:
        H := 2 + (B - R) / Range;
      2:
        H := 4 + (R - G) / Range;
    else
      H := 0;
    end;
    S := Range / rgb[MaxIndex];
    V := rgb[MaxIndex];
    H := H * (1 / 6);
    if H < 0 then
      H := 1 + H;

    _H := Trunc(H * 180);
    _S := Trunc(S * 255);
    _V := Trunc(V);
  end;
end;

end.

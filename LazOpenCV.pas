unit LazOpenCV;


interface

uses
  Graphics, FPImage, IntfGraphics, GraphType,
  Sysutils,Math
  , ocv.core.types_c
  , ocv.core_c
  ;

procedure iplImage2Bitmap(iplImg: PIplImage; var bitmap:TBitmap);
procedure Bitmap2IplImage(iplImg: PIplImage;  bitmap: TBitmap);

Implementation

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
{-----------------------------------------------------------------------------
  Procedure:  IplImage32FTo8Bit
  Author:     G. De Sanctis
  Date:       23-set-2005
  Arguments:  img32: PIplImage; img8: PIplImage
  Description: convert a 32 bit IplImage to a 8 bit IplImage
-----------------------------------------------------------------------------}
procedure IplImage32FTo8Bit(Img32, Img8: PIplImage);
  VAR
    scale, shift, diff: Double;
    minVal, maxVal: Double;
    a, b: TCvPoint;


  begin
  TRY
    assert((Img32^.Depth = 32) and (Img8^.Depth = 8)
                and ( ((Img8^.NChannels = 1) and (Img32^.NChannels = 1))  ),
                'Input must be 32F, 1 channel; output must be 8U, 1 channel!');



    //src =  IplImage * - depth = IPL_DEPTH_32F, nChannels =1
    //dest = IplImage * - depth = IPL_DEPTH_8U, nChannels =1 (gray)


    cvMinMaxLoc(img32,  @minVal, @maxVal, @a, @b);
    diff := maxVal - minVal;
    if diff <>0 then
        scale := 255/diff
    else
        scale := 0;
    shift := -minVal*scale;

    cvConvertScale (img32, img8, scale, shift);

  Except
        on E: Exception do
        begin
             raise  Exception.Create('IplImage32FTo8Bit- error - ' + e.Message);
        end;

  END; //try
  end;



end.



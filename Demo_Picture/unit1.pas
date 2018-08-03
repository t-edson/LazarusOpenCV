unit Unit1;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls
  , ocv.highgui_c
  , ocv.core.types_c
  , ocv.utils
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
  public

  end;

var
  Form1: TForm1;
  img: PIplImage;

implementation
{$R *.lfm}
{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  bmp: TBitmap;
begin
    img:=cvLoadImage( 'photo.jpeg');
    if img = nil then begin
      ShowMessage('Could not open picture!');
      halt;
    end;
    bmp := TBitmap.Create;
    IplImage2Bitmap(img, bmp);
    Form1.Image2.Picture.bitmap.assign(bmp);
    bmp.free;
end;

end.


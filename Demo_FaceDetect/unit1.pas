unit Unit1;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls
  , ocv.highgui_c
  , ocv.core_c
  , ocv.core.types_c
  , ocv.objdetect_c
  , ocv.utils
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image2: TImage;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    HaarList:TStringlist;
  public

  end;

var
  Form1: TForm1;
  img: PIplImage;
  // Create memory for calculations
  storage: pCvMemStorage = nil;
  // Create a new Haar classifier
  ClassifierCascade: pCvHaarClassifierCascade;
  // Create a string that contains the cascade name
  Cascade_Name: AnsiString = '../resource/facedetectxml/haarcascade_frontalface_alt.xml';


implementation

{$R *.lfm}

{ TForm1 }

// Function prototype for detecting and drawing an object from an image
procedure detect_and_draw( image: pIplImage);
var
 scale: Integer;
 temp: pIplImage;
 // two points to represent the face locations
 pt1, pt2: TCvPoint;
 i : Integer;
 faces: pCvSeq;
 r: pCvRect;
 mycvSize:TcvSize;
 bmp : TBitmap;
begin
   scale := 1;
   // Create a new image based on the input image
   mycvSize.width:=image^.width div scale;
   mycvSize.height:=image^.height div scale;
   temp := cvCreateImage(mycvSize , 8, 3);
   cvCopy(image, temp);

   // Clear the memory storage which was used before
   cvClearMemStorage(storage);

   if Assigned( ClassifierCascade) then begin
     // There can be more than one face in an image. So create a growable sequence of faces.
     // Detect the objects and store them in the sequence

     faces := cvHaarDetectObjects(image, ClassifierCascade, storage, 1.1, 2, CV_HAAR_SCALE_IMAGE or CV_HAAR_DO_CANNY_PRUNING, cvSize(40, 40), cvSize(0, 0));
     // Loop the number of faces found.
     for i := 1 to faces^.total do
     begin
       // Create a new rectangle for drawing the face
       r := pCvRect(cvGetSeqElem(faces, i));
       // Find the dimensions of the face,and scale it if necessary
       pt1.x := r^.x * scale;
       pt2.x := (r^.x + r^.width) * scale;
       pt1.y := r^.y * scale;
       pt2.y := (r^.y + r^.height) * scale;

       // Draw the rectangle in the input image
       cvRectangle(temp, pt1, pt2, CV_RGB(255, 0, 0), 3, 8, 0);
     end;
   end;

   bmp := TBitmap.Create;
   IplImage2Bitmap(temp, bmp);
   Form1.Image2.Picture.bitmap.assign(bmp);
   bmp.free;

   // Release the temp image created.
   cvReleaseImage(temp);
end;

procedure main_cycle(image: pIplImage);
var
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  IplImage2Bitmap(image, bmp);
  bmp.free;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  HaarList.free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  img:=cvLoadImage( PChar(extractfilepath(application.exename)+'photo.jpeg'));
  if img = nil then begin
    ShowMessage('Could not open picture!');
    halt;
  end;
  // Allocate the memory storage
  storage := cvCreateMemStorage(0);

  HaarList:=TStringlist.Create;

  // Load the First HaarClassifierCascade
  if fileexists(cascade_name) then
    ClassifierCascade := cvLoad(pCVChar(cascade_name));

  main_cycle( img);
  detect_and_draw( img);
end;

end.


unit Unit1;
//{$mode objfpc}{$H+}
{$mode delphi}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin
  , ocv.highgui_c
  , ocv.core_c
  , ocv.core.types_c
  , ocv.objdetect_c
  , LazOpenCV
  ;

const
   cResourceFaceDetect = '../resource/facedetectxml/';

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    FrameRate: TFloatSpinEdit;
    Image2: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FrameRateChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    HaarList:TStringlist;
    webcam:boolean;
  public

  end;

var
  Form1: TForm1;


  frame: PIplImage;
  capture: PCvCapture;
  // Create memory for calculations
  storage: pCvMemStorage = nil;
  // Create a new Haar classifier
  ClassifierCascade: array[0..1] of pCvHaarClassifierCascade ;
  SecondCascade: pCvHaarClassifierCascade = nil;
  // Create a string that contains the cascade name
  Cascade_Name: AnsiString = cResourceFaceDetect + 'haarcascade_frontalface_alt.xml';


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
 i,x: Integer;
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

 // Find whether the cascade is loaded, to find the faces. If yes, then:
 for x := 0 to high(ClassifierCascade) do begin
     if Assigned( ClassifierCascade[x]) then
     begin
       // There can be more than one face in an image. So create a growable sequence of faces.
       // Detect the objects and store them in the sequence

       faces := cvHaarDetectObjects(image, ClassifierCascade[x], storage, 1.1, 2, CV_HAAR_SCALE_IMAGE or CV_HAAR_DO_CANNY_PRUNING, cvSize(40, 40), cvSize(0, 0));
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
         cvRectangle(temp, pt1, pt2, CV_RGB((255*ord(x=0)), (255*ord(x=1)), (255*ord(x=2))), 3, 8, 0);
       end;
     end;
 end;

 bmp := TBitmap.Create;
 IplImage2Bitmap(temp, bmp);
 Form1.Image2.Picture.bitmap.assign(bmp);
 bmp.free;

 // Release the temp image created.
 cvReleaseImage(temp);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cvReleaseCapture( capture );
  HaarList.free;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  timer1.enabled:=false;
  if (Sender as TCombobox).ItemIndex > 0 then begin
    cascade_name:=HaarList[(Sender as TCombobox).ItemIndex-1];
    // Load the HaarClassifierCascade
    ClassifierCascade[(Sender as TCombobox).Tag] := cvLoad(pAnsiChar(cascade_name), nil, nil, nil);
  end else begin
    ClassifierCascade[(Sender as TCombobox).Tag] := nil;
  end;
  if webcam=true then begin
    timer1.enabled := true;
  end else begin
    detect_and_draw( frame);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   Timer1.enabled := false;
   OpenDialog1.InitialDir:=extractfilepath(application.ExeName);
   if OpenDialog1.Execute then begin
     webcam:=false;
     frame:=cvLoadImage( PChar(OpenDialog1.FileName));
     detect_and_draw( frame);
     Button1.Enabled:=false;
     Button2.Enabled:=true;
   end else begin
     webcam:=true;
     Timer1.enabled := true;
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Timer1.enabled := true;
  Button1.Enabled:=true;
  Button2.Enabled:=false;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  i:integer;
  cs: TCvSize;
begin
  try
  capture := cvCreateCameraCapture(0);
  except
  	on ex : exception  do
  	begin
      	  ShowMessage('Start capturing error - '+ex.message);
  	  halt;
        end;
  end;
  if not(assigned(capture ))  then
  begin
      webcam:=false;
      frame:=cvLoadImage( PChar(extractfilepath(application.exename)+'photo.jpeg'));
      Button1.Enabled:=false;
      Button2.Enabled:=true;
      if frame = nil then begin
        ShowMessage('Could not initialize capturing from camera nor loading picture faces.jpg!');
        halt;
      end;
  end else begin
      webcam:=true;
      cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH, 800);
      cvSetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT, 600);
      Button1.Enabled:=true;
      Button2.Enabled:=false;
  end;

  // Allocate the memory storage
  storage := cvCreateMemStorage(0);

  Combobox1.Items.Clear;
  Combobox2.Items.Clear;
  Combobox1.Items.add('Off');
  Combobox2.Items.add('Off');
  HaarList:=TStringlist.Create;
  FindAllFiles(HaarList, extractfilepath(application.ExeName)+cResourceFaceDetect, '*.xml', true);
  for i := 0 to HaarList.Count-1 do begin
    Combobox1.Items.add(AnsiString(extractfilename(HaarList[i])));
    Combobox2.Items.add(AnsiString(extractfilename(HaarList[i])));
  end;

  // Load the First HaarClassifierCascade
  Combobox1.Text:='haarcascade_frontalface_alt.xml';
  cascade_name:=AnsiString(HaarList[Combobox1.ItemIndex-1]);
  if fileexists(cascade_name) then
    ClassifierCascade[0] := cvLoad(pCVChar(cascade_name));

  // Load the Second HaarClassifierCascade
  Combobox2.Text:='haarcascade_eye.xml';
  cascade_name:=AnsiString(HaarList[Combobox2.ItemIndex-1]);
  if fileexists(cascade_name) then
    ClassifierCascade[1] := cvLoad(pCVChar(cascade_name));

  if webcam=true then begin
    timer1.enabled := true;
  end else begin
    detect_and_draw( frame);
  end;
end;

procedure TForm1.FrameRateChange(Sender: TObject);
begin
  Timer1.Interval :=  Round(1000/FrameRate.Value);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  frame := cvQueryFrame( capture );
  detect_and_draw( frame);
  Timer1.Enabled:=true;
end;

end.


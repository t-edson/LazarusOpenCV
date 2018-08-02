unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ocv.highgui_c, ocv.core.types_c ;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAR: TButton;
    ButtonShow: TButton;
    ButtonClose: TButton;
    ImageCaptured: TImage;
    ImageOut: TImage;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private {Private declarations}
    img1: pIplImage;
    bmp1: TBitmap;
    FOverlaySize: TCvSize;
    FCorner: array [0 .. 100] of TCvPoint2D32f;
    FOverlayImage: pIplImage;
    procedure DetectChess;
  public
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

uses
  ocv.core_c,
  ocv.utils,
  ocv.calib3d_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

const
  //Board size
  nWidthGrid = 5;
  nHeightGrid = 5;

{ TForm1 }

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialization
  FOverlaySize.height := nHeightGrid - 1;
  FOverlaySize.width := nWidthGrid - 1;
  FOverlayImage := cvLoadImage('../resource/media/baboon.jpg');
  if FOverlayImage = nil then begin
    Halt(1);
  end;

  bmp1 := TBitmap.Create;
  bmp1.PixelFormat := pf24bit;

  // Retrieve img1
//  img1 := cvQueryFrame(FCamCapture);
  img1:= cvLoadImage('chess.jpg');
  if img1 = nil then begin
    Halt(1);
  end;
  // Show input
  IplImage2Bitmap(img1, bmp1);
  ImageCaptured.Picture.Bitmap.Assign(bmp1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(bmp1) then
    bmp1.Free;
  cvReleaseImage(FOverlayImage);
end;

procedure TForm1.DetectChess;
var
  nResult: Integer;
  blankImg, negImg, copyImg, greyImg: pIplImage;
  p, q: pCvPoint2D32f;
  warp_matrix: pCvMat;
begin
  if img1=nil then exit;
  try
    // Create gray image from source
    greyImg := cvCreateImage(cvGetSize(img1), IPL_DEPTH_8U, 1);
    cvCvtColor(img1, greyImg, CV_BGR2GRAY);
    nResult := cvCheckChessboard(greyImg, CvSize(nWidthGrid, nHeightGrid));
    if nResult > 0 then begin
      nResult := cvFindChessboardCorners(greyImg, FOverlaySize, @FCorner);
      if nResult > 0 then begin
        try
          // Identifies the pattern from the gray image and saves the valid group of corners
          cvFindCornerSubPix(greyImg, @FCorner, 40, CvSize(11, 11), CvSize(-1, -1),
            CvTermCriteria(CV_TERMCRIT_EPS + CV_TERMCRIT_ITER, 40, 0.1));

          // Set the source points
          q := AllocMem(SizeOf(TCvPoint2D32f) * 4);
          // Src Top left
          q[0].x := 0;
          q[0].y := 0;
          // Src Top right
          q[1].x := FOverlayImage^.width - 1;
          q[1].y := 0;
          // Src Bottom left
          q[2].x := 0;
          q[2].y := FOverlayImage^.height - 1;
          // Src Bot right
          q[3].x := FOverlayImage^.width - 1;
          q[3].y := FOverlayImage^.height - 1;

          // Set the destination points
          p := AllocMem(SizeOf(TCvPoint2D32f) * 4);
          // Src Top right
          p[0].x := FCorner[0].x;
          p[0].y := FCorner[0].y;
          // Src Top left
          p[1].x := FCorner[nWidthGrid - 2].x;
          p[1].y := FCorner[nWidthGrid - 2].y;
          // Src Bottom left
          p[2].x := FCorner[(nWidthGrid - 1) * (nHeightGrid - 2)].x;
          p[2].y := FCorner[(nWidthGrid - 1) * (nHeightGrid - 2)].y;
          // Src Bot right
          p[3].x := FCorner[(nWidthGrid - 1) * (nHeightGrid - 1) - 1].x;
          p[3].y := FCorner[(nWidthGrid - 1) * (nHeightGrid - 1) - 1].y;

          // Create the transformation matrix
          warp_matrix := cvCreateMat(3, 3, CV_32FC1);
          cvGetPerspectiveTransform(q, p, warp_matrix);

          // Support structures
          blankImg := cvCreateImage(cvGetSize(FOverlayImage), FOverlayImage^.depth, FOverlayImage^.nChannels);
          negImg := cvCreateImage(cvGetSize(img1), img1^.depth, img1^.nChannels);
          copyImg := cvCreateImage(cvGetSize(img1), img1^.depth, img1^.nChannels);

          // Transform overlay image
          cvWarpPerspective(FOverlayImage, negImg, warp_matrix, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

          // Set to white
          cvSet(blankImg, cvScalarAll(255));
          // Transform blank image
          cvWarpPerspective(blankImg, copyImg, warp_matrix, CV_INTER_LINEAR or CV_WARP_FILL_OUTLIERS, cvScalarAll(0));

          // Invert image
          cvNot(copyImg, copyImg);

          // Join img1 and overlay image
          cvAnd(copyImg, img1, copyImg);
          cvOr(copyImg, negImg, img1);

          // Show output
          IplImage2Bitmap(img1, bmp1);
          ImageOut.Picture.Bitmap.Assign(bmp1);
          ImageOut.Canvas.Line(0,0,10,10);  //Update Picture
//            ImageOut.Invalidate;
        finally
          FreeMem(q);
          FreeMem(p);
          cvReleaseImage(blankImg);
          cvReleaseImage(negImg);
          cvReleaseImage(copyImg);
        end;
      end;
    end;
  except
    on E: Exception do
  end;
end;

procedure TForm1.ButtonShowClick(Sender: TObject);
begin
  DetectChess;
end;

end.


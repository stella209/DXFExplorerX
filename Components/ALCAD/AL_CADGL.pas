unit AL_CADGL;

interface

uses

  Windows,
  Classes, SysUtils, Graphics, Controls, StdCtrls, System.Generics.Collections,
  Messages, Forms, Dialogs, Math, ClipBrd,
  AL_Objects, AL_CADTypes, NewGeom, DGrafik, Szoveg, B_Spline, Clipper,
  AL_GL, alOpenGL;

Type

  // Event fron changing action/drawmode
  TChangeMode    = procedure(Sender: TObject; ActionMode: TActionMode; DrawMode: TDrawMode) of object;
  // Event fron changing window dimension
  TChangeWindow  = procedure(Sender: TObject; Origo: TPoint2D;  Zoom: Double; CursorPos: TPoint ) of object;
  TMouseEnter    = procedure(Sender: TObject) of object;
  TNewBeginPoint = procedure(Sender: TObject; Curve: integer) of object;
  TChangeCurve   = procedure(Sender: TObject; Curve: TCurve; Point: integer) of object;
  TCutPlan       = procedure(Sender: TObject; Curve: TCurve; Point: integer) of object;
  TProcess       = procedure(Sender: TObject; Status: byte; Percent: integer) of object;
  TAutoSortEvent = procedure(Sender: TObject; Status: byte; ObjectNo: word) of object;
  TNewFile       = procedure(Sender: TObject; FileName:string) of object;
  TBeforeDraw    = procedure(Sender: TObject; ca: TCanvas; cuv: TCurve) of object;

  TVisibles = class(TPersistent)
  private
  public

  end;

  { TCustomCADViewGL }

  TCustomCADViewGL = class(TAL_OpenGL)
  private
    fCADSource: TCADSource;
    FOnReCall: TNotifyEvent;
    FSelectedIndex: integer;
    fSelected: TCurve;
    fPointWidth: integer;
    fShowPoints: boolean;
    FAppend: boolean;
    FCadPens: TCadPens;
    FFilename: string;
    fLayerPaint: boolean;
    fFillBrush: TBrush;
    FFilled: boolean;
    FVisibleBeginPoint: boolean;
    FGraphTitle: Str32;
    fEnableRecall: boolean;
    fCoordHint: boolean;
    fEnablePaint: boolean;
    fHinted: boolean;
    FCompiled: boolean;
    FSensitiveRadius: integer;
    fChangeAll: TNotifyEvent;
    fChangeSelected: TChangeCurve;
    FLoading: boolean;
    procedure SetCADSource(const Value: TCADSource);
    procedure Change(Sender: TObject);
    function GetBlock(idx: integer): TBlock;
    function GetBlockCount: integer;
    function GetCount: integer;
    function GetCurve(idx: integer): TCurve;
    function GetLayer(idx: integer): TLayer;
    function GetLayerCount: integer;
    procedure SetBlock(idx: integer; const Value: TBlock);
    procedure SetCurve(idx: integer; const Value: TCurve);
    procedure SetLayer(idx: integer; const Value: TLayer);
    procedure SetSelectedIndex(const Value: integer);
    procedure SetSelected(const Value: TCurve);
    procedure SetPointWidth(const Value: integer);
    procedure SetShowPoints(const Value: boolean);
    procedure SetAppend(const Value: boolean);
    procedure SetCoordHint(const Value: boolean);
    procedure SetEnablePaint(const Value: boolean);
    procedure SetEnableRecall(const Value: boolean);
    procedure SetFilename(const Value: string);
    procedure SetFillBrush(const Value: TBrush);
    procedure SetFilled(const Value: boolean);
    procedure SetGraphTitle(const Value: Str32);
    procedure SetLayerPaint(const Value: boolean);
    procedure SetVisibleBeginPoint(const Value: boolean);
    procedure SetCompiled(const Value: boolean);
    procedure SetSensitiveRadius(const Value: integer);
    procedure SetLoading(const Value: boolean);
  protected
    Hint1               : THintWindow;
    HintActive          : boolean;
    oldHintStr          : string;
    oldCursor           : TCursor;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    newGraphic    : boolean;    {It must to generate new list}
    Hint_Label    : TLabel;
    CPMatch       : Boolean;    // Matching point
    CurveMatch    : Boolean;    // Matching curve
    CurveIn       : boolean;    // point in curve
    CPCurve       : Integer;
    LastCPCurve   : Integer;
    CPIndex       : Integer;    // Index of metching point
    LastCPIndex   : Integer;
    CPx           : TFloat;     // Aktív pont koordinátái
    CPy           : TFloat;
    BlockMatch    : boolean;    // Metching block
    CPBlock       : integer;    // Block index
    ActText       : Str32;
    PrevSelectedIndex : integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReCall;
    procedure On_Paint(Sender: TObject);
    procedure Paint_GL;
    procedure Paint_GDI;
    procedure DoPaint; // Forces the repaint with new generate of draw
    procedure DrawCurveGDI(ca: TCanvas; FC: TCurve; Magnify, Angle, dx, dy: double);
    procedure ZoomDrawing;
    procedure CurveToCent(AIndex: Integer);
    function IsRectInWindow(R: TRect2d): boolean;
    function IsPaperInWindow: boolean;
    function IsPointInWindow(p: TPoint2d): boolean;

    procedure Clear;
    function LoadGraphFromFile(const FileName: string): Boolean;
    function SaveGraphToFile(const FileName: string): Boolean;

    { Paint routines for OpenGL }
    procedure GenerateList;
    procedure DrawCurve(Cuv: Tcurve);
    procedure DrawBlock(cuv: TCurve);   // Draw an insert curve
    procedure DrawPoints(Cuv: Tcurve);
    procedure DrawBeginPoints(Cuv: Tcurve);
    procedure DrawSelectedCurves;

    procedure PoligonizeAll;
    procedure Poligonize(Cuv: TCurve; PointCount: integer);
    procedure VektorisationAll(MaxDiff: TFloat);

    function  GetCurveName(H: Integer): Str32;
    function  GetCurveHandle(AName: Str32; var H: Integer): Boolean;
    function  GetCurveIndex(AName: Str32): Integer;

    procedure CheckCurvePoints(X, Y: Integer);

    procedure SelectAll(all: boolean);

  {Public}
    property Loading   : boolean read FLoading write SetLoading;
    property Count     : integer read GetCount;
    property Curves[idx: integer]:TCurve read GetCurve write SetCurve;
    property Blocks[idx: integer]:TBlock read GetBlock write SetBlock;
    property Layers[idx: integer]:TLayer read GetLayer write SetLayer;
    property CurveCount: integer read GetCount;
    property BlockCount: integer read GetBlockCount;
    property LayerCount: integer read GetLayerCount;
    property Selected  : TCurve read fSelected write SetSelected;
    property SelectedIndex: integer read FSelectedIndex write SetSelectedIndex default -1;

    {Publeshed}
    property Append        : boolean     read FAppend write SetAppend;
    property CADSource     : TCADSource  read fCADSource write SetCADSource;
    property Compiled      : boolean     read FCompiled write SetCompiled default True;
    property LayerPaint    : boolean     read fLayerPaint write SetLayerPaint default True;
    property CadPens       : TCadPens    read FCadPens write FCadPens;
    property EnablePaint   : boolean     read fEnablePaint write SetEnablePaint default True;
    property EnableRecall  : boolean     read fEnableRecall write SetEnableRecall default True;
    property FileName      : string      read FFilename write SetFilename;
    property GraphTitle    : Str32       read FGraphTitle write SetGraphTitle;
    property Filled        : boolean     read FFilled write SetFilled;
    property FillBrush     : TBrush      read fFillBrush write SetFillBrush;
    property PointWidth    : integer     read fPointWidth write SetPointWidth;
    property ShowPoints    : boolean     read fShowPoints write SetShowPoints;
    property SensitiveRadius:integer     read FSensitiveRadius write SetSensitiveRadius;
    property Hinted        : boolean     read fHinted write fHinted;
    property CoordHint     : boolean     read fCoordHint write SetCoordHint;
    property VisibleBeginPoint: boolean  read FVisibleBeginPoint write SetVisibleBeginPoint;

    property OnReCall      : TNotifyEvent read FOnReCall write FOnReCall;
    property OnChangeAll   : TNotifyEvent read fChangeAll write fChangeAll;
    property OnChangeSelected : TChangeCurve read fChangeSelected write fChangeSelected;
  end;

  TalCADViewGL = class(TCustomCADViewGL)
  published
    property Append;
    property CADSource;
    property Compiled;
    property LayerPaint;
    property CadPens;
    property EnablePaint;
    property EnableRecall;
    property FileName;
    property GraphTitle;
    property Filled;
    property FillBrush;
    property PointWidth;
    property ShowPoints;
    property SensitiveRadius;
    property Hinted;
    property CoordHint;
    property VisibleBeginPoint;
    property OnReCall;
    property OnChangeAll;
    property OnChangeSelected;
  end;

  // Editable CAD component
  TCustomCADGL = class(TCustomCADViewGL)
  private
  protected
  public
  end;

  TalCADGL = class(TCustomCADGL)
  published
  end;

procedure Register;

implementation

{ TCustomCADViewGL }

procedure TCustomCADViewGL.Clear;
begin
  if FCADSource.FCurveList<>nil then
     FCADSource.Clear;
end;

constructor TCustomCADViewGL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks, csReplicatable];
  Width             := 200;
  height            := 200;
  FCompiled         := True;
  OnPaint           := On_Paint;
  CadSource         := nil;
  CadPens           := TCadPens.Create;
  fSensitiveRadius  := 4;
  PrevSelectedIndex := -1;
  Hint1             := THintWindow.Create(Self);
  Hint1.parent      := Self;
  Hint1.Canvas.Brush.Color:=clWhite;
  Hint1.Visible     := False;
  Hinted            := True;
  ZoomPaper;
end;

procedure TCustomCADViewGL.CurveToCent(AIndex: Integer);
var R : TRect2d;
begin
  R := TCurve(FCADSource.FCurveList.Items[Aindex]).BoundsRect;
  MoveCentrum((R.x1+R.x2)/2,(R.y1+R.y2)/2);
end;

destructor TCustomCADViewGL.Destroy;
begin
  CadPens.Free;
  Hint1.Free;
  inherited;
end;

procedure TCustomCADViewGL.DoPaint;
begin
  inherited;
  NewGraphic := True;
  On_Paint(Self);
  invalidate;
end;

procedure TCustomCADViewGL.On_Paint(Sender: TObject);
begin
//  if not Loading then
  if OpenglPaint then Paint_GL
  else Paint_GDI;
end;

procedure TCustomCADViewGL.DrawBeginPoints(Cuv: Tcurve);
Var x,y     : double;
begin
  if Cuv.Visible and (Cuv.Count > 0) then
  Try
             glPointSize(6);
             glColor(clRed);
             glBegin(GL_POINTS);
               Cuv.GetPoint(0,X,Y);
               glVertex2d(x,y);
             glEnd;
  except
    exit;
  end;
end;

procedure TCustomCADViewGL.DrawCurve(Cuv: Tcurve);
var i,H     : integer;
    x,y     : double;
    P1,P2   : TPoint2d;
    Sel     : boolean;
    FCurve  : TCurve;
    LayerColor : TColor;
begin
  CadSource.FCurveList.GetCurveHandle(Cuv.Name,H);
  if SelectedIndex>-1 then
     Sel := H=SelectedIndex
  else Sel := False;
  if LayerCount>0 then
     Cuv.Visible := Layers[Cuv.Layer].Visible;

  if Cuv.Visible and (Cuv.Count > 0) then
  Try
      if LayerPaint and (LayerCount>0) then begin
         LayerColor := Layers[Cuv.Layer].Pen.Color;
         glColor(LayerColor)
      end else begin
         glColor(CadPens.pBasePen.Color);
         glLineWidth(2);
      end;

      if Sel then begin
         glColor(clRed);
         glLineWidth(4);
      end;
      if Cuv.Selected then
      begin
         glLineWidth(4);
         glColor(clBlue);
      end;

      Case Cuv.Shape of
      dmPolygon,dmPolyLine,dmPoint,dmLine,dmRectangle,dmFreeHand,
      dmRotRectangle, dmSolid:
      begin
             If Cuv.Closed then
                if Filled or (Cuv.Shape=dmSolid) then
                   glBegin(GL_POLYGON)
                else
                   glBegin(GL_LINE_LOOP)
             else
                glBegin(GL_LINE_STRIP);
(*             if not Sel then begin
             if Cuv.Shape=dmPolyLine then
                glColor3f(0.5,0.5,0.5);
             if Cuv.Selected then glColor3f(0,0,1);
             end;*)
             for I:=0 to Pred(Cuv.Count) do
             begin
                Cuv.GetPoint(I,X,Y);
                glVertex2d(x,y);
             end;
             glEnd;
      end;
      dmCircle:
        begin
           P1 := Cuv.GetPoint2d(0);
           P2 := Cuv.GetPoint2d(1);
           glCircle(P1,P2);
        end;
      dmEllipse:
        begin
           P1 := Cuv.GetPoint2d(0);
           P2 := Cuv.GetPoint2d(1);
           glEllipse(P1,p2);
        end;
      dmArc:
        If Cuv.Count>2 then
        begin
           glBegin(GL_LINE_STRIP);
             Cuv.FillTempCurve;
             Cuv.TempCurve.Poligonize(0);
             for I:=0 to Pred(Cuv.TempCurve.Count) do
             begin
                Cuv.TempCurve.GetPoint(I,X,Y);
                glVertex2d(x,y);
             end;
           glEnd;
        end;
      dmSpline,dmBSpline :
        begin
          cuv.FillTempCurve;
          cuv.Poligonize(cuv.TempCurve,0);
             If cuv.TempCurve.Closed then
                glBegin(GL_LINE_LOOP)
             else
                glBegin(GL_LINE_STRIP);
             if not Sel then begin
             if cuv.TempCurve.Shape=dmPolyLine then
                glColor3f(0.5,0.5,0.5);
             if cuv.TempCurve.Selected then glColor3f(0,0,1);
             end;
             for I:=0 to Pred(cuv.TempCurve.Count) do
             begin
                cuv.TempCurve.GetPoint(I,X,Y);
                glVertex2d(x,y);
             end;
             glEnd;
        end;
      dmText :
            if (Cuv.FontHeight * Zoom)<-1 then
            begin
                glColor(clBlack);
                Draw2DText(Cuv.Text,-cuv.FontHeight,
                           cuv.FirstPoint.X,
                           cuv.FirstPoint.Y,
                           RadToDeg(cuv.BlockParams.Angle));
            end;
      dmInsert: DrawBlock(cuv);
      end;

  except
    exit;
  end;
end;

// Draw an insert curve (is a block)
procedure TCustomCADViewGL.DrawBlock(cuv: TCurve);
var
  P2,p    : TPoint2D;
  i,jj    : integer;
  block   : TBlock;
  FB,FC   : TCurve;
begin
if cuv.Shape = dmInsert then
begin
     if FCADSource.FBlockList.Count>0 then begin
        if block=nil then
           block:=TBlock.Create('');
        block := FCADSource.FBlockList.GetBlockByName(cuv.BlockParams.BlockName);
        if block<>nil then
        Try
           Try
             for JJ := 0 to Pred(block.FCurveList.Count) do
             begin
               FB := block.FCurveList.Curves[JJ];
               // Transform the block's curves
               p := Point2d(Cuv.BlockParams.BasePoint.x,Cuv.BlockParams.BasePoint.y);
               FB.MagnifyCurve(p,Cuv.BlockParams.Magnify);
               FB.RotateCurve(p,Cuv.BlockParams.Angle);
               FB.MoveCurve(Cuv.BlockParams.TranslateX,Cuv.BlockParams.TranslateY);
               DrawCurve(FB);
               // Reverse Transform the block's curves
               FB.MoveCurve(-Cuv.BlockParams.TranslateX,-Cuv.BlockParams.TranslateY);
               FB.RotateCurve(p,-Cuv.BlockParams.Angle);
               FB.MagnifyCurve(p,1/Cuv.BlockParams.Magnify);
             end;
           Finally
           End;
        except
              exit;
        end;
     end;
end;
end;

procedure TCustomCADViewGL.DrawCurveGDI(ca:TCanvas;FC:TCurve;Magnify,Angle,dx,dy:double);
var
  cuv     : TCurve;
  FL      : TPointList;
  H,I,J,K : integer;
  II,KK   : integer;
  PA      : Array of TPoint;
  p       : TPoint;
  X,Y     : TFloat;
  Radius  : integer;
  pp      : Array[0..2] of TPoint2D;
  P2      : TPoint2D;
  layer   : TLayer;
  co      : TColor;
begin
(*
  if Visible_Contours then
     II := 1
  else*)
     II := 0;

   if FC.Visible THEN
   for KK := 0 to II do
   begin
     case KK of
     0: begin
          FL := FC.FPoints;
          J := Pred(FL.Count);
        end;
     1: begin
          FL := FC.FContour;
          J := Pred(FL.Count);
        end;
     end;

     SetLength(PA,J+1);

     if FL.Count>0 then
     for I:=0 to J do begin
         p2 := FL.GetPoint2d(I);
         if Magnify<>1.0 then
         begin
           p2.x := Magnify * p2.x;
           p2.y := Magnify * p2.y;
         end;
         if Angle<>0.0 then
            Rotate2D(p2,Angle);
         if (dx<>0.0) or (dy<>0.0) then
         begin
           p2.X := p2.X+dx;
           p2.Y := p2.Y+dy;
         end;
         p := WtoS(p2);
         PA[I].x:= p.x;
         PA[I].y:= p.y;
     end;

     if FL.Count>0 then
     BEGIN
(*      // Set Pens and Brushes
      Ca.Pen.Assign(CADPens.pBasePen);
      if FFilled then begin
         ca.Brush.Assign(FillBrush)
      end else
         ca.Brush.Style:=bsClear;

      if H=SelectedIndex then
         ca.Pen.Assign(CADPens.pSigned);

      if Assigned(fBeforeDraw) then fBeforeDraw(Self,ca,FC);

      if KK=1 then begin
         ca.Pen.Assign(CADPens.pContour);
         ca.Brush.Style:=bsClear;
      end;

      if LayerPaint then begin
         layer := CADSource.Layers[FC.Layer];
         co:=Layer.pen.Color;
         if FC.Layer=7 then Layer.pen.Color:=clBlack;
         ca.Pen.Assign(layer.Pen);
      end;

*)
      if PA<>nil then
      Case FC.Shape of
      dmPolygon,dmPolyLine,dmPoint,dmLine,dmRectangle,dmFreeHand,dmRotRectangle:
        If FC.Closed then
            ca.Polygon(PA)
        else
            ca.PolyLine(PA);

      dmCircle:
        begin
           Radius:= Trunc( SQRT( SQR(p.x-PA[0].x) + SQR(p.y-PA[0].y) ) );
           ca.Ellipse(PA[0].x-Radius,PA[0].y-Radius,PA[0].x+Radius,PA[0].y+Radius);
        end;

      dmEllipse:
           ca.Ellipse(PA[0].x-Abs(PA[0].x-p.x),PA[0].y-Abs(PA[0].y-p.y),
                                  PA[0].x+Abs(PA[0].x-p.x),PA[0].y+Abs(PA[0].y-p.y));
      dmArc:
          If FC.FPoints.Count>2 then
          begin
            For i:=0 to 2 do begin
                FC.GetPoint(I,X,Y);
                pp[i] := Point2D(XtoS(x),YToS(y));
            end;
            KorivRajzol(ca,pp[0],pp[1],pp[2]);
          end else
            ca.PolyLine(PA);

      dmSpline:
          begin
              if FC.Closed then K:=3 else K:=4;
              SplineXP(ca,PA,36,TBSplineAlgoritm(K));
          end;

      dmBSpline:
          begin
              if FC.Closed then K:=1 else K:=2;
              SplineXP(ca,PA,36,TBSplineAlgoritm(K));
          end;

      dmText :
          begin
            if (FC.FontHeight * Zoom)<-1 then
            begin
                 ca.Font.Assign(FC.Font);
                 ca.Font.Height := Round(FC.FontHeight * Zoom);
                 ca.TextOut(PA[0].x,PA[0].y+ca.Font.Height,FC.Text);
            end;
          end;

      end; // End Case

(*
      // Sarokpontok rajzolása = Draw points
      If ShowPoints and (not Paning) then
      begin
           ca.Pen.Width := 1;
           ca.Pen.Style := psSolid;
           ca.Brush.Color:=clLime;
           If (FC.Selected) then begin
              ca.Pen.Assign(fCADPens.pSelected);
              ca.Pen.Width := 2;
           end else
              ca.Pen.Color := clBlack;
           for I:=0 to J do
           begin
                if i=1 then
                   ca.Brush.Color := clGray;
                if FC.GetPointRec(I).Selected then begin
                   ca.Brush.Color := clBlue;
                   Radius := 2*fPointWidth;
                end
                else begin
                   ca.Brush.Color:=clLime;
                   Radius := fPointWidth;
                end;
                if PA<>nil then
                ca.Rectangle(PA[I].x-Radius,PA[I].y-Radius,
                                         PA[I].x+Radius,PA[I].y+Radius);
           end;
      end;

      // Draw begin point
      if VisibleBeginPoint then
      begin
      If (FC=Selected) then
           ca.Brush.Color:=clRed
      else
           ca.Brush.Color:=clBlue;
      if FC.OutBase then
           ca.Brush.Color := clLime;
      ca.Ellipse(PA[0].x-fPointWidth-1,PA[0].y-fPointWidth-1,
                PA[0].x+fPointWidth+1,PA[0].y+fPointWidth+1);
      end;
      *)
     END;

//      if Assigned(fAfterDraw) then fBeforeDraw(Self,DrawBmp.Canvas,nil);

  end;
  SetLength(PA,0);
end;

procedure TCustomCADViewGL.DrawPoints(Cuv: Tcurve);
var i       : integer;
    x,y     : double;
begin
  if Cuv.Visible and (Cuv.Count > 0) then
  Try
             glEnable(GL_POINT_SMOOTH);
//             glPointSize(6);
             glBegin(GL_POINTS);
             for I:=Pred(Cuv.Count) downto 0 do
             begin
                if I=0 then begin
                   glColor3f(1,0,0);
                end else begin
                   if Cuv.Shape=dmPolyLine then
                      glColor3f(0.5,0.5,0.5)
                   else
                      glColor(clBlack);
                end;
                if Cuv.Selected then glColor3f(0,0,1);
                Cuv.GetPoint(I,X,Y);
                glVertex2d(x,y);
             end;
             glEnd;
  except
    exit;
  end;
end;

procedure TCustomCADViewGL.DrawSelectedCurves;
var
  H : Integer;
begin
  for H:=0 to Pred(Count) do
  if Curves[H].Selected then
      DrawCurve(Curves[H]);
  if SelectedIndex>-1 then
  if Curves[SelectedIndex]<>nil then
      DrawCurve(Curves[SelectedIndex]);
end;

procedure TCustomCADViewGL.GenerateList;
var
  H : Integer;
begin
  // Curves
  glLineWidth(2);
  if FCompiled then
     glNewList(1000,GL_COMPILE);
  for H:=0 to Pred(Count) do
    DrawCurve(Curves[H]);
  if FCompiled then
     glEndList();
  // Points
  glPointSize(8);
  if FCompiled then
     glNewList(2000,GL_COMPILE);
  for H:=0 to Pred(Count) do
      DrawPoints(Curves[H]);
  if FCompiled then
     glEndList();
  // First points
  glPointSize(8);
  if FCompiled then
     glNewList(3000,GL_COMPILE);
  for H:=0 to Pred(Count) do
      DrawBeginPoints(Curves[H]);
  if FCompiled then
     glEndList();

  NewGraphic := False;
end;

function TCustomCADViewGL.GetBlock(idx: integer): TBlock;
begin
  Result := CadSource.Blocks[idx];
end;

function TCustomCADViewGL.GetBlockCount: integer;
begin
  Result := CadSource.FBlockList.Count;
end;

function TCustomCADViewGL.GetCount: integer;
begin
  if FCADSource.FCurveList<>nil then
     Result := FCADSource.FCurveList.Count
  else
     Result := 0;
end;

function TCustomCADViewGL.GetCurve(idx: integer): TCurve;
begin
  if ((Idx>-1) and (idx < FCADSource.FCurveList.Count)) then
     Result := FCADSource.FCurveList.Items[idx]
  else
     Result := nil;
end;

function TCustomCADViewGL.GetCurveHandle(AName: Str32; var H: Integer): Boolean;
begin
  Result := FCADSource.FCurveList.GetCurveHandle(AName,H);
end;

function TCustomCADViewGL.GetCurveIndex(AName: Str32): Integer;
begin

end;

function TCustomCADViewGL.GetCurveName(H: Integer): Str32;
begin

end;

function TCustomCADViewGL.GetLayer(idx: integer): TLayer;
begin
  Result := CadSource.Layers[idx];
end;

function TCustomCADViewGL.GetLayerCount: integer;
begin
  Result := CadSource.FLayerList.Count;
end;

function TCustomCADViewGL.IsPaperInWindow: boolean;
var
    RP: TRect2d;     // Paper rectangle
begin
  if Paper.Visible then begin
     RP:= Rect2d(0,0,Paper.Width,Paper.Height);
     Result := IsRectInWindow(RP);
  end;
end;

function TCustomCADViewGL.IsPointInWindow(p: TPoint2d): boolean;
begin
     Result := PontInKep(XToS(p.x),YToS(p.y),Window);
end;

function TCustomCADViewGL.IsRectInWindow(R: TRect2d): boolean;
var
    RW,RP: TRect;     // Window and R rectangle on screen
    RR: HRgn;
begin
  Result := False;
  Try
     RW:= Rect(-10,-10,Width+10,Height+10);
     RP:= Rect(XToS(R.x1),YToS(R.y1),XToS(R.x2),YToS(R.y2));
     RR:= CreateRectRgn(RW.Left,RW.Top,RW.Right,RW.Bottom);
     Result := RectInRegion(RR,RP);
  finally
     DeleteObject(RR);
  end;
end;

function TCustomCADViewGL.LoadGraphFromFile(const FileName: string): Boolean;
var crold: TCursor;
begin
  if FCADSource.FCurveList<>nil then
     Result := fCADSource.LoadFromFile(FileName);
  if Result then begin
     FSelectedIndex := -1;
     NewGraphic := True;
     ZoomDrawing;
  end;
end;

procedure TCustomCADViewGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var pr : TPoint2d;
begin
  inherited;
  // Choice selected curve
  if (Shift = [ssLeft]) then begin
       if (CurveMatch or CPMatch or CurveIn) then
       begin
          Selected := Curves[CPCurve];
          SelectedIndex := CPCurve;
          if Assigned(fChangeSelected) then fChangeSelected(Self,Curves[CPCurve],CPIndex);
       end;
  end;
end;

procedure TCustomCADViewGL.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  FCurve : TCurve;
  Hintstr: string;
  HintRect: TRect;
  p: TPoint;
  w,he: integer;
begin
  inherited MouseMove(Shift, X, Y);

  // Searching point, line or inside curve area
  CheckCurvePoints(X,Y);

  {Hint window}
  If Hinted then begin
  If (CurveMatch or CPMatch or CurveIn) and (Shift = []) then begin
     Hint1.Font.Size :=8;
     Hint1.Font.Color:=clRed;
     FCurve := FCADSource.FCurveList.Items[CPCurve];
     Hintstr := '';
     If CurveMatch then
        Hintstr := '- '+IntToStr(CPCurve)+' - '+fCurve.Name;
     If CPMatch then
        Hintstr := ' ['+IntToStr(CPCurve)+'] '+fCurve.Name+'  ( '+IntToStr(CPIndex+1)+' / '+IntToStr(FCurve.Count)+' ) ';
     if CurveIn then begin
        Hintstr := '<'+IntToStr(CPCurve)+'>';
     end;
     p := ClientToScreen(point(x+8,y-18));
     w := Hint1.Canvas.TextWidth(Hintstr)+12;
     he := Hint1.Canvas.TextHeight(Hintstr)+8;
     HintRect := Rect(p.x,p.y,p.x+w,p.y+he);
        Hint1.ActivateHint(HintRect,Hintstr);
        oldHintstr := Hintstr;
        HintActive:=True;
  end else
    If HintActive then begin
       Hint1.ReleaseHandle;
       HintActive := False;
    end;
  end;

end;

procedure TCustomCADViewGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TCustomCADViewGL.Paint_GDI;
var
  tps     : tagPAINTSTRUCT;
  R,RP    : TRect;
  RE      : TRect2d;
  FC,FB   : TCurve;
  II,JJ,H : integer;
  block   : TBlock;
begin

//if EnablePaint then
if FCADSource<>nil then
  {Draw objects}
//if FVisibleObjects then
begin

//  if Assigned(fBeforePaint) then fBeforePaint(Self);

  if FCADSource.FCurveList<>nil then
  for H:=0 to Pred(Count) do
  begin
   FC := Curves[H];

   Case FC.Shape of
   dmInsert : // Block drawing
     if FCADSource.FBlockList.Count>0 then begin
        if block=nil then
           block:=TBlock.Create('');
        block := FCADSource.FBlockList.GetBlockByName(FC.BlockParams.BlockName);
        if block<>nil then
             for JJ := 0 to Pred(block.FCurveList.Count) do
             begin
               FB := block.FCurveList.Curves[JJ];
               FB.Selected := FC.Selected;
               DrawCurveGDI(DrawBmp.Canvas,FB,FC.BlockParams.Magnify,
                   FC.BlockParams.Angle,FC.Points[0].X,FC.Points[0].Y);

        end;
     end;
   else
     DrawCurveGDI(DrawBmp.Canvas,FC,1,0,0,0);
   End;

//      if Assigned(fAfterDraw) then fBeforeDraw(Self,DrawBmp.Canvas,nil);

  end;
end;
//  if Assigned(fAfterPaint) then fAfterPaint(Self);
//  inherited Paint;
end;

procedure TCustomCADViewGL.Paint_GL;
var
    ps : TPaintStruct;
    sz : TSzin;
    H  : integer;
begin
  if OpenGLPaint then
  begin

    if NewGraphic and FCompiled then
       GenerateList;
    if FCompiled then
    begin
      glCallList(1000);
      if fShowPoints then
         glCallList(2000);
      if VisibleBeginPoint then
         glCallList(3000);
      DrawSelectedCurves;
      NewGraphic := False;
    end else
    begin
      // Real time drawing in OpenGL
      // Curves
      glLineWidth(2);
      for H:=0 to Pred(Count) do
        DrawCurve(Curves[H]);
      // Points
      if ShowPoints then
      begin
           glPointSize(8);
           for H:=0 to Pred(Count) do
               DrawPoints(Curves[H]);
      end;
      if VisibleBeginPoint then
      begin
          // First points
          glPointSize(8);
          for H:=0 to Pred(Count) do
              DrawBeginPoints(Curves[H]);
      end;
      NewGraphic := False;
    end;
  end else
        inherited Paint;
end;

procedure TCustomCADViewGL.PoligonizeAll;
begin
  CADSource.FCurveList.PoligonizeAll(0);
  invalidate;
end;

procedure TCustomCADViewGL.Change(Sender: TObject);
begin
// if fEnableRecall then begin
  if CadSource<>nil then begin
    if Assigned(FOnReCall) then FOnReCall(Self);
  end;
  Repaint;
// end;
end;

function TCustomCADViewGL.SaveGraphToFile(const FileName: string): Boolean;
begin
  if FCADSource.FCurveList<>nil then
     Result := fCADSource.SaveToFile(FileName);
end;

// Select/unselect all curves
procedure TCustomCADViewGL.SelectAll(all: boolean);
var i,j: integer;
    cuv: TCurve;
    pr: TPointRec;
begin
  FCADSource.FCurveList.SelectAll(all);
  if Assigned(FChangeAll) then
     FChangeAll(Self);
  Recall;
end;

procedure TCustomCADViewGL.SetAppend(const Value: boolean);
begin
  FAppend:=Value;
  if FCADSource<>nil then
     FCADSource.Append:=Value;
end;

procedure TCustomCADViewGL.SetBlock(idx: integer; const Value: TBlock);
begin

end;

procedure TCustomCADViewGL.SetCADSource(const Value: TCADSource);
begin
  fCADSource := Value;
  if fCADSource = nil then
     fCADSource := TCADSource.Create(Application);
  if fCADSource <> nil then
     fCADSource.OnChange:=Change;
end;

procedure TCustomCADViewGL.SetCompiled(const Value: boolean);
begin
  FCompiled := Value;
  invalidate;
end;

procedure TCustomCADViewGL.SetCoordHint(const Value: boolean);
begin
  fCoordHint := Value;
  invalidate;
end;

procedure TCustomCADViewGL.SetCurve(idx: integer; const Value: TCurve);
begin
  if ((Idx>-1) and (idx < FCADSource.FCurveList.Count)) then
  begin
     FCADSource.FCurveList.Items[idx] := Value;
     Curves[idx] := Value;
  end
  else
     FCADSource.FCurveList.AddCurve(Value);
  invalidate;
end;

procedure TCustomCADViewGL.SetEnablePaint(const Value: boolean);
begin
  fEnablePaint := Value;
end;

procedure TCustomCADViewGL.SetEnableRecall(const Value: boolean);
begin
  fEnableRecall := Value;
  invalidate;
end;

procedure TCustomCADViewGL.SetFilename(const Value: string);
begin
  FFilename:=Value;
  LoadGraphFromFile(FFilename);
end;

procedure TCustomCADViewGL.SetFillBrush(const Value: TBrush);
begin
  fFillBrush := Value;
end;

procedure TCustomCADViewGL.SetFilled(const Value: boolean);
begin
  FFilled := Value;
  doPaint;
end;

procedure TCustomCADViewGL.SetGraphTitle(const Value: Str32);
begin
  FGraphTitle := Value;
end;

procedure TCustomCADViewGL.SetLayer(idx: integer; const Value: TLayer);
begin

end;

procedure TCustomCADViewGL.SetLayerPaint(const Value: boolean);
begin
  fLayerPaint := Value;
  DoPaint;
end;

procedure TCustomCADViewGL.SetLoading(const Value: boolean);
begin
  FLoading := Value;
end;

procedure TCustomCADViewGL.SetPointWidth(const Value: integer);
begin
  fPointWidth := Value;
  invalidate;
end;

procedure TCustomCADViewGL.SetSelected(const Value: TCurve);
begin
if Enabled {and (FSelected <> Value)} then begin
   FSelected := Value;
     fSelected := Value;
     if Value<>nil then
        SelectedIndex := CadSource.FCurveList.GetCurveHandle(Value.Name)
     else
        SelectedIndex := -1;
//     if Assigned(fChangeCurve) then fChangeCurve(Self,fSelected,CPIndex);
     if Assigned(fChangeSelected) then fChangeSelected(Self,FSelected,CPIndex);
     RePaint;
end;
end;

procedure TCustomCADViewGL.SetSelectedIndex(const Value: integer);
begin
  if Selected=nil then begin
     FSelectedIndex:=-1;
     exit;
  end;
  FSelectedIndex := Value;
  if FSelectedIndex>Count-1 then FSelectedIndex:=0;
end;

procedure TCustomCADViewGL.SetSensitiveRadius(const Value: integer);
begin
  FSensitiveRadius := Value;
end;

procedure TCustomCADViewGL.SetShowPoints(const Value: boolean);
begin
  fShowPoints := Value;
  invalidate;
end;

procedure TCustomCADViewGL.SetVisibleBeginPoint(const Value: boolean);
begin
  FVisibleBeginPoint := Value;
  invalidate;
end;

procedure TCustomCADViewGL.VektorisationAll(MaxDiff: TFloat);
begin
  CadSource.FCurveList.VektorisationAll(MaxDiff);
  invalidate;
end;

procedure TCustomCADViewGL.ZoomDrawing;
var nagyx,nagyy : extended;
    I,J: integer;
    BR: TRect2d;
    x1,x2,y1,y2: TFloat;
    cz : boolean;
begin
if Count>0 then begin
 cz := CentralisZoom;
 CentralisZoom := True;
 J:=Count;
 if J>0 then begin
    x1:=1e+10; y1:=1e+10;
    x2:=-1e+10; y2:=-1e+10;
    BR := CADSource.GetDrawExtension;
  Try
  if ((BR.x2-BR.x1)>0) then
     nagyx := Width /(BR.x2-BR.x1)
  else
     nagyx := 1;
  if ((BR.y2-BR.y1)>0) then
     nagyy := Height/(BR.y2-BR.y1)
  else
     nagyy := 1;
  except
     nagyx:=1; nagyy:=1;
  end;
  If nagyx > nagyy Then nagyx:= nagyy;
  Centrum := Point2d((BR.x2+BR.x1)/2,(BR.y2+BR.y1)/2);
  Zoom:= 0.9*nagyx;
 end;
end else ZoomPaper;
  CentralisZoom := cz;
end;

procedure TCustomCADViewGL.Poligonize(Cuv: TCurve; PointCount: integer);
begin
  FCADSource.FCurvelist.Poligonize(Cuv,PointCount);
  Recall;
end;

procedure TCustomCADViewGL.ReCall;
begin
  if CADSource<>nil then
  if fEnableRecall then
     CADSource.Recall;
  Repaint;
end;

procedure TCustomCADViewGL.CheckCurvePoints(X, Y: Integer);
var i,J,K,L,H: integer;
    Lx,Ly  : TFloat;
    xx,yy  : TFloat;
    InCode : TInCode;
    R      : TRect2d;
    ter,oter: double;
    FC     : TCurve;
    delta  : TFloat;
begin
    CPMatch    :=False;
    CurveMatch :=False;
    CurveIn    :=False;
    CPCurve    :=-1;         // nincs curve
    BlockMatch :=False;
    CPBlock    :=-1;
    oter := MaxDouble;

    xx := MapPoint.x;
    yy := MapPoint.y;

    delta := fSensitiveRadius/Zoom;

    J:=Pred(FCADSource.FCurveList.Count);

    for I:=J downto 0 do
    begin

      if Curves[I]=NIL then
         EXIT;

      R := Curves[I].BoundsRect;

      if Curves[I].IsInBoundsRect(xx,yy,delta) then begin

         FC:=Curves[I];

         if Curves[I].Shape in [dmCircle,dmEllipse,dmArc] then
         begin
//         if drawmode=dmNone then begin
            Curves[I].FillTempCurve;
            FC:=Curves[I].TempCurve;
            Poligonize( FC,10);
//         end else exit;
         end;

         if ShowPoints then
         begin
         if Curves[I].Shape in [dmCircle,dmEllipse,dmArc] then
            L := Curves[I].IsOnPoint(xx,yy,delta)
         else
            L := FC.IsOnPoint(xx,yy,delta);
         if L>-1 then
         begin
            CPMatch:=True;
            CurveMatch := True;
            if CPCurve <> I then
               LastCPCurve := CPCurve;
            CPCurve:=I;
            if CPIndex <> L then
               LastCPIndex := CPIndex;
            CPIndex:=L;
            CPx:=Curves[CPCurve].Points[L].x;
            CPy:=Curves[CPCurve].Points[L].y;
            Exit;
         end;
         end;

         if CPIndex <> -1 then
            LastCPIndex := CPIndex;
         CPIndex:=-1;         // nincs pont

         InCode := FC.IsInCurve(xx,yy, delta);
         case inCode of
         icOnLine :
         if not CPMatch then begin
              CurveMatch:=True;
              LastCPCurve := CPCurve;
              CPCurve:=I;
              Exit;
         end;
         icIn :
         begin
              R := FC.BoundsRect;
              ter := (R.x2-R.X1)*(R.y2-R.y1);
              if ter<oter then begin
                 CurveIn:=True;
                 LastCPCurve := CPCurve;
                 CPCurve:=I;
                 oter:=ter;
              end;
         end;
         end;

         if Curves[I].Shape=dmInsert then
         begin
              BlockMatch:=True;
              CurveMatch :=False;
              CurveIn    :=False;
              CPCurve    := -1;
              CPBlock:=I;
         end;

      end;
    end;

end;

procedure Register;
begin
  RegisterComponents('ALCad',[TCustomCADViewGL,TalCADViewGL,TalCADGL]);
end;

end.

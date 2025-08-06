
unit AL_CAD;

(* CAD components
 *
 * Hierarchy : TPointList - TCurve - TCurveList - (TalCADView ->TalCAD)
 *
 * by Agócs László Hun. 2021
 * lagocsstella@gmail.com
 * 2021-11-04
*)


//{$mode delphi}{$H+}

interface

uses

  Windows,
  Classes, SysUtils, Graphics, Controls, StdCtrls, System.Generics.Collections,
  Messages, Forms, Dialogs, Math, ClipBrd,
  AL_Objects, AL_CADTypes, NewGeom, DGrafik, Szoveg, B_Spline, Clipper;

Type

  TCadEvent = (ceNone, ceNew, ceAdd, ceChange, ceInsert, ceDelete);

  TInOutRec = record           // Metszési pont rekord  .margin
       mPont   : TPoint2d;     // metszéspont koordinátái
       idx     : integer;      // idx indexű pont után beszúrni
       d       : double;       // d távolság a kezdőponttól
  end;

  {Gyártási pozíció}
  TWorkPosition = record
    CuvNumber   : integer;      {Aktuális obj. sorszáma}
    PointNumber : integer;      {Aktuális pont sorszáma}
    WorkPoint   : TPoint2d;    {Aktuális pont koordinátái}
  end;


{ A rectangle sorround some selected oblects for further distorsion. }
(*
TSelectedAreaType = (
                     satFix,     // Mérete nem változtatható, csak forgatható
                     satMagnify, // Sarokpontokkal nagytható, oldalpontokkal
                                 //   ||-an elmozgatható
                     satFlex     // Rugalmasan torzítható
                    );
*)
{ TSelectedArea }

TSelectedArea = class(TPersistent)
private
  FPen: TPen;
//  FAllwaysDraw: boolean;
  FVisible: boolean;
  FOnChange: TNotifyEvent;
  FOrigRect: TRect2d;
  FRotAngle: double;
//  FFrameType: TSelectedAreaType;
  FOnVisible: TNotifyEvent;
  FZoom: double;
  FOrtho: boolean;
  procedure SetVisible(const Value: boolean);
//  procedure SetAllwaysDraw(const Value: boolean);
  procedure SetOrigRect(const Value: TRect2d);
  function GetBoundsRect: TRect2d;
  procedure SetRotAngle(const Value: double);
  procedure SetBoundsRect(const Value: TRect2d);
  function GetHeight: double;
  function GetWidth: double;
  procedure SetHeight(const Value: double);
  procedure SetWidth(const Value: double);
  procedure SetZoom(const Value: double);
public
  Nodes     : array[0..9] of TPoint2d; // Corners(0..3),Midpoints(4..7),8:RC,9:RCent
  RC        : TPoint2d;   // Rotation pont
  RCent     : TPoint2d;   // Rotation centrum (átlók metszéspontja)
  FixPoint  : TPoint2d;   // Standard fix point to OrthoTransform
  ActualNode: integer;    // Actual Node for modify
  OrigList  : TList;      // Original curve list in the OrigRect;
  DestList  : TList;      // Curve list after distorsion
  FCurve    : TCurve;
  oCurve    : TCurve;
  dCurve    : TCurve;
  FWidth    : double;
  FHeight   : double;

  constructor Create;
  destructor Destroy; override;

  procedure Init;        // Initialise the variables and lists
  procedure SetRect(R: TRect2d);
  procedure SetSize( w,h: double );  // Modify with and height
  procedure Recalc;      // Recalculate the nodes position in quadrilateral area
  function  IsNode(p: TPoint2d; Radius: double; var idx: integer): boolean;
  function  IsInPoint(p: TPoint2d): boolean;
  procedure SetNode( idx: integer; p: TPoint2d );
  procedure AddCurve(Cuv: TCurve);
  procedure Move(dx,dy : double);
  procedure MoveEdge(idx: integer; dx, dy : double);
  procedure Magnify( coeff: double );
  procedure RelRotate(angle : double); overload;
  procedure RelRotate( P: TPoint2d ); overload;
  procedure Rotate(Cent: TPoint2d; Angle: double);
  procedure Mirror(idx: integer);  // idx=1-függőleges, 2-vizszintes, 3-középpontos
  procedure OrthoTransform( NodeIdx: integer; CurPos: TPoint2d );
  property BoundsRect    : TRect2d read GetBoundsRect write SetBoundsRect;
  property OrigRect      : TRect2d read FOrigRect write SetOrigRect;
published
  // original rectangle
//  property FrameType     : TSelectedAreaType read FFrameType write FFrameType;
  property Ortho         : boolean read FOrtho write FOrtho default false;
  property Pen           : TPen    read FPen write FPen;
  property RotAngle      : double  read FRotAngle write SetRotAngle; // fok
  property Zoom          : double  read FZoom write SetZoom; // fok
  property Visible       : boolean read FVisible write SetVisible default false;
//  property AllwaysDraw   : boolean read FAllwaysDraw write SetAllwaysDraw default True;
  property Height        : double  read GetHeight write SetHeight;
  property Width         : double  read GetWidth write SetWidth;
  property OnChange      : TNotifyEvent read FOnChange write FOnChange;
  property OnVisible     : TNotifyEvent read FOnVisible write FOnVisible;
end;

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


  { TCustomCADView }

  TCustomCADView = class(TCustomControl)
  private
    fAfterDraw: TBeforeDraw;
    fAfterPaint: TNotifyEvent;
    FAppend: boolean;
    fBackColor: TColor;
    FBackImage: TBMPObject;
    fBeforeDraw: TBeforeDraw;
    fBeforePaint: TNotifyEvent;
    FCadPens: TCadPens;
    fCADSource: TCADSource;
    fCentralCross: boolean;
    FCentralisZoom: boolean;
    fCentrum: TPoint2dObj;
    fChangeWindow: TChangeWindow;
    FContourRadius: double;
    fCoordHint: boolean;
    fCoordLabel: TLabel;
    fCursorCross: boolean;
    fEnablePaint: boolean;
    fEnableRecall: boolean;
    FFilename: string;
    fFillBrush: TBrush;
    FFilled: boolean;
    FGraphTitle: Str32;
    fGrid: TGrid;
    fHinted: boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnReCall: TNotifyEvent;
    FOnResize: TNotifyEvent;
    fOrigo: TPoint2d;
    fPaper: TPoint2dObj;
    fPaperColor: TColor;
    FPaperVisible: boolean;
    fPointWidth: integer;
    fSelected: TCurve;
    FSelectedIndex: integer;
    FSelectedVisible: boolean;
    fShowPoints: boolean;
    FVisibleBeginPoint: boolean;
    FVisibleContours: Boolean;
    FVisibleObjects: boolean;
    fZoom: extended;
    fLayerPaint: boolean;
    FStop: boolean;
    function GetCount: integer;
    procedure SetBackImage(AValue: TBMPObject);
    procedure SetCADSource(AValue: TCADSource);
    procedure SetEnableRecall(AValue: boolean);
    procedure SetVisibleBeginPoint(AValue: boolean);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure CMMouseEnter(var msg:TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    function GetCurve(idx: integer): TCurve;
    procedure SetAppend(AValue: boolean);
    procedure SetCoordHint(AValue: boolean);
    procedure SetCurve(idx: integer; AValue: TCurve);
    procedure SetFilename(AValue: string);
    procedure SetFillBrush(AValue: TBrush);
    procedure SetGraphTitle(AValue: Str32);
    procedure SetPointWidth(AValue: integer);
    procedure SetSelected(AValue: TCurve);
    procedure SetSelectedIndex(AValue: integer);
    procedure SetSelectedVisible(AValue: boolean);
    procedure SetShowPoints(AValue: boolean);
    procedure SetVisibleObjects(AValue: boolean);
    function GetWindow: TRect2d;
    procedure doResize(Sender: TObject);
    procedure SetBackColor(AValue: TColor);
    procedure SetCentralCross(AValue: boolean);
    procedure SetCursorCross(AValue: boolean);
    procedure SetEnablePaint(AValue: boolean);
    procedure SetFilled(AValue: boolean);
    procedure SetPaperColor(AValue: TColor);
    procedure SetPaperVisible(AValue: boolean);
    procedure SetWindow(AValue: TRect2d);
    procedure SetZoom(AValue: extended);
    function FTransparentSet: Boolean;
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
    procedure SetLayerPaint(const Value: boolean);
    function GetBlock(idx: integer): TBlock;
    function GetLayer(idx: integer): TLayer;
    procedure SetBlock(idx: integer; const Value: TBlock);
    procedure SetLayer(idx: integer; const Value: TLayer);
    function GetBlockCount: integer;
    function GetCurveCount: integer;
    function GetLayerCount: integer;
//    function GetLType(idx: integer): TLineType;
//    procedure SetLType(idx: integer; const Value: TLineType);
  protected
    oldCentrum          : TPoint2d;   //
    Origin,MovePt       : TPoint;
    oldOrigin,oldMovePt : TPoint;
    MouseInOut          : integer;    // Egér belép:1, bent van:0, kilép:-1
    Hint_Label          : TLabel;
    Hint1               : THintWindow;
    HintActive          : boolean;
    oldHintStr          : string;
    oldCursor           : TCursor;

    procedure Loaded; override;
    procedure Change(Sender: TObject);
    procedure ChangeCentrum(Sender: TObject);
    procedure ChangePaperExtension(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    DrawBmp       : TBitMap;
    Visible_Contours : boolean;
    painting      : boolean;
    MapXY         : TPoint2d;   // Point on Word coordinates
    MapPoint      : TPoint2d;
    Mouse_Pos     : TPoint;     // Mouse x,y position
    Paning        : boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    { Világ koordináták (W) képernyő koordináttákká (S) ill. vissza }
    function  XToW(x:integer):TFloat;
    function  YToW(y:integer):TFloat;
    function  XToS(x:TFloat):integer;
    function  YToS(y:TFloat):integer;
    function  WToS(p:TPoint2d):TPoint; overload;
    function  WToS(x,y:TFloat):TPoint; overload;
    function  SToW(x,y: integer):TPoint2d; overload;
    function  SToW(p: TPoint): TPoint2d; overload;
    function  OrigoToCent:TPoint2D;
    function  CentToOrigo(c:TPoint2D):TPoint2D;
    procedure NewOrigo(x,y:extended);
    procedure NewCentrum(x,y:extended); overload;
    procedure NewCentrum(p:TPoint2d); overload;
    {Teljes papír az ablakban}
    procedure ZoomPaper;
    procedure ZoomDrawing;
    procedure MoveWindow(dx,dy: integer);
    procedure MoveCentrum(fx,fy: double);
    procedure MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
    procedure CurveToCent(AIndex: Integer);
    function GetDrawExtension: TRect2d;
    function IsRectInWindow(R: TRect2d): boolean;
    function IsPaperInWindow: boolean;
    function IsPointInWindow(p: TPoint2d): boolean;

    procedure DrawCurve(ca: TCanvas; FC: TCurve; Magnify, Angle, dx, dy: double);
    procedure GridDraw;
    procedure DrawMouseCross(o:TPoint;PenMode:TPenMode);

    procedure Clear;
    function LoadGraphFromFile(const FileName: string): Boolean;
    function SaveGraphToFile(const FileName: string): Boolean;

//    procedure Recall;

    procedure GeneratePoinsArray(AIndex: integer; var pArr: TPointArray); overload;
    procedure GeneratePoinsArray(ACurve: TCurve; var pArr: TPointArray); overload;

    {Public}
    property Canvas;
    property Window        : TRect2d     read GetWindow write SetWindow;
    property Origo         : TPoint2d    read fOrigo write FOrigo;
    property Centrum       : TPoint2dObj read fCentrum write fCentrum;
    property Selected      : TCurve      read fSelected write SetSelected;
    property Count         : integer     read GetCount;
    property Curves[idx: integer]:TCurve read GetCurve write SetCurve;
    property Blocks[idx: integer]:TBlock read GetBlock write SetBlock;
    property Layers[idx: integer]:TLayer read GetLayer write SetLayer;
//    property LTypes[idx: integer]:TLineType read GetLType write SetLType;
    property CurveCount: integer read GetCurveCount;
    property BlockCount: integer read GetBlockCount;
    property LayerCount: integer read GetLayerCount;
//    property STOP      : boolean read FStop write FStop;

    {Publeshed}
    property Append        : boolean     read FAppend write SetAppend;
    property CoordLabel    : TLabel      read fCoordLabel write fCoordLabel;
    property CadPens       : TCadPens    read FCadPens write FCadPens;
    property CentralCross  : boolean     read fCentralCross write SetCentralCross;
    property CursorCross   : boolean     read fCursorCross write SetCursorCross default False;
    // Kontúr vonal távolsága az objektumtól vágásnál és kontúrozásnál
    property CADSource     : TCADSource  read fCADSource write SetCADSource;
    property BackColor     : TColor      read fBackColor write SetBackColor;
    property BackImage     : TBMPObject  read FBackImage write SetBackImage;
    property EnablePaint   : boolean     read fEnablePaint write SetEnablePaint default True;
    property EnableRecall  : boolean     read fEnableRecall write SetEnableRecall default True;
    property FileName      : string      read FFilename write SetFilename;
    property GraphTitle    : Str32       read FGraphTitle write SetGraphTitle;
    property Filled        : boolean     read FFilled write SetFilled;
    property FillBrush     : TBrush      read fFillBrush write SetFillBrush;
    property Grid          : TGrid       read fGrid Write fGrid;
    property Hinted        : boolean     read fHinted write fHinted;
    property CoordHint     : boolean     read fCoordHint write SetCoordHint;
    property LayerPaint    : boolean     read fLayerPaint write SetLayerPaint default True;
    property Paper         : TPoint2dObj read fPaper write fPaper;
    property PaperColor    : TColor      read fPaperColor write SetPaperColor;
    property PaperVisible  : boolean     read FPaperVisible write SetPaperVisible;
    property PointWidth    : integer     read fPointWidth write SetPointWidth;
    property ShowPoints    : boolean     read fShowPoints write SetShowPoints;
    property SelectedIndex : integer     read FSelectedIndex write SetSelectedIndex;
    // Cursor sensitive radius of circle around of curves' points
    property SelectedVisible: boolean    read FSelectedVisible write SetSelectedVisible;
    property Transparent: Boolean        read GetTransparent write SetTransparent stored FTransparentSet;
    property Zoom          : extended    read fZoom write SetZoom;
    property CentralisZoom : boolean     read FCentralisZoom write FCentralisZoom;
    property VisibleBeginPoint: boolean  read FVisibleBeginPoint write SetVisibleBeginPoint;
    property VisibleObjects: boolean     read FVisibleObjects write SetVisibleObjects;

    property OnReCall      : TNotifyEvent  read FOnReCall write FOnReCall;
    property OnResize      : TNotifyEvent  read FOnResize write FOnResize;
    property OnChangeWindow: TChangeWindow read fChangeWindow write fChangeWindow;
    property OnBeforePaint : TNotifyEvent  read fBeforePaint write fBeforePaint;
             { Before draw the next object/curve Ex. change pen .... }
    property OnBeforeDraw  : TBeforeDraw   read fBeforeDraw write fBeforeDraw;
    property OnAfterDraw   : TBeforeDraw   read fAfterDraw write fAfterDraw;
    property OnAfterPaint  : TNotifyEvent  read fAfterPaint write fAfterPaint;
    property OnMouseEnter  : TNotifyEvent  read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave  : TNotifyEvent  read FOnMouseLeave write FOnMouseLeave;
  end;

  { TCustomCAD : CAD editor component }

  TCustomCAD = class(TCustomCADView)
  private
    fActionMode: TActionMode;
    fActLayer: integer;
    fAutoPoligonize: boolean;
    fAutoUndo: boolean;
    FBackImage: TBMPObject;
    FBoxing: boolean;
    fChangeAll: TNotifyEvent;
    fChangeCurve: TChangeCurve;
    fChanged: boolean;
    fChangeMode: TChangeMode;
    fChangeSelected: TChangeCurve;
    FClone_Contour: double;
    fDefaultLayer: Byte;
    FDefiniedLength: boolean;
    FDesigneMode: boolean;
    FDrawMode: TDrawMode;
    FEnableSelectedFrame: boolean;
    FGravity: boolean;
    fHinted: boolean;
    FLoading: boolean;
    fLocked: boolean;
    FMouseEnter: TMouseEnter;
    FMouseLeave: TMouseEnter;
    fNewBeginPoint: TNewBeginPoint;
    fOnSelectedFrame: TNotifyEvent;
    FOrtho: boolean;
    fpClosed: TPen;
    fpCrossed: TPen;
    fpFazis: integer;
    fpOpened: TPen;
    fpSelected: TPen;
    fpSigned: TPen;
    fpSorted: TPen;
    FSablonSzinkron: boolean;
    FSelectedFrame: TSelectedArea;
    FSensitiveRadius: integer;
    fShowNumbers: boolean;
    FSTOP: boolean;
    FTitleFont: TFont;
    FUndoRedoChangeEvent: TUndoRedoChangeEvent;
    FVecCoeff: double;
    FWorkArea: TRect;
    FXYCompulsion: boolean;
    fWorking: boolean;
    FEnableCutterMethods: boolean;
    function  GetDisabledCount: integer;
    procedure SetActionMode(AValue: TActionMode);
    procedure SetBackImage(AValue: TBMPObject);
    procedure SetBoxing(AValue: boolean);
    procedure SetContourRadius(AValue: double);
    procedure SetDefaultLayer(AValue: Byte);
    procedure SetDesigneMode(AValue: boolean);
    procedure SetDrawMode(AValue: TDrawMode);
    procedure SetEnableSelectedFrame(AValue: boolean);
    procedure SetLoading(AValue: boolean);
    procedure SetLocked(AValue: boolean);
    procedure SetOrtho(AValue: boolean);
    procedure SetpClosed(AValue: TPen);
    procedure SetpCrossed(AValue: TPen);
    procedure SetpFazis(AValue: integer);
    procedure SetpOpened(AValue: TPen);
    procedure SetpSelected(AValue: TPen);
    procedure SetpSigned(AValue: TPen);
    procedure SetpSorted(AValue: TPen);
    procedure SetSablonSzinkron(AValue: boolean);
    procedure SetSensitiveRadius(AValue: integer);
    procedure SetShowNumbers(AValue: boolean);
    procedure SetSTOP(AValue: boolean);
    procedure SetTitleFont(AValue: TFont);
    procedure SetVisibleContours(AValue: Boolean);
    procedure SetWorkArea(AValue: TRect);
    procedure SetWorking(const Value: boolean);
  protected
    H                   : integer;    // New Curve handle
    FCurve              : TCurve;     // Cuve for general purpose
    TempCurve           : TCurve;     // Temporary curve for not poligonized objects: Ex. Spline
    tegla               : Tteglalap;  // Roteted rectangle
    polygonContinue     : boolean;    // Polygon: continue polígon drawing;
    oldActionMode       : TActionMode;
    MaxPointsCount      : integer;    // Max. point in Curve
    MouseOn             : boolean;    // True=pressed left mouse button
    painting            : boolean;
    Zooming             : boolean;
    Moving              : boolean;
    RotCentrum          : TPoint2d;   // Centrum of rotation
    RotStartAngle       : TFloat;     // Rotate curves start angle
    RotAngle            : TFloat;     // Rotation angle
    oldCursor           : TCursor;
    UR                  : TUndoRedo;  // Undo-Redo object
    InnerStream         : TMemoryStream; // memorystream for inner use
    Delta               : TFloat;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure UndoStart;
    procedure UndoStop;
  public
    Mouse_Pos     : TPoint;     // Mouse x,y position
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
    defLength     : double;     // defined length of line
    ClipboardStr  : WideString; // Save draw to clipboard as text
    StrCurve      : TCurve;     // Text for draging

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
    procedure AfterDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
    procedure BeforePaint(Sender: TObject);
    procedure AfterPaint(Sender: TObject);

    function MakeCurve(const AName: Str32; ID: integer; Shape: TDrawMode;
             AEnabled, AVisible, AClosed: Boolean): Integer;
    procedure MakeNewCurve(AIndex: integer; var cuv: TCurve);
    procedure Clear;
    function  AddCurve(ACurve: TCurve):integer;
    procedure DeleteCurve(AItem: Integer);
    procedure DeleteSelectedCurves;
    procedure DeleteInvisibleCurves;
    procedure DeleteEmptyCurves;
    procedure InsertCurve(AIndex: Integer; Curve: TCurve);
    function  GetCurveName(H: Integer): Str32;
    function  GetCurveHandle(AName: Str32; var H: Integer): Boolean;
    function  GetCurveIndex(AName: Str32): Integer;
    function  LastCurve: TCurve;
    function  ShapeCount(Shape: TDrawMode): Integer;
    procedure CloneCurve(AIndex: integer);
    procedure CloneContour(AIndex: integer);
    procedure CloneSeledted;
    procedure CreateBoxObject(AIndex: integer);
    procedure CuttingObject(AIndex: integer);
    function  IsCutObject(p1,p2: TPoint2d; var Aindex: integer): boolean;
    function ConturInOut(cCuv: TCurve; AP, BP: TPoint2d; var BE,
      KI: TInOutRec): integer;
    procedure ShowMagneticCircle(x,y: TFloat; enab: boolean);

    procedure AddPoint(AIndex: Integer; X, Y: TFloat); overload;
    procedure AddPoint(AIndex: Integer; P: TPoint2d); overload;
    procedure InsertPoint(AIndex,APosition: Integer; X,Y: TFloat); overload;
    procedure InsertPoint(AIndex,APosition: Integer; P: TPoint2d); overload;
    procedure DeletePoint(AIndex,APosition: Integer);
    procedure DeleteSamePoints(diff: TFloat);
    procedure ChangePoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure DoMove(Dx,Dy: Integer);  // Move a point in curve
    procedure GetPoint(AIndex,APosition: Integer; var X,Y: TFloat);
    function  GetMaxPoints: Integer;
    function  GetNearestPoint(p: TPoint2d; var cuvIdx, pIdx: integer): TFloat;
    procedure SetNearestBeginPoint(p: TPoint2d);
    procedure SetBeginPoint(ACurve,AIndex: Integer);
    procedure SetOutherBeginPoint(Ax,Ay: TFloat);

    procedure MoveCurve(AIndex :integer; Ax, Ay: TFloat);
    procedure MoveSelectedCurves(Ax,Ay: TFloat);
    procedure RotateSelectedCurves(Cent : TPoint2d; Angle: TFloat);
    procedure InversSelectedCurves;
    procedure InversCurve(AIndex: Integer);
    procedure SelectCurveByName(aName: string);
    procedure SelectCurve(AIndex: Integer);
    procedure PoligonizeAll(PointCount: integer);
    procedure PoligonizeAllSelected(PointCount: integer);
    procedure Poligonize(Cuv: TCurve; PointCount: integer);
    procedure VektorisationAll(MaxDiff: TFloat);
    procedure VektorisationAllSelected(MaxDiff: TFloat);
    procedure Vektorisation(MaxDiff: TFloat; Cuv: TCurve);
    procedure PontSurites(Cuv: TCurve; Dist: double);
    procedure PontSuritesAll(Dist: double);
    procedure CheckCurvePoints(X, Y: Integer);

    procedure SelectAll(all: boolean);
    procedure SelectAllInArea(R: TRect2D);
    procedure SelectAllInAreaEx(R: TRect2d); // Select only points
    procedure AddSelectedToSelectedFrame;
    procedure ClosedAll(all: boolean);
    procedure BombAll;                      // Bomb for lines
    procedure SelectAllCut;                 // Vágási segédvonalak
    procedure SelectAllPolylines;
    procedure SelectAllPolygons;
    procedure SelectParentObjects;
    procedure SelectChildObjects;
    function  GetSelectedCount: integer;
    function  GetSelectArea( var RArea: TRect2d): boolean;
    procedure ChangeSelectedShape( newShape: TDrawMode );

    procedure EnabledAll(all: boolean);
    procedure SignedAll(all: boolean);
    procedure CrossedAll(all: boolean);
    procedure SortedAll(all: boolean);
    procedure SignedNotCutting;
    function  GetSignedCount: integer;
    function  GetCrossedCount: integer;

    procedure JoinSelected;
    procedure MirrorSeledted(Horiz, Vert: boolean);
    procedure SetAllDirect;

    { Transformations }
    procedure Normalisation(Down: boolean);
    procedure NormalisationEx(Down: boolean);
    procedure Eltolas(dx,dy: double);
    procedure Nyujtas(tenyezo:double);
    procedure CentralisNyujtas(Cent: TPoint2d; tenyezo: double);
    procedure MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
    procedure MirrorHorizontal;
    procedure MirrorVertical;
    procedure MirrorCentral;

    { Virtual Clipboard procedures }
    procedure CopySelectedToVirtClipboard;
    procedure CutSelectedToVirtClipboard;
    procedure PasteSelectedFromVirtClipboard;
    procedure CopyScreenToClipboard;
    procedure SaveGraphToMemoryStream(var stm: TMemoryStream);
    procedure LoadGraphFromMemoryStream(stm: TMemoryStream);

    {Undo,Redo}
    procedure UndoInit;
    procedure Undo;
    procedure Redo;
    procedure UndoSave;
    procedure UndoRedo(Sender:TObject; Undo,Redo:boolean);

    // SelectedFrame
    procedure DoSelectedFrame(aSave: boolean);
    procedure Draw_Curve(Cuv: TCurve; co: TColor);
    procedure DrawNode(p: TPoint2d; m: integer; Fill: boolean; co: TColor);
    procedure DrawSelectedFrame;
    procedure LoadSelectedFrame;
    procedure AdjustSelectedFrame;
    function IsNode(p: TPoint2d; Radius: double; var idx: integer): boolean;

    { Contours }
    procedure SetContour(AIndex: Integer; Radius: double);
    procedure SetAllContour;

    { Clipper }
    function GetClipperContour(Cuv: TCurve; dist: double): TCurve;
    procedure ClipperBool(ClipType: TClipType);
    procedure cUnion;
    procedure cIntersection;
    procedure cDifference;
    procedure cXor;

    procedure ReCall;

    // Enable/disable Cutter Nachibe specific methods
    property EnableCutterMethods: boolean read FEnableCutterMethods
                                          write FEnableCutterMethods
                                          default True;

    // Drawing state: 0=begin; 1=continue
    property pFazis: integer read fpFazis write SetpFazis;    // Drawing phase
    property WorkArea: TRect read FWorkArea write SetWorkArea;
    property Loading: boolean read FLoading write SetLoading;
    property SelectedCount: integer read GetSelectedCount;
    property DisabledCount: integer read GetDisabledCount;
    property CentralisZoom: boolean read FCentralisZoom write FCentralisZoom;

    property ActionMode    : TActionMode read fActionMode write SetActionMode;
    property ActLayer      : integer read fActLayer write fActLayer default 0;
    property AutoPoligonize: boolean read fAutoPoligonize write fAutoPoligonize;
    property AutoUndo      : boolean read fAutoUndo write fAutoUndo;
    property BackImage     : TBMPObject  read FBackImage write SetBackImage;
    property Changed       : boolean read fChanged write fChanged;
             // Kontúr vonal távolsága egyedi objektum kontúrozáshoz
    property ContourRadius : double      read FContourRadius write SetContourRadius;
    property Clone_Contour : double  read FClone_Contour write FClone_Contour;
    property VisibleContours:Boolean read FVisibleContours write SetVisibleContours;
    property DefaultLayer  : Byte    read fDefaultLayer write SetDefaultLayer default 0;
    property DesigneMode   : boolean read FDesigneMode write SetDesigneMode default False;
    property DrawMode      : TDrawMode read FDrawMode write SetDrawMode;
    property FileName      : string  read FFilename write SetFilename;
    property Gravity       : boolean read FGravity write FGravity default False;
    property Locked        : boolean read fLocked write SetLocked;  // Editable?
    property Ortho         : boolean read FOrtho write SetOrtho default false;
    property SablonSzinkron: boolean read FSablonSzinkron write SetSablonSzinkron;
             // Cursor sensitive radius of circle around of curves' points
    property SensitiveRadius:integer read FSensitiveRadius write SetSensitiveRadius;
             // Drawing an inclusion rectangle (Befoglaló téglalap rajzolása)
    property ShowBoxes     : boolean read FBoxing write SetBoxing;
    property ShowNumbers   : boolean read fShowNumbers write SetShowNumbers;
    property TitleFont     : TFont   read FTitleFont write SetTitleFont;
    property XYCompulsion  : boolean read FXYCompulsion write FXYCompulsion default False;
             // SelectedFrame
    property EnableSelectedFrame : boolean read FEnableSelectedFrame write SetEnableSelectedFrame;
    property SelectedFrame       : TSelectedArea read FSelectedFrame write FSelectedFrame;
             // Pre definied length of line
    property DefiniedLength      : boolean read FDefiniedLength write fDefiniedLength;
    property STOP                : boolean read FSTOP write SetSTOP;
    // Vectorization Coefficient = Vektorizálási tűrés
    property VecCoeff      : double read FVecCoeff write FVecCoeff;
    property Working       : boolean read fWorking write SetWorking;

    property OnChangeAll      : TNotifyEvent read fChangeAll write fChangeAll;
    property OnChangeCurve    : TChangeCurve read fChangeCurve write fChangeCurve;
    property OnChangeMode     : TChangeMode read fChangeMode write fChangeMode;
    property OnChangeSelected : TChangeCurve read fChangeSelected write fChangeSelected;
    property OnChangeWindow   : TChangeWindow read fChangeWindow write fChangeWindow;
    property OnMouseEnter     : TMouseEnter read FMouseEnter write FMouseEnter;
    property OnMouseLeave     : TMouseEnter read FMouseLeave write FMouseLeave;
    property OnNewBeginPoint  : TNewBeginPoint read fNewBeginPoint write fNewBeginPoint;
    property OnUndoRedoChange : TUndoRedoChangeEvent read FUndoRedoChangeEvent
             write FUndoRedoChangeEvent;
    property OnSelectedFrame  : TNotifyEvent read fOnSelectedFrame write fOnSelectedFrame;
    property PointWidth;
  end;

  { TalCADView }

  TalCADView = class(TCustomCADView)
  published
    property Append;
    property BackColor;
    property CadPens;
    property CADSource;
    property CoordLabel;
    property Hinted;
    property CoordHint;
    property CentralCross;
    property CursorCross;
    property EnablePaint;
    property EnableRecall;
    property Filled;
    property FillBrush;
    property Grid;
    property Paper;
    property PaperColor;
    property PaperVisible;
    property PointWidth;
    property ShowPoints;
    property Zoom;
    property CentralisZoom;
    property VisibleBeginPoint;

    property OnResize;
    property OnChangeWindow;
    property OnBeforePaint;
    { Before draw the next object/curve Ex. change pen .... }
    property OnBeforeDraw;
    property OnAfterDraw;
    property OnAfterPaint;
    property OnMouseEnter;
    property OnMouseLeave;

    property Align;
    property Anchors;
    property Caption;
    property Enabled;
    property Font;
    property Hint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
  end;

  { TalCAD }

  TalCAD = class(TCustomCAD)
  published
    property ActionMode;
    property ActLayer;
    property Append;
    property AutoPoligonize;
    property AutoUndo;
    property BackColor;
    property BackImage;
    property CADSource;
    property CadPens;
    property CentralCross;
    property CursorCross;
    property Clone_Contour;
    property CoordLabel;
    property ContourRadius;
    property Changed;
    property DefaultLayer;
    property DesigneMode;
    property DrawMode;
    property EnablePaint;
    property EnableRecall;
    property FileName;
    property GraphTitle;
    property Filled;
    property FillBrush;
    property Grid;
    property Gravity;
    property Hinted;
    property CoordHint;
    property Locked;
    property Ortho;
    property Paper;
    property PaperColor;
    property PaperVisible;
    property PointWidth;
    property ShowPoints;
    property SablonSzinkron;
    property SensitiveRadius;
    property SelectedIndex;
    property SelectedVisible;
    property ShowBoxes;
    property ShowNumbers;
    property TitleFont;
    property XYCompulsion;
    property EnableSelectedFrame;
    property SelectedFrame;
    property DefiniedLength;
    property Zoom;
    property CentralisZoom;
    property VisibleBeginPoint;
    property VisibleContours;
    property VisibleObjects;

    property Align;
    property Anchors;
    property Caption;
    property Enabled;
    property Font;
    property Hint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnResize;
    property OnBeforePaint;
    property OnBeforeDraw;
    property OnAfterPaint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeAll;
    property OnChangeCurve;
    property OnChangeMode;
    property OnChangeSelected;
    property OnChangeWindow;
    property OnNewBeginPoint;
    property OnUndoRedoChange;
    property OnSelectedFrame;

    property OnClick;
    property OnContextPopup;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
  end;

  { TALSablon }

  TALSablon = class(TCustomCAD)
  private
//    FAutoCutMethod: integer;
    FDownUp: boolean;
    FMMPerLepes: extended;
    FPlan: TProcess;
    fWorkOrigo: TPoint2d;
    function GetTotalWorkWay: double;
//    procedure SetAutoCutMethod(AValue: integer);
    procedure SetDownUp(AValue: boolean);
    procedure SetWorkOrigo(AValue: TPoint2d);
  protected
  public
    WorkPosition  : TWorkPosition;
    kesleltetes   : double;     // A test vágás lépései közti szünet

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
    procedure AfterDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
    procedure AfterPaint(Sender: TObject);

    {Automatkus objektum sorrend képzés}
    procedure AutoSortObject(BasePoint: TPoint2d); overload;
    procedure AutoSortObject(BasePoint: TPoint2d; Connecting: boolean); overload;
    procedure ReorderObjects;
    procedure Lazitas(coeff: TFloat);
    procedure Optimalisation;
    procedure AutoCutSequence(BasePoint: TPoint2d; Sorting: boolean;
                              CutMethod: byte);
//   procedure AutoSTRIP(FileName: string; BasePoint: TPoint2d);
   procedure StripObj12(AParent,Achild: integer);
   procedure StripChildToParent(AIndex: integer);
   procedure StripAll;
   procedure StripAll1;

   procedure InitParentObjects;
   function  IsParent(AIndex: Integer): boolean; overload;
   function  IsParent(x, y: TFloat): boolean; overload;
   function  GetInnerObjectsCount(AIndex: Integer): integer; overload;
   function  GetInnerObjectsCount(Cuv: TCurve): integer; overload;
   function  GetParentObject(AIndex: Integer): integer; overload;
   function  GetParentObject(x,y: TFloat): integer; overload;
   function  GetRealParentObject(AIndex: Integer): integer;

   procedure ContourOptimalizalas(Cuv: TCurve);
   function  ElkerulesAB(Var eCurve: TCurve): boolean;
   procedure Elkerules;
   procedure Elkerules1;

       { Working }
    procedure DrawWorkPoint(x,y:double);
    procedure ClearWorkPoint;
    procedure WorkpositionToCentrum;
    procedure TestVekOut(dx,dy:extended);
    procedure TestWorking(AObject,AItem:integer);

    Property TotalWorkWay: double read GetTotalWorkWay;
    property WorkOrigo : TPoint2d read fWorkOrigo write SetWorkOrigo;

  published
//   property AutoCutMethod : integer  read FAutoCutMethod write SetAutoCutMethod;
   property DownUp    : boolean  read FDownUp write SetDownUp;
   property MMPerLepes: extended read FMMPerLepes write FMMPerLepes;
   property OnPlan    : TProcess read FPlan write FPlan; // Event for autocut percent

   property EnableCutterMethods;
   property ActionMode;
   property ActLayer;
   property Append;
   property AutoPoligonize;
   property AutoUndo;
   property BackColor;
   property BackImage;
   property CADSource;
   property CadPens;
   property Centrum;
   property CentralCross;
   property CursorCross;
   property Clone_Contour;
   property CoordLabel;
   property ContourRadius;
   property Changed;
   property DefaultLayer;
   property DesigneMode;
   property DrawMode;
   property EnablePaint;
   property EnableRecall;
   property FileName;
   property GraphTitle;
   property Filled;
   property FillBrush;
   property Grid;
   property Gravity;
   property Hinted;
   property CoordHint;
   property Locked;
   property Ortho;
   property Paper;
   property PaperColor;
   property PaperVisible;
   property PointWidth;
   property ShowPoints;
   property SablonSzinkron;
   property SensitiveRadius;
   property SelectedIndex;
   property SelectedVisible;
   property ShowBoxes;
   property ShowNumbers;
   property STOP;
   property TitleFont;
   property XYCompulsion;
   property EnableSelectedFrame;
   property SelectedFrame;
   property DefiniedLength;
   property Zoom;
   property CentralisZoom;
   property VisibleBeginPoint;
   property VisibleContours;
   property VisibleObjects;
   property Working;

   property Align;
   property Anchors;
   property Caption;
   property Enabled;
   property Font;
   property Hint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop;
   property Visible;

   property OnResize;
   property OnBeforePaint;
   property OnBeforeDraw;
   property OnAfterPaint;
   property OnMouseEnter;
   property OnMouseLeave;
   property OnChangeAll;
   property OnChangeCurve;
   property OnChangeMode;
   property OnChangeSelected;
   property OnChangeWindow;
   property OnNewBeginPoint;
   property OnUndoRedoChange;
   property OnSelectedFrame;

   property OnClick;
   property OnContextPopup;
   property OnConstrainedResize;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMouseMove;
   property OnMouseDown;
   property OnMouseUp;
   property OnMouseWheel;
   property OnMouseWheelDown;
   property OnMouseWheelUp;
   property OnStartDrag;
  end;

Var
    VirtualClipboard : TMemoryStream;   // Store List of vectorial curves for public
    ClipboardStr     : WideString;      // Save draw to clipboard as text

{ =============== Other routins ============================================== }

procedure DrawShape(Canvas: TCanvas; T,B: TPoint; DrawMode: TDrawMode;
                            AMode: TPenMode);
procedure FillActionStrings(st: TStrings);
procedure FillDrawmodeStrings(st: TStrings);
function  CheckForOverLaps( Cuv1, Cuv2 : TCurve): boolean;

procedure Register;

implementation

//Uses LResources;

procedure TALSablon.SetDownUp(AValue: boolean);
begin
  FDownUp:=AValue;
  if FDownUp then
     WorkOrigo := Point2d(0,Paper.y)
  else
     WorkOrigo := Point2d(0,0);
  invalidate;
end;

function TALSablon.GetTotalWorkWay: double;
// A teljes sablon hossza mm-ben
var i,j : integer;
    x,y,ox,oy : TFloat;
    Cuv : TCurve;
begin
If ComponentState=[] then begin
  Result := 0;
  ox:=WorkOrigo.x; oy:=WorkOrigo.Y;
  For i:=0 to FCADSource.FCurveList.Count-1 do begin
       Cuv := FCADSource.FCurveList.Items[i];
        For j:=0 to Cuv.FPoints.Count-1 do begin
            Cuv.GetPoint(j,x,y);
            Result := Result + sqrt(sqr(x-ox)+sqr(y-oy));
            ox:=x; oy:=y;
        end;
        If Cuv.Closed then begin
            Cuv.GetPoint(0,x,y);
            Result := Result + sqrt(sqr(x-ox)+sqr(y-oy));
            ox:=x; oy:=y;
        end;
  end;
end;
end;

(*
procedure TALSablon.SetAutoCutMethod(AValue: integer);
begin
  if FAutoCutMethod=AValue then Exit;
  FAutoCutMethod:=AValue;
end;
*)
procedure TALSablon.SetWorkOrigo(AValue: TPoint2d);
begin
  fWorkOrigo:=AValue;
  invalidate;
end;

{ TALSablon }
(*
procedure TALSablon.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TALSablon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TALSablon.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

function TALSablon.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;
*)
constructor TALSablon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownUp      := False;
  FWorkOrigo   := Point2d(0,0);
  FMMPerLepes  := 4;
  OnBeforeDraw := BeforeDraw;
  OnAfterDraw  := AfterDraw;
  OnAfterPaint := AfterPaint;
end;

destructor TALSablon.Destroy;
begin
  if Self<>nil then
     inherited Destroy;
end;

procedure TALSablon.BeforeDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
begin
  inherited;
end;

procedure TALSablon.AfterDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
begin
  inherited;
end;

procedure TALSablon.AfterPaint(Sender: TObject);
begin
  DrawWorkPoint(WorkOrigo.x,WorkOrigo.y);
  inherited;
end;

procedure TALSablon.AutoSortObject(BasePoint: TPoint2d);
{Automatkus objektum sorrend képzés}
var i,j,idx: integer;
    x,y,x1,y1,d,dd : double;
    p0,p : TPoint2d;
    ts: TMemoryStream;
    Closed,Begining,Continue : boolean;
    CurveCount : integer;
    BaseCurve,nextCurve : TCurve;
    pp: PPointRec;
    curve0 : integer;  //Curve sorszáma amit objektummá növelünk
    CuvIdx,NodeIdx: integer;
begin

if FCADSource.FCurveList.Count>1 then
Try
Try
  oldCursor := Cursor;
  Cursor := crHourGlass;
  Loading := True;
  ts:= TMemoryStream.Create;
  VektorisationAll(VecCoeff);
  CurveCount := 0;
  p0 := BasePoint;
  While FCADSource.FCurveList.Count>0 do begin
        d := 1000000000;
        idx := -1;
        for I:=0 to Pred(FCADSource.FCurveList.Count) do
        begin
            FCurve:=FCADSource.FCurveList.Items[I];
               FCurve.GetPoint(0,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := True;
               end;
               FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := False;
               end;
        end;
        FCurve:=FCADSource.FCurveList.Items[Idx];
        if not Begining then FCurve.InversPointOrder;

        IF FCurve.Closed {or Begining} then
           FCurve.GetPoint(0,X,Y)     // Zárt alakzat első pontja
        else
           FCurve.GetPoint(Pred(FCurve.FPoints.Count),X,Y);  // Nyiltak utolsó pontja

        FCADSource.FCurveList.SaveCurveToStream(ts,Idx);
        Inc(CurveCount);
        p0 := Point2d(X,Y);
        FCADSource.FCurveList.Delete(Idx);
  end;
finally
  // A ts stream-re rendezett alakzatok visszatöltése
  FCADSource.FCurveList.Clear;
  ts.seek(0,0);
  FCADSource.FCurveList.LoadCurvesFromStream(ts);

  // Kapcsolt objektumokból egyetlen objektum képzése

  Begining := True;
  i:=1;
  idx:=0;
  Repeat
      FCurve:=FCADSource.FCurveList.Items[I-1];
      nextCurve:=FCADSource.FCurveList.Items[I];
      nextCurve.GetPoint(0,X1,Y1);
      FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
      dd:=KeTPontTavolsaga(x1,y1,X,Y);
      Continue:=dd<1;
      if begining then begin
         FCurve.GetPoint(0,X,Y);
         Inc(idx);
         p0 := Point2d(X,Y);
         BaseCurve := FCurve;
         FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
         begining:=False;
      end;
      if Continue then begin
            nextCurve.AbsolutClosed;
            for j:=0 to Pred(nextCurve.FPoints.Count) do begin
                pp := nextCurve.FPoints.Items[j];
                p.x := pp^.x;
                p.y := pp^.y;
                BaseCurve.AddPoint(p.X,p.Y);
            end;
            FCADSource.FCurveList.Delete(i);
            BaseCurve.Shape := dmPolyLine;
            BaseCurve.Selected := True;
      end else begin
            if not BaseCurve.Closed then
               BaseCurve.Closed := (ABS(P0.X-X)<1) and (ABS(P0.Y-Y)<1);
            if BaseCurve.Closed then BaseCurve.Shape := dmPolyGon;
            begining := True;
            Inc(i);
      end;
  Until i>=FCADSource.FCurveList.Count;

  if not BaseCurve.Closed then
  begin
     p0 := BaseCurve.Points[0];
     p  := BaseCurve.LastPoint;
     BaseCurve.Closed := (ABS(P0.X-p.X)<1) and (ABS(P0.Y-p.Y)<1);
     if BaseCurve.Closed then BaseCurve.Shape := dmPolyGon;
  end;

  ts.Free;
  UndoSave;
  Cursor := oldCursor;
  Invalidate;
  Loading := False;
end;
except
  Cursor := oldCursor;
  Invalidate;
  Loading := False;
end;
end;


procedure TALSablon.AutoSortObject(BasePoint: TPoint2d; Connecting: boolean);
{Automatkus objektum sorrend képzés}
{Automatkus objektum sorrend képzés}
(*var i,j,idx: integer;
    CuvIdx,NodeIdx: integer;
    x,y,x1,y1,d,dd : double;
    p0,p : TPoint2d;
    ts: TMemoryStream;
    Closed,Begining,Continue : boolean;
    CurveCount : integer;
    BaseCurve,nextCurve : TCurve;
    pp: PPointRec;
    curve0 : integer;  //Curve sorszáma amit objektummá növelünk
begin
if FCADSource.FCurveList.Count>1 then
Try
Try
  AutoUndo := False;
  oldCursor := Cursor;
  Cursor := crHourGlass;
  Loading := True;
  ts:= TMemoryStream.Create;
  StripAll1;
  CurveCount := 0;
  p0 := BasePoint;
  // A legközelebbi objektum legközelebbi pontja lesz a kezdőpont
  GetNearestPoint( BasePoint,CuvIdx,NodeIdx );
  SetBeginPoint( CuvIdx,NodeIdx );

  While FCADSource.FCurveList.Count>0 do begin
        d := 1000000000;
        idx := -1;
        for I:=0 to Pred(FCADSource.FCurveList.Count) do
        begin
            FCurve:=FCADSource.FCurveList.Items[I];
               FCurve.GetPoint(0,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := True;
               end;

               FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := False;
               end;

        end;

        FCurve:=FCADSource.FCurveList.Items[Idx];

        IF FCurve.Closed {or Begining} then
        begin
           FCurve.GetPoint(0,X,Y)     // Zárt alakzat első pontja
        end
        else
        begin
           FCurve.GetPoint(Pred(FCurve.FPoints.Count),X,Y);  // Nyiltak utolsó pontja
        end;

        if not Begining then FCurve.InversPointOrder;
        FCADSource.FCurveList.SaveCurveToStream(ts,Idx);
        Inc(CurveCount);
        p0 := Point2d(X,Y);
        FCADSource.FCurveList.Delete(Idx);

        GetNearestPoint( p0,CuvIdx,NodeIdx );
        SetBeginPoint( CuvIdx,NodeIdx );
  end;
finally
  // A ts stream-re rendezett alakzatok visszatöltése
  FCADSource.FCurveList.Clear;
  ts.seek(0,0);
  FCADSource.FCurveList.LoadCurvesFromStream(ts);

  // Kapcsolt objektumokból egyetlen objektum képzése
  if Connecting then begin
  Begining := True;
  i:=1;
  idx:=0;
  Repeat
      FCurve:=FCADSource.FCurveList.Items[I-1];
      nextCurve:=FCADSource.FCurveList.Items[I];
      nextCurve.GetPoint(0,X1,Y1);
      FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
      dd:=KeTPontTavolsaga(x1,y1,X,Y);
      Continue:=dd<0.5;
      if begining then begin
         FCurve.GetPoint(0,X,Y);
         Inc(idx);
         p0 := Point2d(X,Y);
         BaseCurve := FCurve;
         FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
         begining:=False;
      end;
      if Continue then begin
            nextCurve.AbsolutClosed;
            for j:=0 to Pred(nextCurve.FPoints.Count) do begin
                pp := nextCurve.FPoints.Items[j];
                p.x := pp^.x;
                p.y := pp^.y;
                BaseCurve.AddPoint(p.X,p.Y);
            end;
            FCADSource.FCurveList.Delete(i);
      end else begin
            if not BaseCurve.Closed then
               BaseCurve.Closed := (ABS(P0.X-X)<0.5) and (ABS(P0.Y-Y)<0.5);
            if BaseCurve.Closed then BaseCurve.Shape := dmPolygon
               else BaseCurve.Shape := dmPolyline;
            begining := True;
            Inc(i);
      end;
  Until i>=FCADSource.FCurveList.Count;
  end;

           if not BaseCurve.Closed then begin
              dd:=KeTPontTavolsaga(BaseCurve.Points[0],BaseCurve.LastPoint);
              BaseCurve.Closed := dd<0.5;
           end;
           if BaseCurve.Closed then BaseCurve.Shape := dmPolygon
              else BaseCurve.Shape := dmPolyline;

  ts.Free;
  VektorisationAll(VecCoeff);

  If AutoUndo then UndoSave;
  Cursor := oldCursor;
  Invalidate;
  AutoUndo := False;
  UndoSave;
  Loading := False;
end;
except
  Cursor := oldCursor;
  Invalidate;
  AutoUndo := False;
  UndoSave;
  Loading := False;
end;
end;
*)


var i,j,idx: integer;
    CuvIdx,NodeIdx: integer;
    x,y,x1,y1,d,dd : double;
    p0,p : TPoint2d;
    ts: TMemoryStream;
    Closed,Begining,Continue : boolean;
    CurveCount : integer;
    BaseCurve,nextCurve : TCurve;
    pp: PPointRec;
    curve0 : integer;  //Curve sorszáma amit objektummá növelünk
begin
if FCADSource.FCurveList.Count>1 then
Try
Try
  oldCursor := Cursor;
  Cursor := crHourGlass;
  Loading := True;
  ts:= TMemoryStream.Create;
  CurveCount := 0;
  p0 := BasePoint;
  // A legközelebbi objektum legközelebbi pontja lesz a kezdőpont
  GetNearestPoint( BasePoint,CuvIdx,NodeIdx );
  SetBeginPoint( CuvIdx,NodeIdx );

  While FCADSource.FCurveList.Count>0 do begin
        d := 1000000000;
        idx := -1;
        for I:=0 to Pred(FCADSource.FCurveList.Count) do
        begin
            FCurve:=FCADSource.FCurveList.Items[I];
               FCurve.GetPoint(0,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := True;
               end;

               FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
               dd:=KeTPontTavolsaga(p0.x,p0.y,X,Y);
               if dd<=d then begin
                  d := dd;
                  idx := i;
                  Begining := False;
               end;

        end;

        FCurve:=FCADSource.FCurveList.Items[Idx];
        if not Begining then FCurve.InversPointOrder;

        IF FCurve.Closed {or Begining} then
           FCurve.GetPoint(0,X,Y)     // Zárt alakzat első pontja
        else
           FCurve.GetPoint(Pred(FCurve.FPoints.Count),X,Y);  // Nyiltak utolsó pontja

        FCADSource.FCurveList.SaveCurveToStream(ts,Idx);
        Inc(CurveCount);
        p0 := Point2d(X,Y);
        FCADSource.FCurveList.Delete(Idx);

  end;
finally
  // A ts stream-re rendezett alakzatok visszatöltése
  FCADSource.FCurveList.Clear;
  ts.seek(0,0);
  FCADSource.FCurveList.LoadCurvesFromStream(ts);

  // Kapcsolt objektumokból egyetlen objektum képzése
  if Connecting then begin
  Begining := True;
  i:=1;
  idx:=0;
  Repeat
      FCurve:=FCADSource.FCurveList.Items[I-1];
      nextCurve:=FCADSource.FCurveList.Items[I];
      nextCurve.GetPoint(0,X1,Y1);
      FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
      dd:=KeTPontTavolsaga(x1,y1,X,Y);
      Continue:=dd<0.5;
      if begining then begin
         FCurve.GetPoint(0,X,Y);
         Inc(idx);
         p0 := Point2d(X,Y);
         BaseCurve := FCurve;
         FCurve.GetPoint(FCurve.FPoints.Count-1,X,Y);
         begining:=False;
      end;
      if Continue then begin
            nextCurve.AbsolutClosed;
            for j:=0 to Pred(nextCurve.FPoints.Count) do begin
                pp := nextCurve.FPoints.Items[j];
                p.x := pp^.x;
                p.y := pp^.y;
                BaseCurve.AddPoint(p.X,p.Y);
            end;
            FCADSource.FCurveList.Delete(i);
      end else begin
            if not BaseCurve.Closed then
               BaseCurve.Closed := (ABS(P0.X-X)<0.5) and (ABS(P0.Y-Y)<0.5);
            if BaseCurve.Closed then BaseCurve.Shape := dmPolygon
               else BaseCurve.Shape := dmPolyline;
            begining := True;
            Inc(i);
      end;
  Until i>=FCADSource.FCurveList.Count;
  end;

           if not BaseCurve.Closed then begin
              dd:=KeTPontTavolsaga(BaseCurve.Points[0],BaseCurve.LastPoint);
              BaseCurve.Closed := dd<0.5;
           end;
           if BaseCurve.Closed then BaseCurve.Shape := dmPolygon
              else BaseCurve.Shape := dmPolyline;

  ts.Free;
  If AutoUndo then UndoSave;
  Cursor := oldCursor;
  Invalidate;
  Loading := False;
end;
except
  Cursor := oldCursor;
  Invalidate;
  Loading := False;
end;
end;


// Fordított Objektum sorrend
procedure TALSablon.ReorderObjects;
var i :  integer;
    ts:  TMemoryStream;
begin
Try
  ts := TMemoryStream.Create;
  for I:=Pred(FCADSource.FCurveList.Count) downto 0 do
            FCADSource.FCurveList.SaveCurveToStream(ts,i);
  FCADSource.FCurveList.Clear;
  ts.Seek(0,0);
  FCADSource.FCurveList.LoadCurvesFromStream(ts);
finally
  ts.Free;
end;
end;

// Az objektumokat eltávolítja egymástól,
// de a méretüket nem módosítja
procedure TALSablon.Lazitas(coeff: TFloat);
var H: integer;
    dx,dy: TFloat;
    basePoint : TPoint2d;
    R: TRect2d;
begin
  StripAll;
  for H:=0 to Pred(FCADSource.FCurveList.Count) do
  begin
    FCurve:=Curves[H];
//    R := FCurve.BoundsRect;
//    basePoint := Point2d((R.x2+R.x1)/2,(R.y2+R.y1)/2);
    basePoint := FCurve.Points[0];
    dx := (coeff - 1) * basePoint.x;
    dy := (coeff - 1) * basePoint.y;
    FCurve.MoveCurve(dx,dy);
  end;
  Repaint;
end;

procedure TALSablon.Optimalisation;
begin
  Loading := True;
  SelectAll(False);
  PoligonizeAll(0);
  SelectAllCut;
  SelectAllPolylines;
  DeleteSelectedCurves;
  StripAll;
  VektorisationAll(0.05);
  SetAllDirect;
//  DeleteSamePoints(0.1);
//  BombAll;
//  AutoSortObject(WorkOrigo);
  SelectAll(False);
  Loading := False;
end;

// Automatikus vágási terv készítés
procedure TALSablon.AutoCutSequence(BasePoint: TPoint2d; Sorting: boolean;
                                    CutMethod: byte);
var i,j,idx: integer;
    x,y,d,dd : double;
    p0,p,p1 : TPoint2d;
    BaseCurve : TCurve;
    Cuv,CC    : TCurve;
    cuvIDX,pIdx: integer;
    Child: boolean;
    KonturHossz: double;     // Kontúr hossza
    KonturSzelet: double;    // Kontúr egy szeletének hossza
    cCount: integer;
    R      : TRect2d;
    sc     : integer;
    dx,dy,m: double;
    h      : integer;

label ide;

label Ujra;

    procedure VisibleAll;
    var ii: integer;
    begin
         For ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
             FCurve := FCADSource.FCurveList.Items[ii];
             FCurve.Visible:=True;
         end;
    end;

    function VisibleCount: integer;
    var ii: integer;
    begin
         Result := 0;
         For ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
             FCurve := FCADSource.FCurveList.Items[ii];
             if FCurve.Visible then
                Inc(Result);
         end;
    end;

    function NextVisible: integer;
    var ii: integer;
    begin
         Result := -1;
         For ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
             FCurve := FCADSource.FCurveList.Items[ii];
             if FCurve.Visible then begin
                Result := ii;
                exit;
             end;
         end;
    end;

begin

if (FCADSource.FCurveList.Count>0) and (ActionMode <> amAutoPlan) then
Try
  SelectedVisible := true;
  if Assigned(FPlan) then FPlan(Self,0,0);
  ActionMode := amAutoPlan;
  STOP := False;
  oldCursor := Cursor;
  Screen.Cursor := crHourGlass;
  Loading := True;
  AutoUndo := False;

  InnerStream.Clear;    // Ide rendezzük a vágási mintát

  if Assigned(FPlan) then FPlan(Self,0,0);


  // Töröljük az összes nyílt alakzatot
  SignedAll(False);
  VisibleAll;
  SelectAll(False);
//  SelectAllCut;
  SelectAllPolylines;
  DeleteSelectedCurves;
//  PoligonizeAll(0);
//  StripAll;
//  AutoSortObject(BasePoint);

  SetNearestBeginPoint(BasePoint);

  if Assigned(FPlan) then FPlan(Self,1,0);

  Case CutMethod of
  0,1 :
      begin
         R := GetDrawExtension;
         m := Grid.Margin;
         if m<10 then m:=10;
         dx := 2*m-R.x1;
         dy := Paper.y-2*m-R.y2;
         Eltolas(dx,dy);
         R := GetDrawExtension;

         // Keret létrehozása a rajz körül margin távolságban
         R := Rect2d(R.x1-m,R.y1-m,R.x2+m,R.y2+m);

         h:=MakeCurve('Border',-1,dmRectangle,True,True,True);
         AddPoint(H,R.x1,R.y1);
         AddPoint(H,R.x1,R.y2);
         AddPoint(H,R.x2,R.y2);
         AddPoint(H,R.x2,R.y1);

//         StripAll;
(*
         GetPoint(CuvIdx,0,x,y);
         // Első megközelítő vonal képzése Origóból
         idx:=MakeCurve('Cut',-1,dmPolyline,True,True,False);
         AddPoint(idx,p0.x,p0.y);
         AddPoint(idx,x,y);
*)
//         exit;

         CutMethod:=2;
      end;
  end;

  StripAll;
  SetAllContour;
  invalidate;

Try

  p0 := BasePoint;
  cuvIDX := 0;
  cCount := VisibleCount;


  if VisibleCount>0 then begin
        // Az első polygon kiolvasása
//        if Assigned(FAutoSortEvent) then FAutoSortEvent(Self,2,CuvIdx);
        if Sorting then begin
           GetNearestPoint(p0,cuvIDX,pIdx);
           SetBeginPoint(cuvIDX,pIdx);
        end else
           CuvIdx := NextVisible;
        BaseCurve := FCADSource.FCurveList.Items[cuvIDX];
        GetPoint(CuvIdx,0,x,y);
        // Első megközelítő vonal képzése Origóból
        idx:=MakeCurve('Cut',-1,dmPolyline,True,True,False);
        AddPoint(idx,p0.x,p0.y);
        AddPoint(idx,x,y);
        FCADSource.FCurveList.SaveCurveToStream(innerStream,IDX);
        if CutMethod<2 then
        FCADSource.FCurveList.SaveCurveToStream(innerStream,cuvIDX);
        DeleteCurve(IDX);
  end;

  invalidate;

  if CutMethod>1 then
  While VisibleCount>0 do begin

        // Kontúrozás és eredeti Polygon mentése
           BaseCurve := Curves[cuvIDX];

           SetContour(cuvIDX,ContourRadius);
           TempCurve := Curves[cuvIDX].Contour;
           if TempCurve.IsDirect then TempCurve.InversPointOrder;

           FCADSource.FCurveList.SaveCurveToStream(innerStream,cuvIDX);
           BaseCurve.Visible := False;
           p := BaseCurve.Contour.Points[0];

           if Assigned(FPlan) then FPlan(Self,2,Trunc((100/cCount)*cCount/(VisibleCount+1)));

        if VisibleCount>0 then begin

           // A következő polygon kiolvasása
           if Sorting then begin
              GetNearestPoint(p,cuvIDX,pIdx);
              BaseCurve := FCADSource.FCurveList.Items[cuvIDX];
              SetBeginPoint(cuvIDX,pIdx);
              GetPoint(cuvIDX,0,x,y);
              p1:=Point2d(x,y);
           end else begin
              CuvIdx := NextVisible;
              BaseCurve := FCADSource.FCurveList.Items[cuvIDX];
              p1 := BaseCurve.GetPoint2d(0);
           end;

              // Kontúron az optimális útvonal keresés a köv. objektumhoz
              idx:=MakeCurve('Cut',-1,dmPolyline,True,True,False);
              Cuv := Curves[idx];
              Cuv.Visible:=False;

              Cuv.AddPoint(p.x,p.y);
              SetContour(cuvIDX,ContourRadius);
              p1 := BaseCurve.Contour.Points[0];
              Cuv.AddPoint(p1.x,p1.y);



//              ContourOptimalizalas(Cuv);
//              p := BaseCurve.GetPoint2d(0);
//              Cuv.AddPoint(p.x,p.y);
              Cuv.Visible:=True;
              FCADSource.FCurveList.SaveCurveToStream(innerStream,IDX);
              DeleteCurve(IDX);
              Invalidate;
        end; // if

  end;

finally

  // A ts stream-re rendezett alakzatok visszatöltése
  FCADSource.FCurveList.Clear;
  innerStream.seek(0,0);
  FCADSource.FCurveList.LoadCurvesFromStream(innerStream);

  // Kilépés az utolsó objektumból a kontúr első pontjába
  idx:=MakeCurve('Cut',-1,dmPolyline,True,True,False);
  if CutMethod>1 then begin
     TempCurve := BaseCurve.Contour;
     TempCurve.GetPoint(0,x,y);
  end else begin
  end;

  AddPoint(idx,x,y);
  // Vissza az Workorigóba
  AddPoint(idx,BasePoint);

  InnerStream.Clear;
//  ReOrderNames;
  invalidate;

//  if Assigned(FPlan) then FPlan(Self,3,0);
  ActionMode := amAutoPlan;

  If CutMethod=0 then begin
     // Virtualbox-nál a befoglaló keretet töröljük
     Cuv := FCADSource.FCurveList[1];
     Cuv.DeletePoint(R.x1,R.y1);
     Cuv.DeletePoint(R.x1,R.y2);
     Cuv.DeletePoint(R.x2,R.y2);
     Cuv.DeletePoint(R.x2,R.y1);
     SelectAllPolylines;
     DeleteSelectedCurves;
     AutoSortObject(BasePoint);
     SetNearestBeginPoint(BasePoint);
     Eltolas(-Grid.Margin,Grid.Margin);
     h:=MakeCurve('Line',-1,dmPolyline,True,True,False);
     FCurve := FCADSource.FCurveList.Items[h];
     FCurve.AddPoint(Cuv.Points[0]);
     FCurve.AddPoint(BasePoint);
  end;
(*
  If CutMethod=1 then begin
     // A befoglaló téglalap vágódjon utoljára
     // első 4 pontot a végére másoljuk
     DeleteCurve(0);
     DeleteCurve(1);
     Cuv := FCurveList[0];
     Cuv.DeletePoint(R.x1,R.y1);
     Cuv.DeletePoint(R.x1,R.y2);
     Cuv.DeletePoint(R.x2,R.y2);
     Cuv.DeletePoint(R.x2,R.y1);
     GetNearestPoint( BasePoint,CuvIdx,i );
     SetBeginPoint( CuvIdx,i );
     Cuv.AddPoint(Cuv.Points[0]);
     Cuv.AddPoint(R.x1,R.y2);
     Cuv.AddPoint(R.x2,R.y2);
     Cuv.AddPoint(R.x2,R.y1);
     Cuv.AddPoint(R.x1,R.y1);
     Cuv.InsertPoint(0,R.x1,R.y2);
     Cuv.InsertPoint(0,R.x1,R.y2);
     h:=MakeCurve('Line',-1,dmPolyline,True,True,False);
     FCurve := FCurveList.Items[h];
     FCurve.AddPoint(Cuv.Points[0]);
     FCurve.AddPoint(BasePoint);
  end;
*)

  STOP := False;
  VektorisationAll(0.5);

  if CutMethod>1 then Elkerules;

ide:  invalidate;

  SignedNotCutting;
  VektorisationAll(VecCoeff);

  Loading := False;
  AutoUndo := True;
  Changed := True;

  if AutoUndo then UndoSave;
  if Assigned(FPlan) then FPlan(Self,4,0);
  ActionMode := amNone;
  Screen.Cursor := crDefault;
end;

except
  Screen.Cursor := oldCursor;
  ActionMode := amNone;
  Invalidate;
  Loading := False;
  AutoUndo := True;
end;
SelectedVisible := false;
end;

(*
// AutoSTRIP : FŰZÉR KÉPZÉS
// Ha a FileName='', akkor nincs automatikus mentés
procedure TALSablon.AutoSTRIP(FileName: string; BasePoint: TPoint2d);
var i,h,m: integer;
    bm: TPoint2d;
    R:  TRect2d;
    fc,Cuv: TCurve;
    dx,dy: extended;
    fn,path: string;

    procedure VisibleAll;
    var ii: integer;
    begin
         For ii:=0 to Pred(FCurveList.Count) do begin
             FCurve := FCurveList.Items[ii];
             FCurve.Visible:=True;
         end;
    end;

    function GetFileName(fn:string;toldat:string): string;
    var
       fName,path,ext : string;
    begin
     Result := '';
     fName := ExtractFileName(fn);
     if fName<>'' then
     begin
          ext  := ExtractFileExt(fn);
          path := ExtractFilePath(fn);
          if ext <> '' then fName := DelSub(fName,ext)+toldat+ext
          else fName := fName + toldat + '.sbn';
          Result := path + fName;
     end;
    end;

begin
if (FCurveList.Count>0) and (ActionMode <> amAutoPlan) then
Try
  // Alap beállítások
  if Assigned(FPlan) then FPlan(Self,0,0);
  ActionMode := amAutoPlan;
  STOP := False;
  oldCursor := Cursor;
  Screen.Cursor := crHourGlass;
  Loading := True;
  AutoUndo := False;

  // Eredeti rajz mentése
  // Ha nincs olyan könyvtár, akkor létrehozza
  if Filename<>'' then begin
     path := ExtractFilePath(FileName);
     if not DirectoryExists(path) then
        CreateDir(path);
     if DirectoryExists(path) then
        SaveGraphToFile( FileName );
  end;

  // Töröljük az összes nyílt alakzatot
  SignedAll(False);
  VisibleAll;
  SelectAll(False);

  // Polyline-ok törlése
  SelectAllPolylines;
  DeleteSelectedCurves;

         R := GetDrawExtension;
         m := Grid.Margin;
         dx := 2*m-R.x1;
            dy := Paper.y-2*m-R.y2;
         Eltolas(dx,dy);


  // Parent objektumok másolása a virtuál clipboardra
  // Csak a gyerek objektumok maradnak
  SelectParentObjects;
  CutSelectedToVirtClipboard;


  // Gyerek objektumok sorba rendezése
  AutoSortObject(BasePoint);

  if Assigned(FPlan) then FPlan(Self,1,0);


  // Gyerek objektumok felfűzése
  bm := BasePoint;
  h  := Pred(FCurveList.Count);
  for i:=0 to h do
  begin
      fc := TCurve.Create;
      fc.ClearPoints;
      fc.Name := 'Strip'+inttostr(i);
      fc.Closed := False;
      fc.Shape := dmPolyline;
      fc.AddPoint(bm);
      FCurve:=FCurveList.Items[2*i];
      bm := FCurve.GetPoint2d(0);
      fc.AddPoint(bm);
      FCurveList.Insert(2*i,fc);
  end;
      fc := TCurve.Create;
      fc.ClearPoints;
      fc.Name := 'Strip'+inttostr(i);
      fc.Closed := False;
      fc.Shape := dmPolyline;
      fc.AddPoint(bm);
      fc.AddPoint(BasePoint);
      FCurveList.Add(fc);
  invalidate;
  // Fűzér mentése child-ekkel
  if Filename<>'' then begin
     fn := GetFileName(FileName,'_1');
     if fn<>'' then
        SaveGraphToFile( fn );
  end;

  // Polygon-ok/child-ek törlése
  SelectAllPolygons;
  DeleteSelectedCurves;
  if Filename<>'' then begin
     fn := GetFileName(FileName,'_0');
     if fn<>'' then
        SaveGraphToFile( fn );
  end;

  // Fűzér törlése
  FCurveList.Clear;
  invalidate;

  // Parent objektumok visszatöltése, rendezés, felfűzés
  PasteSelectedFromVirtClipboard;
  Selectall(False);
  R := GetDrawExtension;
  AutoCutSequence(BasePoint,True,0);
  if Filename<>'' then
     SaveGraphToFile( GetFileName(FileName,'_2') );

  // A fűzér betöltése
  Clear;
  fn := GetFileName(FileName,'_0');
  if fn<>'' then
     LoadGraphFromFile( fn );

  invalidate;
  Changed := false;
  AutoUndo := True;

except
  AutoUndo := True;
end; // Try
end;
*)

    // A Child objektumot felfűzi a Parent objektumra
    procedure TALSablon.StripObj12(AParent,Achild: integer);
    var j,f,k     : integer;
        pCuv,cCuv : TCurve;
        dMin      : double;
        d         : double;
        pp0,pp,mp : TPoint2d;
        pPointidx,cPointidx: integer;
    begin
       pCuv := FCADSource.FCurveList.Items[AParent];    // Parent obj.
       cCuv := FCADSource.FCurveList.Items[Achild];     // Child obj.
       // Fiók felfűzése
       dMin := 10000000;
       // Legközelebbi pontok keresése
       For j:=0 to Pred(pCuv.Fpoints.Count) do begin
           pp := pCuv.GetPoint2d(j);
           d  := cCuv.GetNearestPoint(pp,k);
           if d < dMin then begin
              pPointidx := j;
              cPointidx := k;
              dMin := d;
           end;
       end;
       pp0 := pCuv.GetPoint2d(pPointidx);
       pp  := cCuv.GetPoint2d(cPointidx);
       // Vizsgáljuk, hogy a szakasz átvágja-e a befűzendő polygon oldalát
       if cCuv.IsCutLine(pp0,pp,cPointidx,mp) then begin
          // Ha metszi, akkor be kell szúrni egy pontot oda
          InsertPoint(Achild,cPointidx,mp);
//          Dec(cPointidx);
       end;

       // Fiók bekezdési pontjának megadása
       SetBeginPoint(Achild,cPointidx);
       // Fiók befűzése
       for f:=0 to Pred(cCuv.Fpoints.Count) do begin
           pp := cCuv.GetPoint2d(f);
           pCuv.InsertPoint(pPointidx+f+1,pp.x,pp.y);
       end;
       pp := cCuv.GetPoint2d(0);
       pCuv.InsertPoint(pPointidx+f+1,pp.x,pp.y);
       pCuv.InsertPoint(pPointidx+f+2,pp0.x,pp0.y);
       // Az eredeti Child törlése
       cCuv.Visible := False;
    end;

(*
// A Child objektumot felfűzi a Parent objektumra
procedure TALSablon.StripObj12(AParent,Achild: integer);
var j,f,k     : integer;
    pCuv,cCuv : TCurve;
    dMin      : double;
    d         : double;
    pp0,pp,mp : TPoint2d;
    pPointidx,cPointidx: integer;
begin
   pCuv := FCurveList.Items[AParent];    // Parent obj.
   cCuv := FCurveList.Items[Achild];     // Child obj.
   // Fiók felfűzése
   dMin := 10000000;
   // Legközelebbi pontok keresése
   For j:=0 to Pred(pCuv.Fpoints.Count) do begin
       pp := pCuv.GetPoint2d(j);
       d  := cCuv.GetNearestPoint(pp,k);
       if d < dMin then begin
          pPointidx := j;
          cPointidx := k;
          dMin := d;
       end;
   end;
   pp0 := pCuv.GetPoint2d(pPointidx);
   pp  := cCuv.GetPoint2d(cPointidx);

   // Vizsgáljuk, hogy a szakasz átvágja-e a befűzendő polygon oldalát
   if cCuv.IsCutLine(pp0,pp,cPointidx,mp) then begin
      // Ha metszi, akkor be kell szúrni egy pontot oda
      InsertPoint(Achild,cPointidx,mp);
   end;

   // Fiók bekezdési pontjának megadása
   SetBeginPoint(Achild,cPointidx);
   cCuv.AbsolutClosed;

   // Fiók befűzése
   for f:=0 to Pred(cCuv.Fpoints.Count) do begin
       pp := cCuv.GetPoint2d(f);
       pCuv.InsertPoint(pPointidx+f+1,pp.x,pp.y);
   end;
   // A parent befűzési pontját újra be kell insertálni
   pCuv.InsertPoint(pPointidx+f+2,pp0.x,pp0.y);

   // Az eredeti Child törlése
   cCuv.Visible := False;

   invalidate;
end;
*)

// A fiók objektumok felfűzése a szülő objektumra
procedure TALSablon.StripChildToParent(AIndex: integer);
var childCount: integer;
    i,k       : integer;
    pCuv,cCuv : TCurve;    // Parent and Child
    dMin      : double;
    parent    : integer;
begin
if (AIndex>-1) and (AIndex<=Pred(FCADSource.FCurveList.Count)) then begin
   childCount := GetInnerObjectsCount(AIndex);
   if childCount=0 then begin
     cCuv := FCADSource.FCurveList.Items[AIndex];
     parent := cCuv.ParentID;
     StripObj12(parent,AIndex);
   end;
end;
end;

// A fiók objektumok felfűzése a szülő objektumokra
procedure TALSablon.StripAll1;
var sCount : integer;

    // Megszámolja a teljes rajzban előforduló gyerek objektumokat
    Function ChildCountAll: integer;
    var p: integer;
    begin
         Result := 0;
         InitParentObjects;
         For p:=0 to Pred(FCADSource.FCurveList.Count) do
             if Curves[p].Visible then
             if (Curves[p].ParentId=-1) then
                Result := Result + GetInnerObjectsCount(p);
    end;


    // Megszámolja a Cuv gyerek objektumait
    Function ChildCount(C: TCurve): integer;
    var p: integer;
    begin
         Result := 0;
         InitParentObjects;
         if c.Visible then
            Result := GetInnerObjectsCount(C);
    end;

    Function Strip: integer;
    var i: integer;
        Cuv : TCurve;
    begin
        Result := 0;
        InitParentObjects;
        For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
            Cuv   := Curves[i];
            if Cuv.Visible and (Cuv.Shape=dmPolygon) then
            if (Cuv.ParentId>-1) and (GetInnerObjectsCount(i)=0) then
            begin
               StripObj12(Cuv.ParentID,i);
               Inc(Result);
               exit;
            end;
        end;
    end;

begin
  Loading := True;
  DeleteInvisibleCurves;
  PontsuritesAll(2);
  InitParentObjects;
  repeat
    sCount:=Strip;
  until sCount=0;
  DeleteInvisibleCurves;
  SelectAll(false);
  Loading := False;
  invalidate;
end;

procedure TALSablon.StripAll;
var i,n: integer;
    Cuv : TCurve;
begin
  Loading := True;
  DeleteInvisibleCurves;
  PoligonizeAll(0);
  PontsuritesAll(2);
  InitParentObjects;
  n := 100;
  While n>0 do begin
        n := 0;
        For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
            Cuv   := FCADSource.FCurveList.Items[i];
            if Cuv.Visible and (Cuv.Shape=dmPolygon) then
            if not IsParent(i) and (GetInnerObjectsCount(i)=0) then begin
               StripChildToParent(i);
               Inc(n);
               Break;
            end;
        end;
        DeleteInvisibleCurves;
  end;
  Loading := False;
  invalidate;
end;


// A zárt poligonok ParentID-jét beállítja, ha van szülője
procedure TALSablon.InitParentObjects;
var i: integer;
begin
  for i:=0 to Pred(FCADSource.FCurveList.Count) do begin
      FCurve := Curves[i];
      FCurve.ParentID := -1;
      if FCurve.Closed and FCurve.Visible then
            FCurve.ParentID := GetRealParentObject(i);
  end;
end;

// Megvizsgálja, hogy az objektumnak van-e szüleje, azaz
// olyan objektum, aminek a belselyében található.
// Ha van, akkor Result=False ,
// ha nincs, akkor: True;
function TALSablon.IsParent(AIndex: Integer): boolean;
begin
  Result := False;
  if AIndex>-1 then
     Result := GetParentObject(AIndex)=-1;
end;

// Megvizsgálja, hogy a pont körüli objektumnak van-e szüleje, azaz
// olyan objektum, aminek a belselyében található
// True = ha szülő objektum
function TALSablon.IsParent(x,y: TFloat): boolean;
begin
  Result := False;
  Result := GetParentObject(x,y)=-1;
end;

function TALSablon.GetInnerObjectsCount(AIndex: Integer): integer;
// Result = 0 or inner objects' count;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    i       : integer;
begin
  Result := 0;
  FCurve := FCADSource.FCurveList.Items[AIndex];
  OurRect:= FCurve.Boundsrect;
  For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
    if i<>AIndex then begin
      FCurve := FCADSource.FCurveList.Items[I];
      if FCurve.Visible and (FCurve.Shape=dmPolygon) then begin
         inRect := FCurve.Boundsrect;
         If RectInRect2D(OurRect,inRect) then Inc(Result);
      end;
    end;
  end;
end;

function TALSablon.GetInnerObjectsCount(Cuv: TCurve): integer;
// Result = 0 or inner objects' count;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    i       : integer;
begin
  Result := 0;
  OurRect:= Cuv.Boundsrect;
  For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
      FCurve := FCADSource.FCurveList.Items[I];
      if FCurve.Visible and (FCurve<>Cuv) then begin
         inRect := FCurve.Boundsrect;
         If RectInRect2D(OurRect,inRect) then
         if Cuv.IsInCurve(FCurve.Points[0],fSensitiveRadius/fzoom)=icIn then begin
            Inc(Result);
         end;
      end;
  end;
end;

// Megkeresi, hogy a sokszög melyik legkisebbnek alakzat belselyében van
// Ha nincs befoglalója, akkor Result=-1
// Ha van, akkor annak az ID-jével tér vissza
function TALSablon.GetParentObject(AIndex: Integer): integer;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    oRect   : TRect2d;
    Cuv     : TCurve;
    p,p1,p2 : TPoint2d;
    i,j     : integer;
    maxy    : double;
begin

  Result := -1;
  FCurve := FCADSource.FCurveList.Items[AIndex];
  p := FCurve.GetPoint2d(0);
  maxy := minReal;
  // Megkeresem a max. y pontot
  for i:=0 to Pred(FCurve.Count) do begin
      p1 := FCurve.Points[i];
      if p1.y>maxy then begin
         maxy := p1.y;
         p := p1;
      end;
  end;
  Result := GetParentObject(p.x,p.y);
  FCurve.ParentID := Result;
end;

// Megkeresi a pont körüli legkisebb befoglaló objektumot
// Result = -1, vagy a talált befoglaló objektum ID-je
function TALSablon.GetParentObject(x,y: TFloat): integer;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    oRect   : TRect2d;
    i,j     : integer;
    Cuv     : TCurve;
    dist    : double;
    p,p1,p2,o : TPoint2d;
begin
  Result := -1;
  dist   := maxReal;
  For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
      Cuv := FCADSource.FCurveList.Items[I];
      if Cuv.Visible then begin
         OurRect:= Cuv.Boundsrect;
         if PontInKep(x,y,OurRect) then
         for j:=0 to Cuv.Count-2 do begin
             p1:=Cuv.Points[j];
             p2:=Cuv.Points[j+1];
             if Kozben(p1.x,p2.x,x,0) and (p2.x<>p1.x)then begin
                   o := Osztopont(p1,p2,(x-p1.x)/(p2.x-p1.x));
                   if (o.y>y) and (Abs(o.y-y)<dist) then begin
                      dist := Abs(o.y-y);
                      Result := i;
                   end;
             end;
         end;
      end;
  end;
end;

function TALSablon.GetRealParentObject(AIndex: Integer): integer;
Var CuvParent : TCurve;
    Cuv     : TCurve;
    pIdx    : integer;
    i,j     : integer;
begin
  Result := -1;
  Cuv := Curves[AIndex];
  for I := 0 to Pred(FCADSource.FCurveList.Count) do
    if i<>AIndex then begin
       CuvParent := Curves[i];
       if CuvParent.IsInCurve(Cuv.Points[0],fSensitiveRadius/fzoom)=icIn then
       begin
          Result := i;
          Exit;
       end;
    end;
end;
(*
function TALSablon.ObjectContour(Cuv: TCurve; OutCode: double): TCurve;
begin

end;

function TALSablon.GetContour(Cuv: TCurve; OutCode: double): TCurve;
begin

end;

procedure TALSablon.SetContour(AIndex: Integer; Radius: double);
begin

end;

procedure TALSablon.SetAllContour;
begin

end;
*)

//   ContourOptimalizalas
//   ----------------------------------------------------------------------------
//   Lényege: A kontúron haladva növekvő indexek szerint, minden esetben megvizsgáljuk,
//   hogy a végponttól visszafelé haladva melyik az az első kontúrpont, melyre
//   közvetlen rálátás van. Nyilván, a közbülső pontok törölhetők.
//
procedure TALSablon.ContourOptimalizalas(Cuv: TCurve);
    Type mpRec = record
         idx : integer;
         d   : double;
         end;
    var kezdP,vegP     : TPoint2d;
        ii,jj,kk,nn,n  : integer;
        CR             : double;
        PointsArray    : array of TPoint2d;
        cCuv           : TCurve;
        mpArr          : array of mpRec;

    // megszámolja, hogy az AB szakasz, hány polygont metsz
    // és feltölti az mpArr tömböt az A ponttól való távolság sorrendjében

    function IsCutPolygons(A,B: TPoint2d): integer;
    var i: integer;
        fc: TCurve;
        pCount : integer;
        dd: double;
        pr: mpRec;
        csere: boolean;
        R: TRect2d;
    begin
         pCount := 0;
         SetLength(mpArr,1000);
         For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
             if Curves[i].Shape=dmPolygon then begin
                fc := Curves[i].Contour;
                   if (fc.IsCutLine(A,B,dd)) then begin
                      mpArr[pCount].idx := i;
                      mpArr[pCount].d   := dd;
                      Inc(pCount);
                   end;
             end;
         end;
         // Tömb rendezése távolság szerint növekvően
         mpArr := Copy(mpArr,0,pCount);
         Result := pCount;
         i:=0; csere:=True;
         While csere do begin
            csere := False;
            for i:=0 to pCount-2 do begin
                if mpArr[i].d > mpArr[i+1].d then begin
                   pr := mpArr[i]; mpArr[i]:=mpArr[i+1]; mpArr[i+1]:=pr;
                   csere := True;
                end;
            end;
         end;
    end;

    begin
      STOP := False;
      CR := ContourRadius;
      ContourRadius := ContourRadius-0.3;
      SetAllContour;
      Cuv.SelectAllPoints(False);

      nn := Pred(Cuv.Count);
      ii := 0;

      if Cuv.Count>2 then
      While ii<=(nn) do begin
            kezdP := Cuv.Points[ii];
            // Keressük a legtávolabbi, közvetlen rálátási pontot
            for jj:=nn downto (ii+1) do begin
                vegP := Cuv.Points[jj];
                if (IsCutPolygons(kezdP,vegP)=0) then
                   Break;
                Application.ProcessMessages;
                if STOP then Break;
            end;

            if (jj-ii>1) then
            begin
                for kk := ii+1 to jj-1 do           // törli a közbülső pontokat
                    Cuv.SelectPoint(kk,true);
                ii := jj+1;
            end else
                Inc(ii);

            Application.ProcessMessages;
            if STOP then Break;
      end;
      Cuv.DeleteSelectedPoints;
(*
      // Urolsó két pont szakaszának elkerülése
      ii  := Pred(Cuv.Count);
      if (IsCutPolygons(Cuv.Points[ii-1],Cuv.Points[ii])<>0) then
      Try
        cCuv  := TCurve.Create;
        cCuv.Shape := dmLine;
        cCuv.Closed := False;
        cCuv.AddPoint(Cuv.Points[ii-1]);
        cCuv.AddPoint(Cuv.Points[ii]);
        ElkerulesAB( cCuv );
      finally
        Cuv.DeletePoint(Pred(Cuv.Count));
        Cuv.DeletePoint(Pred(Cuv.Count));
        for jj:=0 downto Pred(cCuv.Count) do
            Cuv.AddPoint(cCuv.Points[jj]);
        cCuv.Free;
      end;
*)
      SetLength(mpArr,0);
      ContourRadius := CR;
      SetAllContour;

end;  // End ContourOptimalizalas

(*
procedure TALSablon.ContourOptimalizalas(Cuv: TCurve);
    Type mpRec = record
         idx : integer;
         d   : double;
         end;
    var kezdP,vegP     : TPoint2d;
        ii,jj,kk,nn,n  : integer;
        CR             : double;
        PointsArray    : array of TPoint2d;
        cCuv           : TCurve;
        mpArr          : array of mpRec;

    // megszámolja, hogy az AB szakasz, hány polygont metsz
    // és feltölti az mpArr tömböt az A ponttól való távolság sorrendjében

    function IsCutPolygons(A,B: TPoint2d): integer;
    var i: integer;
        fc: TCurve;
        pCount : integer;
        dd: double;
        pr: mpRec;
        csere: boolean;
        R: TRect2d;
    begin
         pCount := 0;
         SetLength(mpArr,1000);
         For i:=0 to Pred(FCurveList.Count) do begin
             if Curves[i].Shape=dmPolygon then begin
                fc := Curves[i].Contour;
                   if (fc.IsCutLine(A,B,dd)) then begin
                      mpArr[pCount].idx := i;
                      mpArr[pCount].d   := dd;
                      Inc(pCount);
                   end;
             end;
         end;
         // Tömb rendezése távolság szerint növekvően
         mpArr := Copy(mpArr,0,pCount);
         Result := pCount;
         i:=0; csere:=True;
         While csere do begin
            csere := False;
            for i:=0 to pCount-2 do begin
                if mpArr[i].d > mpArr[i+1].d then begin
                   pr := mpArr[i]; mpArr[i]:=mpArr[i+1]; mpArr[i+1]:=pr;
                   csere := True;
                end;
            end;
         end;
    end;

    begin
      STOP := False;
      CR := ContourRadius;
      ContourRadius := ContourRadius-0.3;
      SetAllContour;
      Cuv.SelectAllPoints(False);

      nn := Pred(Cuv.Count);
      ii := 0;

      if Cuv.Count>2 then
      While ii<=(nn) do begin
            kezdP := Cuv.Points[ii];
            // Keressük a legtávolabbi, közvetlen rálátási pontot
            for jj:=nn downto (ii+1) do begin
                vegP := Cuv.Points[jj];
                if (IsCutPolygons(kezdP,vegP)=0) then
                   Break;
                Application.ProcessMessages;
                if STOP then Break;
            end;

            if (jj-ii>1) then
            begin
                for kk := ii+1 to jj-1 do           // törli a közbülső pontokat
                    Cuv.SelectPoint(kk,true);
                ii := jj+1;
            end else
                Inc(ii);

            Application.ProcessMessages;
            if STOP then Break;
      end;
//      Cuv.DeleteSelectedPoints;
(*
      // Urolsó két pont szakaszának elkerülése
      ii  := Pred(Cuv.Count);
      if (IsCutPolygons(Cuv.Points[ii-1],Cuv.Points[ii])<>0) then
      Try
        cCuv  := TCurve.Create;
        cCuv.Shape := dmLine;
        cCuv.Closed := False;
        cCuv.AddPoint(Cuv.Points[ii-1]);
        cCuv.AddPoint(Cuv.Points[ii]);
        ElkerulesAB( cCuv );
      finally
        Cuv.DeletePoint(Pred(Cuv.Count));
        Cuv.DeletePoint(Pred(Cuv.Count));
        for jj:=0 downto Pred(cCuv.Count) do
            Cuv.AddPoint(cCuv.Points[jj]);
        cCuv.Free;
      end;

      SetLength(mpArr,0);
      ContourRadius := CR;
      SetAllContour;

end;  // End ContourOptimalizalas
*Ö

//=======================  Elkerules  =====================================


    (*
      ELKERÜLÉSI RUTIN A pontból B pontba
    *)
    //============================================================================
    function TALSablon.ElkerulesAB(var eCurve: TCurve): boolean;
    Type
      { Polygon metszések vizsgálatához}
      TmpRec = record
           Cuvidx   : integer;   // Polygon sorszáma
           Pointidx : integer;   // legközelebbi pontjának sorszáma
           d        : double;    // Távolsága
      end;
    Var mpArr : array of TmpRec; // Metszett polygonok tömbje
        i,j,k       : integer;
        mpRec       : TmpRec;        // Legközelebbi polygon és pont + d távolság
        BaseCurve   : TCurve;     // Elkerülő polyline
        TempCurve   : TCurve;     // Kontúr
        Cuv         : TCurve;     // Legközelebbi polygon
        BePont,KiPont : TInOutRec; // Be-ki lépési pontok a kontúron
        mpCount     : integer;    // Kontúr-szakasz metszéspontok száma
        AP,BP       : TPoint2d;   // Szakasz eleje, vége
        KonturHossz : double;     // Kontúr kerülete
        KonturSzelet: double;     // Egy szeletének hossza
        p           : TPoint2d;
        nCikl       : integer;    // Számláló a próbálkozásokhoz


        // Az A ponthoz legközelebbi polygon legközelebbi pontját adja
        function GetNearest(A: TPoint2d): TmpRec;
        var jj: integer;
            Idx: integer;
            dd1,dd: double;
            x,y: double;
            fCuv: TCurve;
        begin
        dd1 := 10e+10;
        For jj:=0 to Pred(FCADSource.FCurveList.Count) do
        begin
            fCuv:=FCADSource.FCurveList.Items[jj];    // = a polygon lista egyik eleme
            if fCuv.Shape = dmPolygon then
            begin
               dd:=fCuv.GetNearestPoint(A,Idx);
               if dd<dd1 then begin
                  dd1 := dd;
                  Result.Cuvidx := jj;
                  Result.Pointidx := Idx;
                  Result.d := dd;
               end;
            end;
        end;
        end;

        // megszámolja, hogy az AB szakasz, hány polygont metsz
        // és feltölti az mpArr tömböt az A ponttól való távolság sorrendjében
        function IsCutPolygons(A,B: TPoint2d): integer;
        var ii: integer;
            fc: TCurve;
            pCount : integer;
            dd: double;
            pr: TmpRec;
            csere: boolean;
        begin
             pCount := 0;
             SetLength(mpArr,1000);
             For ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
                 fc:=FCADSource.FCurveList.Items[ii];
                 if fc.Shape=dmPolygon then
                    if fc.IsCutLine(A,B,dd) then begin
                       mpArr[pCount].Cuvidx := ii;
                       mpArr[pCount].d   := dd;
                       Inc(pCount);
                    end;
             end;
             mpArr := Copy(mpArr,0,pCount);
             Result := pCount;
             // Tömb rendezése távolság szerint növekvően
             ii:=0; csere:=True;
             While csere do begin
                csere := False;
                for ii:=0 to pCount-2 do begin
                    if mpArr[ii].d > mpArr[ii+1].d then begin
                       pr := mpArr[ii]; mpArr[ii]:=mpArr[ii+1]; mpArr[ii+1]:=pr;
                       csere := True;
                    end;
                end;
             end;
        end;

    begin
      Result := True;
      nCikl := 0;

      k := Pred(eCurve.Count);
      AP := eCurve.GetPoint2d(k-1);
      BP := eCurve.GetPoint2d(k);

      // Megkeresem az AP ponthoz legközelebbi polygon legközelebbi pontját
      mpRec := GetNearest(AP);
      Cuv := Curves[mpRec.Cuvidx];
      if mpRec.d<ContourRadius then begin
         // Ha egy polygonhoz túl közel van, akkor kontúron kell haladni a
         // AB húr metszéspontjáig
         TempCurve := Cuv.Contour;
         mpCount := ConturInOut(TempCurve,AP,BP,BePont,KiPont);
         if mpCount=1 then
            TempCurve.InsertPoint(KiPont.Idx,KiPont.mPont.x,KiPont.mPont.y);
         // A kontúr legközelebbi pontjára lépek
         TempCurve.GetNearestPoint(AP,j);
         if mpCount=0 then
            TempCurve.GetNearestPoint(BP,k);
                  For i:=j to k do begin
                      p := TempCurve.GetPoint2d(i);
                      eCurve.InsertPoint(Pred(eCurve.Count),p.x,p.y);
                  end;
         AP := p;
      end;

      Repaint;

      while IsCutPolygons(AP,BP)>0 do begin
            Cuv:=FCADSource.FCurveList.Items[mpArr[0].Cuvidx];   // = az átmetszett poligon
            TempCurve := Cuv.Contour;
            // Belépési és kilépési pontok keresése a kontúron
            mpCount := ConturInOut(TempCurve,AP,BP,BePont,KiPont);
            // Ha nincs metszéspont, akkor az egyenes elkerüli a polygonokat;
            // Ha 1 van, akkor be, vagy ki lép a poligonból
            // Ha 2 vagy több van, akkor átmetszi.

            if mpCount=1 then begin
               // Belépés a kontúrba a végpont felé

            end
            else

            if mpCount>1 then begin
               if KiPont.Idx>BePont.Idx then begin
                  TempCurve.InsertPoint(KiPont.Idx,KiPont.mPont.x,KiPont.mPont.y);
                  TempCurve.InsertPoint(BePont.Idx,BePont.mPont.x,BePont.mPont.y);
                  Inc(KiPont.Idx);
               end else begin
                  TempCurve.InsertPoint(BePont.Idx,BePont.mPont.x,BePont.mPont.y);
                  TempCurve.InsertPoint(KiPont.Idx,KiPont.mPont.x,KiPont.mPont.y);
                  Inc(BePont.Idx);
               end;

               KonturHossz  := TempCurve.GetKerulet;  // Teljes kontúr hossz
               KonturSzelet := TempCurve.GetKeruletSzakasz(BePont.Idx,KiPont.Idx);
               // Minimális kontúrszakasz keresés
               if KonturSzelet < (KonturHossz-KonturSzelet) then begin

                  if KiPont.Idx>BePont.Idx then
                  For i:=BePont.Idx to KiPont.Idx do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end
                  else begin
                  For i:=BePont.Idx to Pred(TempCurve.Count) do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end;
                  For i:=0 to KiPont.Idx do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end;
                  end;

               end
               else begin

                  if BePont.Idx>KiPont.Idx then
                  For i:=BePont.Idx downto KiPont.Idx do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end
                  else begin
                  For i:=BePont.Idx downto 0 do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end;
                  For i:=Pred(TempCurve.Count) downto KiPont.Idx do begin
                      p := TempCurve.GetPoint2d(i);
                      k := Pred(eCurve.Count);
                      eCurve.InsertPoint(k,p.x,p.y);
                  end;
                  end;

               end;
            end;
            Repaint;
            AP := p;

            Application.ProcessMessages;
            if STOP then begin
               STOP:=False;
               Exit;
            end;

            inc( nCikl );
            if nCikl>100 then begin
               Result := False;
               exit;
            end;

      end;

    //        ContourOptimalizalas( eCurve );
    end;

    //=======================  Elkerules  =====================================

    procedure TALSablon.Elkerules;
    // Sorra veszem a nyílt objektumokat és kontúr optimalizálással
    // megkeresem az optimális útvonalat
    var i,j    : integer;
        CR     : double;
        cuv    : TCurve;
    begin
    Try
      if Assigned(FPlan) then FPlan(Self,1,0);
      CR := ContourRadius;
      ContourRadius := ContourRadius*0.8;
      SetAllContour;
      for i := 0 to Pred(FCADSource.FCurvelist.Count) do
      begin
        if not Curves[i].Closed then
        begin
           Selected := Curves[i];
           SelectedIndex := i;
           if Curves[i].Count>2 then
           for j := Pred(Curves[i].Count)-1 downto 1 do
               Curves[i].DeletePoint(j);
           if Assigned(FPlan) then FPlan(Self,1,Trunc(100*i/Pred(FCADSource.FCurvelist.Count)));
           cuv := Curves[i];
           ElkerulesAB(cuv);
           ContourOptimalizalas(Cuv);
                Application.ProcessMessages;
                if STOP then Break;
        end;
      end;
    Finally
      ContourRadius := CR;
      SetAllContour;
    End;
    end;

    procedure TALSablon.Elkerules1;
    Type mpRec = record
           idx : integer;
           d   : double;
         end;

    var i,j,k: integer;
        BaseCurve,Cuv,TempCurve: TCurve;
        p1,p2: TPoint2d;          // p1-p2 szakasz
        p11,p12: TPoint2d;        // metszett poligon két szélső pontja
        idx1,idx2: integer;       // metszett poligon szélső pontjainak indexe
        fi,fi1,fi2: double;       // A pontból húzott poligon határolók irányszöge
        sz1,sz2: double;
        d1,d2: double;
        efg: TEgyenesFgv;
        Cutting: integer;
        mpArr : array of mpRec;
        cuvIDX,pIdx: integer;
        ContP: TPoint2d;          // kontúr pont
        n: integer;
        metszes: boolean;
        szaz: integer;
        ciklusCounter: integer;
        CR: double;

    label ujra;

        // megszámolja, hogy az AB szakasz, hány polygont metsz
        // és feltölti az mpArr tömböt az A ponttól való távolság sorrendjében

        function IsCutPolygons(A,B: TPoint2d): integer;
        var ii: integer;
            fc: TCurve;
            pCount : integer;
            dd: double;
            pr: mpRec;
            csere: boolean;
            contCuv : TCurve;
            R: TRect2d;
        begin
             pCount := 0;
             SetLength(mpArr,1000);
             For ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
                 fc:=FCADSource.FCurveList.Items[ii];
                 if fc.Shape=dmPolygon then begin
                    R := fc.BoundsRect;
                    R := Rect2d(R.X1-delta,R.y1-delta,R.x2+delta,R.y2+delta);
                    if IsSzakaszNegyszogMetszes(A,B,R) then begin
                       if fc.Contour=nil then begin
                          fc.SetContour(ContourRadius);
                          Vektorisation(0.1,fc.Contour);
                       end;
                       if (fc.Contour.IsCutLine(A,B,dd)) then begin
                          mpArr[pCount].idx := ii;
                          mpArr[pCount].d   := dd;
                          Inc(pCount);
                       end;
                    end;
                 end;
             end;
             mpArr := Copy(mpArr,0,pCount);
             Result := pCount;
             // Tömb rendezése távolság szerint növekvően
             ii:=0; csere:=True;
             While csere do begin
                csere := False;
                for ii:=0 to pCount-2 do begin
                    if mpArr[ii].d > mpArr[ii+1].d then begin
                       pr := mpArr[ii]; mpArr[ii]:=mpArr[ii+1]; mpArr[ii+1]:=pr;
                       csere := True;
                    end;
                end;
             end;
        end;


        function GetNearPoint(cc: TCurve; A: TPoint2d): integer;
        var jj: integer;
            dd1,dd: double;
            x,y: double;
        begin
        Result := -1;
        if cc<>nil then begin
        dd1 := 10e+10;
        For jj:=0 to Pred(cc.FPoints.Count) do
        begin
            cc.GetPoint(jj,x,y);
            if IsCutPolygons(A,Point2d(x,y))=0 then begin
            dd:=KetPontTavolsaga(A.x,A.y,x,y);
            if dd<dd1 then begin
               dd1 := dd;
               Result   := jj;
            end;
            end;
        end;
        end;
        end;

    begin
      STOP := False;
      SetAllContour;
      SignedNotCutting;
      invalidate;
      n:=0;
      // Veszem a polyline-okat és metszést vizsgálok polygon-okkal
      For i:=0 to Pred(FCADSource.FCurveList.Count) do begin
          BaseCurve:=FCADSource.FCurveList.Items[i];    // = a polyline
          Selected := BaseCurve;
          szaz := Trunc(100*(i/FCADSource.FCurveList.Count));
          if Assigned(FPlan) then FPlan(Self,3,szaz);

    ujra: if BaseCurve.Shape=dmPolyline then
          begin

             // Veszem a polyline 2 utolsó pontját
             k:=Pred(BaseCurve.Fpoints.Count);
             p1:=BaseCurve.GetPoint2d(k-1);
             p2:=BaseCurve.GetPoint2d(k);

             // ennyi db poligont vág át : mpArr tömb tartalmazza a vágott poligonokat
             Cutting:=IsCutPolygons(p1,p2);


          While Cutting>0 do begin
                k:=Pred(BaseCurve.Fpoints.Count);
                p1:=BaseCurve.GetPoint2d(k-1);
                p2:=BaseCurve.GetPoint2d(k);
                Application.ProcessMessages;
                if STOP then
                   Break;

                 Cuv:=FCADSource.FCurveList.Items[mpArr[0].idx];   // = az átmetszett poligon
                 if Cuv.Shape=dmPolygon then begin

                       TempCurve := Cuv.Contour;
                       Vektorisation(0.1,TempCurve);
    //                   ContourOptimalizalas(BaseCurve);

                    // Megkeressük a kontúr A ponthoz legközelebbi pontját
                    pIdx := GetNearPoint(TempCurve,p1);
                    p1 := TempCurve.GetPoint2d(pIdx);
                    // Addig haladunk a kontúron míg a poligont metszi a maradék szakasz
                    metszes := Cuv.IsCutLine(p1,p2);
                    if metszes then begin
                       ciklusCounter:=0;
                    While Cuv.IsCutLine(p1,p2) or TempCurve.IsCutLine(p1,p2) do begin
                          k:=Pred(BaseCurve.Fpoints.Count);  // Cél pont
                          p2:=BaseCurve.GetPoint2d(k);
                          BaseCurve.InsertPoint(k,p1.x,p1.y);
                          Inc(pIdx);
                          if pIdx>Pred(TempCurve.Count) then pIdx:=0;
                          p1 := TempCurve.GetPoint2d(pIdx);
                          Application.ProcessMessages;
                          Inc(ciklusCounter);
                          if STOP or (ciklusCounter>10000) then
                             Break;
                    end;
                          k:=Pred(BaseCurve.Fpoints.Count);  // Cél pont
                          p1 := TempCurve.GetPoint2d(pIdx);
                          BaseCurve.InsertPoint(k,p1.x,p1.y);
                          Application.ProcessMessages;
                          if STOP then Break;
                    end
                    else begin
                          k:=Pred(BaseCurve.Fpoints.Count);  // Cél pont
                          p1 := TempCurve.GetPoint2d(pIdx);
                          BaseCurve.InsertPoint(k,p1.x,p1.y);
                          Break;
                    end;
                 end;
                // Veszem a polyline 2 utolsó pontját
                k:=Pred(BaseCurve.Fpoints.Count);
                p1:=BaseCurve.GetPoint2d(k-1);
                p2:=BaseCurve.GetPoint2d(k);
                Cutting:=IsCutPolygons(p1,p2);
                if STOP then Break;
                if (ciklusCounter>10000) then
                   Cutting:=0;
             end;

             ContourOptimalizalas(BaseCurve);
             Application.ProcessMessages;
             if STOP then
                Break;
          end;
      end;
       SetLength(mpArr,0);
       SignedNotCutting;
       invalidate;
       if Assigned(FPlan) then FPlan(Self,3,0);
    end;

    procedure TALSablon.DrawWorkPoint(x, y: double);
    var
      I,J: Integer;
    begin
      WorkPosition.WorkPoint.x:=x;
      WorkPosition.WorkPoint.y:=y;
      With {DrawBmp.}Canvas do
      begin
           I:=XToS(x);
           J:=YToS(y);
           Pen.Color:=clRed;
           Pen.Mode:=pmCopy;
           Brush.Style := bsSolid;
           Brush.Color:=clRed;
           Ellipse(I-4,J-4,I+4,J+4);
      end;
    end;

    procedure TALSablon.ClearWorkPoint;
    begin

    end;

    procedure TALSablon.WorkpositionToCentrum;
    begin
      MoveCentrum(WorkPosition.WorkPoint.x,WorkPosition.WorkPoint.y);
    end;

    procedure TALSablon.TestVekOut(dx, dy: extended);
    var i,lepesszam: integer;
        d,xr,yr,s,c,lepeskoz : double;
        x,y,alfa : double;
        kesleltetes: double;
        okesleltetes,correction: double;
        DestPosition : TPoint3D;
        R  : TRect;

        procedure Delay;
        var i: integer;
        begin
          i:=0;
          While i<1000000 do Inc(i);
        end;

    begin
      DestPosition.x := WorkPosition.WorkPoint.x+dx;
      DestPosition.y := WorkPosition.WorkPoint.y+dy;

      d := sqrt((dx*dx)+(dy*dy));

    If d>MMPerLepes then begin
      lepeskoz    := MMPerLepes;
      Kesleltetes := 1;
      lepesszam   := Round(d/lepeskoz);

      alfa := SzakaszSzog(0,0,dx,dy);
      xr := 0;
      yr := 0;
      s := lepeskoz*sin(alfa); c := lepeskoz*cos(alfa);

      okesleltetes:=Kesleltetes;

        For i:=1 to lepesszam do begin
          x:=xr/MMPerLepes; y:=yr/MMPerLepes;
          xr := xr+c;
          yr := yr+s;
          If Round(x)<>Round(xr/MMPerLepes) then begin
             If dx>0 then WorkPosition.WorkPoint.x:=WorkPosition.WorkPoint.x+MMPerLepes;
             If dx<0 then WorkPosition.WorkPoint.x:=WorkPosition.WorkPoint.x-MMPerLepes;
          end;
          If Round(y)<>Round(yr/MMPerLepes) then begin
             If dy>0 then WorkPosition.WorkPoint.y:=WorkPosition.WorkPoint.y+MMPerLepes;
             If dy<0 then WorkPosition.WorkPoint.y:=WorkPosition.WorkPoint.y-MMPerLepes;
          end;

          if fSablonSzinkron then begin
             MoveCentrum(WorkPosition.WorkPoint.x,WorkPosition.WorkPoint.y);
             repaint;
          end
          else begin
              Canvas.LineTo(XToS(WorkPosition.WorkPoint.x),YToS(WorkPosition.WorkPoint.y));
          end;
          Application.ProcessMessages;
          Delay;

          {$ifdef WINDOWS}
          if (lo(GetAsyncKeyState(VK_ESCAPE)) > 0)
          then begin
               STOP := True;
          end;
          if (lo(GetAsyncKeyState(VK_SPACE)) > 0)
          then begin
               fSablonSzinkron := not fSablonSzinkron;
          end;
          {$endif}

          if STOP then begin
             Kesleltetes:=okesleltetes;
             exit;
          end;
      end;
    end;
      Kesleltetes:=okesleltetes;
    end;

{Megmunkálás az Aobject AItem sorszámú pontjától}
procedure TALSablon.TestWorking(AObject,AItem:integer);
var Cuv : TCurve;
    i,j,j0 : integer;
    x,y : double;
    elso: boolean;
    p : TPoint2d;
    oc : boolean;
begin
  Try
    if Assigned(FPlan) then FPlan(Self,1,0);
    STOP := False;
    EnablePaint := False;
    elso := True;
    ShowPoints := False;
    oc := CentralCross;
    WorkPosition.CuvNumber := AObject;
    WorkPosition.PointNumber := AItem;
    Paint;
    For i:=AObject to FCADSource.FCurveList.Count-1 do begin
        if Assigned(FPlan) then FPlan(Self,1,Trunc(100*i/Pred(FCADSource.FCurvelist.Count)));
        Application.ProcessMessages;
        if STOP then Break;
        Cuv := FCADSource.FCurveList.Items[i];
        if Ord(Cuv.Shape)>5 then
           Poligonize(FCADSource.FCurveList.Items[i],0);
        If Cuv.Enabled then begin
        WorkPosition.CuvNumber := i;

        If elso then begin
           j0:=AItem; elso:=False;
           Canvas.Pen.Color := clRed;
           Canvas.Pen.Style := psSolid;
           Canvas.Pen.Mode  := pmCopy;
           Canvas.Pen.width := 4;
           Canvas.MoveTo(XToS(WorkPosition.WorkPoint.x),YToS(WorkPosition.WorkPoint.y));
        end else j0 := 0;


        For j:=j0 to Cuv.FPoints.Count-1 do begin
            Cuv.GetPoint(j,x,y);
            WorkPosition.PointNumber := j;
            TestVekOut(x-WorkPosition.WorkPoint.x,y-WorkPosition.WorkPoint.y);
            Application.ProcessMessages;
            if STOP then Break;
        end;

        if STOP then
           Break;
          {$ifdef WINDOWS}
          if (lo(GetAsyncKeyState(VK_ESCAPE)) > 0)
          then begin
               STOP := True;
               Break;
          end;
          if (lo(GetAsyncKeyState(VK_SPACE)) > 0)
          then begin
               fSablonSzinkron := not fSablonSzinkron;
          end;
          {$endif}
        // Closed curve back to 0. point if points count>2
        If Cuv.Closed and (Cuv.FPoints.Count>2) then begin
            Cuv.GetPoint(0,x,y);
            WorkPosition.PointNumber := j;
            TestVekOut(x-WorkPosition.WorkPoint.x,y-WorkPosition.WorkPoint.y);
            if STOP then Break;
        end;
        end;
    end
    finally
      if Self<>nil then begin
         EnablePaint := True;
         CentralCross := oc;
         if Assigned(FPlan) then FPlan(Self,1,0);
      end;
    end;
end;

{ TSelectedArea }

constructor TSelectedArea.Create;
begin
  FCurve     := TCurve.Create;
  oCurve     := TCurve.Create;
  dCurve     := TCurve.Create;
  OrigList   := TList.Create;
  DestList   := TList.Create;
  fPen       := TPen.Create;
  fPen.Color := clBlue;
  fPen.Width := 2;
  fPen.Mode  := pmCopy;
  fVisible   := False;
//  fAllwaysDraw := False;
//  FFrameType := satFlex;
  Init;
end;

destructor TSelectedArea.Destroy;
var
  I: Integer;
begin
  for I:=Pred(OrigList.Count) downto 0 do
  begin
    FCurve:=OrigList.Items[I];
    FCurve.Free;
    FCurve:=DestList.Items[I];
    FCurve.Free;
  end;
  OrigList.Free;
  DestList.Free;
  fPen.Free;
  inherited;
end;

// Return the bounding rect of quadrilateral
{ Bounding rect: left-bottom and right-top corners }
function TSelectedArea.GetBoundsRect: TRect2d;
var
  I: Integer;
  x1,y1,x2,y2: TFloat;
  P: TPoint2d;
  rAngle: double;
begin
(*
if Ortho then begin
   rAngle := RotAngle;
   RotAngle := 0;
end;*)
  x1:=1E+10;
  y1:=1E+10;
  x2:=-1E+10;
  y2:=-1E+10;
  for I:=0 to 3 do begin
      P:=Nodes[i];
      if P.x<x1 then x1:=P.x;
      if P.x>x2 then x2:=P.x;
      if P.y<y1 then y1:=P.y;
      if P.y>y2 then y2:=P.y;
  end;
      Result := Rect2d(x1,y1,x2,y2);
//if Ortho then RotAngle := rAngle;
end;

procedure TSelectedArea.SetBoundsRect(const Value: TRect2d);
var B,R: TRect2d;
    cx,cy: double;
    I: Integer;
begin
  B := BoundsRect;
  R := CorrectRealRect(Value);
  for I := 0 to 3 do begin
      cx := ( Nodes[i].x - B.x1 )/( B.x2 - B.x1 );
      cy := ( Nodes[i].y - B.y1 )/( B.y2 - B.y1 );
      Nodes[i].X := R.x1 + cx*(R.x2 - R.x1);
      Nodes[i].Y := R.y1 + cy*(R.Y2 - R.y1);
  end;
  recalc;
end;

function TSelectedArea.GetHeight: double;
var R: TRect2d;
begin
(*
  case FrameType of
  satFix:
    begin

    end;
  satMagnify:
    begin

    end;
  satFlex :
    begin
       R := BoundsRect;
       Result := R.y2-R.y1;
    end;
  end;*)
  R := BoundsRect;
  Result := R.y2-R.y1;
  FHeight := Result;
end;

procedure TSelectedArea.SetHeight(const Value: double);
begin
if FHeight<>Value then begin
   FHeight := Value;
   Recalc;
end;
end;

function TSelectedArea.GetWidth: double;
var R: TRect2d;
begin
(*  case FrameType of
  satFix:
    begin

    end;
  satMagnify:
    begin

    end;
  satFlex :
    begin
       R := BoundsRect;
       Result := R.x2-R.x1;
//       Result := Nodes[1]-Nodes[0];
    end;
  end;*)
  R := BoundsRect;
  Result := R.x2-R.x1;
  FWidth := Result;
end;

procedure TSelectedArea.SetWidth(const Value: double);
begin
if FWidth<>Value then begin
   FWidth := Value;
   Recalc;
end;
end;

procedure TSelectedArea.SetZoom(const Value: double);
begin
  if FZoom <> Value then
  begin
       Magnify(Value/FZoom);
       FZoom := Value;
  end;
end;

procedure TSelectedArea.Init;
var
  I: Integer;
begin
  OrigList.Clear;
  DestList.Clear;
  OrigRect := Rect2d(0,0,0,0);
  ActualNode := -1;
  FRotAngle := 0; // fok
  FZoom := 1.0;
  for I := 0 to 9 do
      Nodes[I] := Point2d(0,0);
end;

function TSelectedArea.IsInPoint(p: TPoint2d): boolean;
    var arr: array of TPoint2d;
        i: integer;
    begin
       SetLength(arr,4);
       for i:=0 to 3 do
           arr[i] := Nodes[i];
       Result := IsPointInPoligon( arr, p );
    end;

function TSelectedArea.IsNode(p: TPoint2d; Radius: double; var idx: integer): boolean;
var i: integer;
    d: double;
    re: TRect2d;
begin
  // Radius : graviti radius
  // idx : 0..3 nodes, 4..7: midpoints, 8: RC, 9: RCent
  Result := False;
  idx := -1;
  re := Rect2d(p.x-Radius,p.y+Radius,p.x+Radius,p.y-Radius);
  for i := 0 to 9 do begin
      if PontInKep(Nodes[I].X,Nodes[I].Y,re) then
      begin
        idx := i;
        Result := true;
        Break;
      end;
  end;
end;

procedure TSelectedArea.MoveEdge(idx: integer; dx, dy : double);
var Pcent,Pcur,P: TPoint2d;
    rAngle: double;
    fpIdx : integer;
    ef : TEgyenesFgv;
    fy: double;
    BR: TRect2d;
    w,h,w1,h1: double;
begin
  rAngle := RotAngle;

  // Oldal felező pontok mozgatása
  if (idx>3) and (idx<8) then begin
  if Ortho then
  begin
    fpIdx := (idx-2) mod 4;
    FixPoint := Nodes[fpIdx];
    P := Point2d(dx,dy);
    Rotate2D(P,-DegToRad(rAngle));
    RotAngle := 0;
    if (idx=4) or (idx=6) then begin
       dx := 0; dy := P.Y;
    end;
    if (idx=5) or (idx=7) then begin
       dx := P.X; dy := 0;
    end;
  end;
     Nodes[idx mod 4] := Point2d(Nodes[idx mod 4].X+dx, Nodes[idx mod 4].y+dy );
     if idx=7 then idx:=3;
     Nodes[idx-3] := Point2d(Nodes[idx-3].X+dx, Nodes[idx-3].y+dy );
     Recalc;

     if Ortho then begin
        RotAngle := rAngle;
        Move( FixPoint.X-Nodes[fpIdx].X,FixPoint.Y-Nodes[fpIdx].Y);
     end;

     Recalc;
     exit;
  end;

  // Sarok pontok mozgatása
  if (idx>-1) and (idx<4) then begin
  if Ortho then
  begin
    fpIdx := (idx+2) mod 4;
    FixPoint := Nodes[fpIdx];
    RotAngle := 0;
    P := Point2d(dx,dy);
    Rotate2D(P,-DegToRad(rAngle));
    Recalc;
    BR := BoundsRect;       // Befoglaló téglalap
    w  := BR.x2 - BR.x1;    // szélessége
    h  := BR.y2 - BR.y1;    // magassága
    case Idx of
    0,3 : w1 := w - dx;
    1,2 : w1 := w + dx;
    end;
    // Oldalak korrekciója
    h1 := (w1/w)*h;
    SetSize(w1,h1);
    Recalc;
  end else
     Nodes[idx] := Point2d(Nodes[idx].X+dx, Nodes[idx].y+dy );

     if Ortho then begin
        RotAngle := rAngle;
        Move( FixPoint.X-Nodes[fpIdx].X,FixPoint.Y-Nodes[fpIdx].Y);
     end;
     Recalc;
     exit;
  end;
end;

// Orto mode: az oldalfelező pontok a téglalap oldalait ||-an tolja el;
// a sarokpontok pedig nagyítanak az aspect ratio megtartásával
procedure TSelectedArea.OrthoTransform(NodeIdx: integer; CurPos: TPoint2d);
var pIdx: integer;
    oldRotAngle : double;
    cP: TPoint2d;  // Kurzorpozíció
begin
if NodeIdx<8 then
begin
  oldRotAngle := RotAngle;
  // Fix pont meghatározása
  case NodeIdx of
  0..3: pIdx := (NodeIdx + 2) mod 4;
  4..7: pIdx := (NodeIdx + 3) mod 4;
  end;
  FixPoint := Nodes[pIdx];
  // Fix ponttal keret eltolása az origóba és -RotAngle elforgatása
  Move(-FixPoint.X,-FixPoint.Y);
  Rotate( Point2d(0,0),-RotAngle );
  // Kursor eltolás és elforgatás
  cP := SubPoints( CurPos, FixPoint );
  Rotate2D( cP, -RotAngle );
  // Oldalak átméretezése
//  if NodeIdx=4 then SetSize( cp.y);

  // Visszaforgatás és visszatolás a fix pontba
  Rotate( Point2d(0,0),RotAngle );
  Move(FixPoint.X,FixPoint.Y);
  RotAngle := oldRotAngle;
end;
end;

// Magnify from Cent
procedure TSelectedArea.Magnify(coeff: double);
var B,R: TRect2d;
    C  : TPoint2d; // Középpont
    dx,dy : double;
begin
  B  := BoundsRect;
  C  := Point2d((B.x2+B.x1)/2,(B.y1+B.y2)/2);
  dx := coeff * (C.X - B.x1);
  dy := coeff * (C.y - B.y1);
  R  := Rect2d( C.X-dx, C.Y-dy, C.X+dx, C.Y+dy );
  BoundsRect := R;
  Recalc;
end;

procedure TSelectedArea.Mirror(idx: integer);
Var R: TRect2d;
    CP: TPoint2d;
    i: integer;
begin
  R := BoundsRect;
  CP := Point2d( (R.x1+R.x2)/2, (R.y1+R.y2)/2 );
  case idx of
  1: // Verical mirror
       for I := 0 to 9 do
           Nodes[i].Y := 2*CP.Y - Nodes[i].Y;
  2: // Horizontal mirror
       for I := 0 to 9 do
           Nodes[i].X := 2*CP.X - Nodes[i].X;
  3: // Central mirror
       for I := 0 to 9 do
       begin
           Nodes[i].X := 2*CP.X - Nodes[i].X;
           Nodes[i].Y := 2*CP.Y - Nodes[i].Y;
       end;
  end;
  Recalc;
end;

procedure TSelectedArea.Move(dx, dy : double);
var i: integer;
begin
  for I := 0 to 9 do
      Nodes[i] := Point2d(Nodes[i].X+dx, Nodes[i].y+dy );
  Recalc;
end;
(*
procedure TSelectedArea.SetAllwaysDraw(const Value: boolean);
begin
  FAllwaysDraw := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;
*)
procedure TSelectedArea.SetOrigRect(const Value: TRect2d);
begin
  FOrigRect := CorrectRealRect(Value);
  Nodes[0]  := Point2d(FOrigRect.x1,FOrigRect.y2);
  Nodes[1]  := Point2d(FOrigRect.x2,FOrigRect.y2);
  Nodes[2]  := Point2d(FOrigRect.x2,FOrigRect.y1);
  Nodes[3]  := Point2d(FOrigRect.x1,FOrigRect.y1);
  recalc;
end;

procedure TSelectedArea.RelRotate( P: TPoint2d );
var ra0 : double;
begin
  ra0 := RelAngle2D( RCent, P );
  Self.RelRotate( ra0 );
end;

// Cent pont körül forgatja el a keretet
procedure TSelectedArea.Rotate(Cent: TPoint2d; Angle: double);
var i: integer;
begin
  for I := 0 to 7 do
      RelRotate2D( Nodes[i], Cent, Rad(angle) );
  FRotAngle := Angle;
  Recalc;
end;

// RCent (keret centruma) körül forgatja el a keretet
procedure TSelectedArea.RelRotate(angle: double);
var i: integer;
begin
  for I := 0 to 7 do
      RelRotate2D( Nodes[i], RCent, Rad(angle) );
  FRotAngle := FRotAngle + angle;
  Recalc;
end;

procedure TSelectedArea.SetRect(R: TRect2d);
begin
  CorrectRealRect(R);
  Nodes[0] := Point2d(R.x1,R.y2);
  Nodes[1] := Point2d(R.x2,R.y2);
  Nodes[2] := Point2d(R.x2,R.y1);
  Nodes[3] := Point2d(R.x1,R.y1);
  Recalc;
end;

procedure TSelectedArea.SetRotAngle(const Value: double);
begin
  if FRotAngle <> Value then begin
     RelRotate( Value-FRotAngle );
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

// Resize the area from left-bottom point
procedure TSelectedArea.SetSize(w, h: double);
var wo,wh  : double;
    p0,p1  : TPoint2d;
    R      : TRect2d;
    rAngle : double;
begin
if Ortho then begin
   rAngle := RotAngle;
   RotAngle := 0;
end;
  R := GetBoundsRect;
  CorrectRealRect(R);
  p0 := R.P1;
  p1 := Point2d( p0.X+w,p0.Y+h);
  R  := Rect2d(p0.x,p0.y,p1.X,p1.Y);
  SetBoundsRect(R);
if Ortho then
   RotAngle := rAngle;
end;

procedure TSelectedArea.SetNode(idx: integer; p: TPoint2d);
begin
  if idx<=High(Nodes) then
  begin
    Nodes[idx] := p;
    Recalc;
  end;
end;

procedure TSelectedArea.SetVisible(const Value: boolean);
begin
  FVisible := Value;
  if not fVisible then
     Init;
  if Assigned(FOnChange) then FOnChange(Self);
  if Assigned(FOnVisible) then FOnVisible(Self);
end;

procedure TSelectedArea.AddCurve(Cuv: TCurve);
var oC,dC: TCurve;
    I: Integer;
begin
  oC:= TCurve.Create;
  oC.ID    := Cuv.ID;
  oC.Name  := Cuv.Name;
  oC.Shape  := Cuv.Shape;
  oC.Closed := Cuv.Closed;
  dC:= TCurve.Create;
  dC.ID    := Cuv.ID;
  dC.Name  := Cuv.Name;
  dC.Shape  := Cuv.Shape;
  dC.Closed := Cuv.Closed;
  for I := 0 to Pred(Cuv.Count) do begin
      oC.AddPoint(Cuv.GetPoint2d(I));
      dC.AddPoint(Cuv.GetPoint2d(I));
  end;
  OrigList.Add(oC);
  DestList.Add(dC);
end;

procedure TSelectedArea.Recalc;
Var i,j            : integer;
    oPoint,dPoint  : TPoint2d;      // Point from OrigList and DestList
    aP             : TPoint2d;
    A_B,D_C        : TPoint2d;      // Vectors to real point
    P              : TPoint2d;      // Transformed point
    ef1,ef2,ef3    : TEgyenesfgv;
    w,h            : double;        // OrigRect width / height
    R              : TRect2d;
begin
     // midpoints
       Nodes[4] := FelezoPont(Nodes[0],Nodes[1]);
       Nodes[5] := FelezoPont(Nodes[1],Nodes[2]);
       Nodes[6] := FelezoPont(Nodes[2],Nodes[3]);
       Nodes[7] := FelezoPont(Nodes[3],Nodes[0]);

     // Rotation centrum
       RC  := Elometszes(Nodes[0],Nodes[1],0.2,0.2);     // Rotation point
       Nodes[8] := RC;
       ef1  := KeTPontonAtmenoEgyenes(RC,Nodes[4]);
       ef2  := KeTPontonAtmenoEgyenes(Nodes[2],Nodes[3]);
       P    := KetEgyenesMetszespontja(ef1,ef2);
       RCent:= FelezoPont(Nodes[4],P);
       Nodes[9] := RCent;

       w  := SzakaszSzog(Nodes[0].X,Nodes[0].Y,Nodes[1].x,Nodes[1].y);
       FRotAngle := RadToDeg(w);

  if OrigList.Count>0 then begin
     w := OrigRect.x2-OrigRect.x1;
     h := OrigRect.y2-OrigRect.y1;

     for I := 0 to Pred(OrigList.Count) do begin
         oCurve := OrigList.Items[i];
         dCurve := DestList.Items[i];
         if oCurve<>nil then begin
            for j := 0 to Pred(oCurve.FPoints.Count) do
            begin
                oPoint := TCurve(OrigList.Items[i]).GetPoint2d(j);          // Original pont
                // Get multiplicator (normalisation)
                aP.X   := (oPoint.X-OrigRect.x1)/w;
                aP.Y   := (OrigRect.y2-oPoint.Y)/h;
                A_B    := Osztopont(Nodes[0],Nodes[1],aP.x);
                D_C    := Osztopont(Nodes[3],Nodes[2],aP.x);
                P      := Osztopont(A_B,D_C,aP.Y);
                TCurve(DestList.Items[i]).Points[J] := p;
            end;
         end;
     end;

  end;
  if Assigned(FOnChange) {and FAllwaysDraw} then FOnChange(Self);
end;

// ================ End of TSelectedArea ===============================

{ TCustomCAD }

constructor TCustomCAD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MouseOn    := False;

  FDesigneMode      := True;
  painting          := False;
  Changed           := False;
  DrawBmp           := TBitMap.Create;
  LastCPIndex       :=-1;         // nincs pont
  LastCPCurve       :=-1;         // nincs curve
  CPIndex           :=-1;         // nincs pont
  CPCurve           :=-1;         // nincs curve
  FBoxing           := False;
  MouseInOut        := 1;
  Origin            := Point(0,0);
  MovePt            := Origin;
  oldMovePt         := MovePt;
  fDefaultLayer     := 0;
  fSensitiveRadius  := 8;
  delta             := fSensitiveRadius;
  fPointWidth       := 3;
  fDrawMode          := dmNone;
  FTitleFont        := TFont.Create;
  With FTitleFont do begin
       Name := 'Times New Roman';
       Color:= clNavy;
       Size := 8;
  end;
  innerStream := TMemoryStream.Create;
//  VirtualClipboard := TMemoryStream.Create;
  fAutoUndo            := True;
  UR:= TUndoRedo.Create;
  Ur.UndoLimit         := 100;
  Ur.UndoSaveProcedure := SaveGraphToMemoryStream;
  Ur.UndoRedoProcedure := LoadGraphFromMemoryStream;
  Ur.OnUndoRedo        := UndoRedo;
  UndoInit;
  TempCurve            := TCurve.Create;
  FEnablePaint         := True;
  fSelectedFrame        := TSelectedArea.Create;
//  SelectedFrame.AllwaysDraw := True;
  fSelectedFrame.OnChange := change;
  fDefiniedLength      := false;
  defLength            := 100;
  FBackImage           := TBMPobject.Create;
  FBackImage.OnChange  := Change;
  fShowNumbers         := False;
  FCursorCross         := False;
  FAutoPoligonize      := True;
  fEnableRecall        := True;
  TabStop              := true;
  FClone_Contour       := 5;
  FVecCoeff            := 0.05;
  OnBeforeDraw         := BeforeDraw;
  OnAfterDraw          := AfterDraw;
  OnBeforePaint        := BeforePaint;
  OnAfterPaint         := AfterPaint;

  StrCurve             := TCurve.Create;
  StrCurve.Name        := 'StrCurve';
  StrCurve.Shape       := dmText;
  StrCurve.AddPoint(0,0);

  ZoomPaper;
end;

destructor TCustomCAD.Destroy;
begin
if Self <> nil then begin
  STOP := True;
  FCurve.Free;
  if SelectedFrame<>nil then
     SelectedFrame.Free;
  if FBackImage<>nil then
     FBackImage.Free;
  if TempCurve<>nil then TempCurve.Free;
  FTitleFont.Free;
  UR.Free;
  innerStream.Free;
//  VirtualClipboard.Free;
end;
  inherited Destroy;
end;

procedure TCustomCAD.BeforeDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
VAR bRect: TRect2d;
begin
  With ca do begin
  if FC<>nil then
  begin

     // Draw bounding boxes
     if ShowBoxes and (FC.Count>0) and (FC.Shape<>dmInsert) then
     IF FC.BlockParams.BlockName='' then begin
       bRect := FC.BoundsRect;
       Pen.Color := clRed;
       Pen.Width := 1;
       Pen.Style := psDot;
       Brush.Style := bsClear;
       Rectangle(xtos(bRect.x1),ytos(bRect.y1),xtos(bRect.x2),ytos(bRect.y2));
     end;

     // Tollak beállítása =====================================================

       If FC.Closed then
        Pen.Assign(CADPens.pClosed)
     else
        Pen.Assign(CADPens.pOpened);

     if FC.Signed then
       Pen.Assign(CADPens.pSigned);
     if FC.Crossed then
       Pen.Assign(CADPens.pCrossed);
     if FC.Sorted then
       Pen.Assign(CADPens.pSorted);
     If (FC.Selected) then begin
       Pen.Width := 4;
       Pen.Assign(CADPens.pSelected);
     end;

     if SelectedVisible and (SelectedIndex>-1) then
     if FC = Curves[SelectedIndex] then
     begin
        Pen.Width := 4;
        if FC.Selected then
           Pen.Color := clFuchsia
        else
           Pen.Color := clRed;
     end;

  end;
  end;
  inherited;
end;

procedure TCustomCAD.BeforePaint(Sender: TObject);
begin
  inherited;
end;

procedure TCustomCAD.AfterDraw(Sender: TObject; ca: TCanvas; FC: TCurve);
begin
  inherited;
end;

function TCustomCAD.MakeCurve(const AName: Str32; ID: integer;
  Shape: TDrawMode; AEnabled, AVisible, AClosed: Boolean): Integer;
begin
  Result := FCADSource.FCurveList.MakeCurve(AName, ID, Shape, AEnabled, AVisible, AClosed);
  Changed := True;
  if not loading then if Assigned(fChangeAll) then fChangeAll(Self);
end;

procedure TCustomCAD.MakeNewCurve(AIndex: integer; var cuv: TCurve);
begin
  FCADSource.FCurveList.MakeNewCurve(AIndex,cuv);
end;

procedure TCustomCAD.Clear;
begin
  if SelectedFrame.Visible then
     DoSelectedFrame(false);
  FCADSource.Clear;
  BackImage.Clear;
  SelectedIndex := -1;
  Recall;
end;

function TCustomCAD.AddCurve(ACurve: TCurve): integer;
begin
  FCADSource.FCurveList.AddCurve(ACurve);
  Recall;
end;

procedure TCustomCAD.DeleteCurve(AItem: Integer);
begin
  FCADSource.FCurveList.DeleteCurve(AItem);
  Recall;
end;

procedure TCustomCAD.DeleteSelectedCurves;
begin
  FCADSource.FCurveList.DeleteSelectedCurves;
  if AutoUndo then UndoSave;
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;

procedure TCustomCAD.DeleteInvisibleCurves;
begin
  FCADSource.FCurveList.DeleteInvisibleCurves;
end;

procedure TCustomCAD.DeleteEmptyCurves;
begin
  FCADSource.FCurveList.DeleteEmptyCurves;
end;

procedure TCustomCAD.InsertCurve(AIndex: Integer; Curve: TCurve);
begin
  FCADSource.FCurveList.InsertCurve(AIndex,Curve);
  if AutoUndo then UndoSave;
  Recall;
end;

function TCustomCAD.GetCurveName(H: Integer): Str32;
begin
  Result := FCADSource.FCurveList.GetCurveName(H);
end;

function TCustomCAD.GetCurveHandle(AName: Str32; var H: Integer): Boolean;
begin
  Result := FCADSource.FCurveList.GetCurveHandle(AName,H);
end;

function TCustomCAD.GetCurveIndex(AName: Str32): Integer;
begin
  Result := FCADSource.FCurveList.GetCurveIndex(AName);
end;

function TCustomCAD.ShapeCount(Shape: TDrawMode): Integer;
begin
  Result := FCADSource.FCurveList.ShapeCount(Shape);
end;

procedure TCustomCAD.CloneCurve(AIndex: integer);
begin
  FCADSource.FCurveList.CloneCurve(AIndex);
  Recall;
end;

procedure TCustomCAD.CloneContour(AIndex: integer);
var h,i: integer;
begin
  if (AIndex>-1) and (Aindex<FCADSource.FCurveList.Count) and (not Loading) then
  begin
  UndoSave;
  Curves[AIndex].SetContour( Clone_Contour );
  h:=MakeCurve('Clone',-1,Curves[AIndex].Shape,Curves[AIndex].Enabled,
                          Curves[AIndex].Visible,Curves[AIndex].Closed);
  for I := 0 to Pred(Curves[AIndex].FContour.Count) do
      AddPoint(h,Curves[AIndex].FContour.Points[i]);
  UndoSave;
//  Recall;
  Changed := True;
  end;
end;

procedure TCustomCAD.CloneSeledted;
var
   i: integer;
begin
  if GetSelectedCount > 0 then
  begin
         For i:=0 to FCADSource.FCurveList.Count-1 do begin
             FCurve:=Curves[i];
             if FCurve.Selected then
                CloneCurve(i);
         end;
    Changed := True;
    Recall;
  end;
end;

procedure TCustomCAD.CreateBoxObject(AIndex: integer);
begin

end;

// Az AIndex által megjelölt nyílt objektummel kettévágunk egy objektumot.
// A gyakorlatban ezt úgy tehetjük, hogy egy Line/Polyline vonallal teljesen
// átmetsszük a darabolni kívánt objektumot. A metszéspontokat beinsertáljuk és
// a közüttük lévő rész határoló vonala lesz a ketté metszett új objektumoknak.
procedure TCustomCAD.CuttingObject(AIndex: integer);
var
    h,nh: integer;
    i,n1,n2: integer;
    AP,BP: TPoint2d;
    mpCount: integer;
    BePont,KiPont: TInOutRec;

begin

  if (AIndex>-1) and (Aindex<FCADSource.FCurveList.Count) and (not Loading) then
  begin
     if AutoUndo then UndoSave;       // Előtte mentés

     AP := Curves[Aindex].Points[0];                          // 0. pont
     BP := Curves[Aindex].Points[Pred(Curves[Aindex].Count)]; // Utolsó pont

     // Ha átvág egy objektumot
     h:=-1;
     If IsCutObject(AP,BP,h) then
     if h>-1 then begin
          mpCount := ConturInOut(Curves[h],AP,BP,BePont,KiPont);
          if mpCount=2 then
          begin
               n1 := Bepont.idx;
               n2 := Kipont.idx;
               if Bepont.idx>KiPont.idx then begin
                  n1 := Kipont.idx;
                  n2 := Bepont.idx;
                  Curves[h].InsertPoint(n1,KiPont.mPont.x,KiPont.mPont.y);
                  Curves[h].InsertPoint(n2+1,BePont.mPont.x,BePont.mPont.y);
               end else begin;
                  Curves[h].InsertPoint(n1,BePont.mPont.x,BePont.mPont.y);
                  Curves[h].InsertPoint(n2+1,KiPont.mPont.x,KiPont.mPont.y);
               end;

               Curves[h].SelectAllPoints(false);

               nh:=MakeCurve('Cut',-1,Curves[h].Shape,Curves[h].Enabled,
                          Curves[h].Visible,Curves[h].Closed);


               for I := n1 to n2+1 do
                   AddPoint(nh,Curves[h].Points[i]);
               MoveCurve(FCADSource.FCurveList.Count-1,0,16);
               Curves[nh].Selected := true;

               for I := n2 downto n1+1 do
                   DeletePoint(h,i);

               DeleteCurve(AIndex);

               if AutoUndo then UndoSave;       // Utána mentés

          end;
     end;
     invalidate;
  end;

end;

// Megvizsgálja, hogy a p1-p2 szakasz átvágja valamelyik vagy több objektumot.
//     Aindex = az elsőként érintett objektum sorszáma
function TCustomCAD.IsCutObject(p1, p2: TPoint2d; var Aindex: integer): boolean;
var i: integer;
    t: Trect2d;
    Cuv : TCurve;
begin
   Result := False;
   For i:=0 to Pred(FCADSource.FCurvelist.Count) do begin
      Cuv := Curves[I];
      t := Cuv.BoundsRect;
      if Cuv.Visible and (IsSzakaszNegyszogMetszes(p1,p2,t)) then
      if Cuv.IsCutLine(p1,p2) then begin
         Aindex := i;
         Result := True;
         exit;
      end;
   end;
end;

// Belépési és kilépési pontok keresése a kontúron (körvonalon)
// Result = metszéspontok száma
function TCustomCAD.ConturInOut(cCuv: TCurve; AP, BP: TPoint2d; var BE,
  KI: TInOutRec): integer;
Var i     : integer;
    idx1,idx2 : integer;    // metszéspontot megelőző kontúrpont indexe
    mPonts: TMemoryStream;  // a metszéspont rekordokat ide teszem
    P1,P2 : TPoint2d;       // Kontúr két egymást követő pontja
    mp    : TPoint2d;       // Kontúr metszéspontja
    d     : double;         // Metszéspont távolsága AP kezdőponttól
    M_P   : TInOutRec;      // METSZÉSPONT REKORDJA
    d1,d2 : double;         // be és kilépő metszéspontok
begin
  Result := 0;
  // A legközelebbi és legtávolabbi metszéspontok megkeresése
  d1:=10e+10; d2:= -1;
  For i:=0 to Pred(cCuv.Count) do begin
      P1 := cCuv.GetPoint2d(i);
      if i=Pred(cCuv.Count) then   // Utolsó pont után a 0. pontot kell venni
         P2 := cCuv.Points[0]
      else
         P2 := cCuv.GetPoint2d(i+1);
      if SzakaszSzakaszMetszes(AP,BP,P1,P2,mp) then begin // Ha van metszéspont
         d := RelDist2d(AP,mp);
         if d<d1 then begin
            BE.mPont := mp;
            BE.idx   := i+1;
            BE.d     := d;
            d1 := d;
         end;
         if d>d2 then begin
            KI.mPont := mp;
            KI.idx   := i+1;
            KI.d     := d;
            d2 := d;
         end;
         Inc(Result);
      end;
  end;
end;

// Az aktív pont körül senzitiveradius távolságban kört rajzol
procedure TCustomCAD.ShowMagneticCircle(x, y: TFloat; enab: boolean);
var p: TPoint;
begin
  Paint;
  if enab then begin
     p := WToS(Point2d(x,y));
     Canvas.Pen.Color := clBlue;
     Canvas.Pen.Width := 2;
     Canvas.Pen.Mode := pmCopy;
     Canvas.Ellipse(P.x-SensitiveRadius,P.y-SensitiveRadius,
                 P.x+SensitiveRadius,P.y+SensitiveRadius);
  end;
end;

procedure TCustomCAD.AddPoint(AIndex: Integer; X, Y: TFloat);
begin
  FCADSource.FCurveList.AddPoint(AIndex,X,Y);
//  Recall;
end;

procedure TCustomCAD.AddPoint(AIndex: Integer; P: TPoint2d);
begin
  FCADSource.FCurveList.AddPoint(AIndex,P);
end;

procedure TCustomCAD.InsertPoint(AIndex, APosition: Integer; X, Y: TFloat);
begin
  FCADSource.FCurveList.InsertPoint(AIndex,APosition,X,Y);
  Recall;
end;

procedure TCustomCAD.InsertPoint(AIndex, APosition: Integer; P: TPoint2d);
begin
  FCADSource.FCurveList.InsertPoint(AIndex,APosition,P);
  Recall;
end;

procedure TCustomCAD.DeletePoint(AIndex, APosition: Integer);
begin
  FCADSource.FCurveList.DeletePoint(AIndex,APosition);
  Recall;
end;

procedure TCustomCAD.DeleteSamePoints(diff: TFloat);
begin
  FCADSource.FCurveList.DeleteSamePoints(diff);
end;

procedure TCustomCAD.ChangePoint(AIndex, APosition: Integer; X, Y: TFloat);
begin
  FCADSource.FCurveList.ChangePoint(AIndex,APosition,X,Y);
  Recall;
end;

procedure TCustomCAD.DoMove(Dx, Dy: Integer);
begin
  if CPMatch then begin
    CPx:=XToW(Dx);
    CPy:=YToW(Dy);
    ChangePoint(CPCurve,CPIndex,XToW(Dx),YToW(Dy));
    if not loading then
       if Curves[CPCurve].Closed then
          Curves[CPCurve].SetContour(ContourRadius);
    Recall;
  end;
end;

procedure TCustomCAD.GetPoint(AIndex, APosition: Integer; var X, Y: TFloat);
begin
  FCADSource.FCurveList.GetPoint(AIndex, APosition,X,Y);
end;

function TCustomCAD.GetMaxPoints: Integer;
var
  I,Max: Integer;
begin
  Max:=0;
  for I:=0 to Pred(FCADSource.FCurveList.Count) do
  begin
    FCurve:=FCADSource.FCurveList.Items[I];
    if FCurve<>nil then
    if FCurve.FPoints.Count > Max then Max:=FCurve.FPoints.Count;
  end;
  Result:=Max;
end;

// Searching for the nearest point in graph
// Result : distance from p point
//    VAR : cuvIdx = Curve's number
//          pIdx   = Point's number
function  TCustomCAD.GetNearestPoint(p: TPoint2d; var cuvIdx, pIdx: integer): TFloat;
var
  I,J : Integer;
  d   : Double;
  x,y : double;
  Cuv : TCurve;
  p0,p1,p2,mp : TPoint2d;
begin
  Result := 10e+10;
  cuvIdx := -1;
  pIdx   := -1;
  for I:=0 to Pred(FCADSource.FCurveList.Count) do
  begin
    Cuv:=FCADSource.FCurveList.Items[I];
    if Cuv.Visible then
    For J:=0 to Pred(Cuv.FPoints.Count) do
    begin
        Cuv.GetPoint(j,x,y);
        d:=KetPontTavolsaga(p.x,p.y,x,y);
        if d<Result then begin
           cuvIdx := I;
           pIdx   := J;
           Result := d;
        end;
    end;
  end;
  // Ha talál objektumot, meg kell vizsgálni, hogy nem metszi-e:
  // Ha igen, akkor a metszett közeli vonalszakasz legközelebbi végpontja kell.
  if CuvIdx>-1 then begin
     Cuv := FCADSource.FCurveList.Items[CuvIdx];
     p0:=Cuv.GetPoint2d(pIdx);
     if Cuv.IsCutLine(p,p0) then begin
        For J:=0 to Cuv.FPoints.Count-2 do begin
            p1:=Cuv.GetPoint2d(j);
            p2:=Cuv.GetPoint2d(j+1);
            if SzakaszSzakaszMetszes(p,p0,p1,p2,mp) then begin
               if KetPontTavolsaga(p.x,p.y,p1.x,p1.y)<=
                  KetPontTavolsaga(p.x,p.y,p2.x,p2.y)
               then begin
                  pIdx   := J;
                  d:=KetPontTavolsaga(p.x,p.y,p1.x,p1.y);
               end else begin
                  pIdx   := J+1;
                  d:=KetPontTavolsaga(p.x,p.y,p2.x,p2.y);
               end;
               Break;
            end;
        end;
     end;
  end;
  // Ha nem talál, akkor a távolság -1; CuvIdx=-1; pIdx=-1;
  if cuvIdx=-1 then Result := -1;
end;

procedure TCustomCAD.SetNearestBeginPoint(p: TPoint2d);
var CuvIdx,NodeIdx: integer;
begin
  if GetNearestPoint( p,CuvIdx,NodeIdx )>-1 then begin
     SetBeginPoint( CuvIdx,NodeIdx );
     Recall;
  end;
end;

procedure TCustomCAD.SetBeginPoint(ACurve, AIndex: Integer);
begin
Try
       FCurve:=FCADSource.FCurveList.Items[ACurve];
       FCurve.SetBeginPoint(AIndex);
       Selected := FCurve;
       Changed := True;
       if Assigned(fNewBeginPoint) then fNewBeginPoint(Self,ACurve);
       Recall;
except
end;
end;

procedure TCustomCAD.SetOutherBeginPoint(Ax, Ay: TFloat);
begin
Try
       GetNearestPoint(Point2d(Ax,Ay),CPCurve,CPIndex);
       FCurve:=FCADSource.FCurveList.Items[CPCurve];
       FCurve.SetOutherBeginPoint(Ax,Ay);
       Selected := FCurve;
       Changed := True;
       if Assigned(fNewBeginPoint) then fNewBeginPoint(Self,CPCurve);
       Recall;
except
end;
end;

procedure TCustomCAD.MoveCurve(AIndex: integer; Ax, Ay: TFloat);
var i: integer;
begin
  if InRange(AIndex,0,Pred(FCADSource.FCurveList.Count)) then begin
      FCurve:=Curves[AIndex];
      FCurve.MoveCurve(Ax/Zoom, Ay/Zoom);
      Selected := FCurve;
      Changed := True;
      Recall;
  end;
end;

procedure TCustomCAD.MoveSelectedCurves(Ax, Ay: TFloat);
var i: integer;
begin
  for i:=0 to Pred(FCADSource.FCurveList.Count) do begin
      FCurve:=Curves[i];
      if FCurve.Selected then begin
         FCurve.MoveCurve(Ax/Zoom, Ay/Zoom);
         if FCurve.Shape=dmInsert then
            CadSource.SetBlockBoundsRect(FCurve);
      end
      else
      if ShowPoints then
      begin
         FCurve.MoveSelectedPoints(Ax/Zoom, Ay/Zoom);
         if FCurve.Shape=dmInsert then
            CadSource.SetBlockBoundsRect(FCurve);
      end;
  end;
  Repaint;
  Changed := True;
end;
(*
begin
  FCurveList.MoveSelectedCurves(Ax/Zoom, Ay/Zoom);
  Recall;
end;
*)
procedure TCustomCAD.RotateSelectedCurves(Cent: TPoint2d; Angle: TFloat);
begin
  FCADSource.FCurveList.RotateSelectedCurves(Cent,Angle);
  Recall;
end;

procedure TCustomCAD.InversSelectedCurves;
begin
  FCADSource.FCurveList.InversSelectedCurves;
  Recall;
end;

// Draw invers curve
procedure TCustomCAD.InversCurve(AIndex: Integer);
var
  I,H,N: Integer;
  X,Y: TFloat;
  Size: Word;
  PA: Array[0..1000] of TPoint;
  p0: TPoint;
  R : HRgn;
begin
  if InRange(AIndex,0,Pred(FCADSource.FCurveList.Count)) then
  begin
  Try
      FCurve:=FCADSource.FCurveList.Items[AIndex];
      N := FCurve.FPoints.Count+1;
      for I:=0 to Pred(N) do
      begin
        FCurve.GetPoint(I,X,Y);
        PA[I].x:=Trunc(X);
        PA[I].y:=Trunc(Y);
      end;
        FCurve.GetPoint(0,X,Y);
        PA[i+1].x:=Trunc(X);
        PA[i+1].y:=Trunc(Y);
        {$ifdef WINDOWS}
	  R := CreatePolygonRgn(PA,N,ALTERNATE);
          FillRgn(Canvas.Handle,R,Canvas.Brush.Handle);
        {$endif}
  finally
     DeleteObject(R);
  end;
  end;
end;

procedure TCustomCAD.SelectCurveByName(aName: string);
var n: integer;
begin
   For n:=0 to FCADSource.FCurveList.Count-1 do begin
      FCurve:=FCADSource.FCurveList.Items[n];
      if FCurve.Name = aName then begin
         Selected := FCurve;
         Recall;
      end;
   end;
end;

procedure TCustomCAD.SelectCurve(AIndex: Integer);
begin
  FCADSource.FCurvelist.SelectCurve(AIndex);
  Recall;
end;

procedure TCustomCAD.PoligonizeAll(PointCount: integer);
begin
  FCADSource.FCurvelist.PoligonizeAll(PointCount);
  Recall;
end;

procedure TCustomCAD.PoligonizeAllSelected(PointCount: integer);
begin
  FCADSource.FCurvelist.PoligonizeAllSelected(PointCount);
  Recall;
end;

procedure TCustomCAD.Poligonize(Cuv: TCurve; PointCount: integer);
begin
  FCADSource.FCurvelist.Poligonize(Cuv,PointCount);
  Recall;
end;

procedure TCustomCAD.VektorisationAll(MaxDiff: TFloat);
begin
  FCADSource.FCurvelist.VektorisationAll(MaxDiff);
  Recall;
end;

procedure TCustomCAD.VektorisationAllSelected(MaxDiff: TFloat);
begin
  FCADSource.FCurvelist.VektorisationAllSelected(MaxDiff);
  Recall;
end;

procedure TCustomCAD.Vektorisation(MaxDiff: TFloat; Cuv: TCurve);
begin
  FCADSource.FCurvelist.Vektorisation(MaxDiff,Cuv);
  Recall;
end;

procedure TCustomCAD.PontSurites(Cuv: TCurve; Dist: double);
begin
  FCADSource.FCurvelist.PontSurites(Cuv,Dist);
  Recall;
end;

procedure TCustomCAD.PontSuritesAll(Dist: double);
begin
  FCADSource.FCurvelist.PontSuritesAll(Dist);
  Recall;
  If AutoUndo then UndoSave;
end;

procedure TCustomCAD.CheckCurvePoints(X, Y: Integer);
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

    xx := XToW(x);
    yy := YToW(y);

    delta := fSensitiveRadius/fzoom;

    J:=Pred(FCADSource.FCurveList.Count);

    for I:=J downto 0 do
    begin

      if Curves[I]=NIL then
         EXIT;

      R := Curves[I].BoundsRect;

      if Curves[I].IsInBoundsRect(xx,yy,delta) then begin

         if Curves[I].Shape in [dmCircle,dmEllipse,dmArc] then
         begin
         if drawmode=dmNone then begin
            Curves[I].FillTempCurve;
            FC:=Curves[I].TempCurve;
            Poligonize( FC,10);
         end else exit;
         end
         else
            FC:=Curves[I];

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

// Select/unselect all curves
procedure TCustomCAD.SelectAll(all: boolean);
var i,j: integer;
    cuv: TCurve;
    pr: TPointRec;
begin
  FCADSource.FCurveList.SelectAll(all);
  if Assigned(FChangeAll) then
     FChangeAll(Self);
  Recall;
end;

// Select all curves in Area
procedure TCustomCAD.SelectAllInArea(R: TRect2D);
var i: integer;
    cuv: TCurve;
    RR,RC : TRect2d;
begin
  FCADSource.FCurveList.SelectAllInArea(R);
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;

// Select all points in Area
procedure TCustomCAD.SelectAllInAreaEx(R: TRect2d);
var i,j: integer;
    cuv: TCurve;
    RR,RC : TRect2d;
    pr: TPointrec;
begin
  FCADSource.FCurveList.SelectAllInAreaEx(R);
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;

procedure TCustomCAD.AddSelectedToSelectedFrame;
var i: integer;
    cuv: TCurve;
begin
  PoligonizeAllSelected(0);
  if SelectedFrame.Visible then
  for i:=0 to Pred(FCADSource.FCurveList.Count) do begin
      Cuv:=FCADSource.FCurveList.Items[i];
      if Cuv.Selected then
            SelectedFrame.AddCurve(Cuv);
  end;
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Invalidate;
end;

procedure TCustomCAD.ClosedAll(all: boolean);
begin
  FCADSource.FCurveList.ClosedAll(all);
  Recall;
end;

procedure TCustomCAD.BombAll;
begin
  FCADSource.FCurveList.BombAll;
  if AutoUndo then UndoSave;
  Recall;
end;

procedure TCustomCAD.SelectAllCut;
var i: integer;
    cuv: TCurve;
begin
  for i:=0 to Pred(Count) do begin
      Cuv:=Curves[i];
      if Copy(cuv.Name,1,3)='Cut' then
         Cuv.Selected := True;
  end;
end;

procedure TCustomCAD.SelectAllPolylines;
begin
  FCADSource.FCurveList.SelectAllPolylines;
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;

procedure TCustomCAD.SelectAllPolygons;
begin
  FCADSource.FCurveList.SelectAllPolygons;
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;

procedure TCustomCAD.SelectParentObjects;
begin
  FCADSource.FCurveList.SelectParentObjects;
  Recall;
end;

procedure TCustomCAD.SelectChildObjects;
begin
  FCADSource.FCurveList.SelectChildObjects;
  Recall;
end;

function TCustomCAD.GetSelectedCount: integer;
begin
  Result := FCADSource.FCurveList.SelectedCount;
end;

// Return the least rect of selected objects
function TCustomCAD.GetSelectArea(var RArea: TRect2d): boolean;
var i: integer;
    boundsR: TRect2d;
begin
   Result := False;
   RArea := Rect2d(MaxDouble,MaxDouble,MinDouble,MinDouble);
   if GetSelectedCount>0 then
   begin
   For i:=0 to Pred(FCADSource.FCurvelist.Count) do begin
       FCurve:=Curves[i];
       if FCurve.Selected then begin
          boundsR := FCurve.BoundsRect;
          if boundsR.x1<RArea.x1 then RArea.x1 := boundsR.x1;
          if boundsR.x2>RArea.x2 then RArea.x2 := boundsR.x2;
          if boundsR.y1<RArea.y1 then RArea.y1 := boundsR.y1;
          if boundsR.y2>RArea.y2 then RArea.y2 := boundsR.y2;
       end;
   end;
     Result := true;
   end;
end;

// Change the Shape property of the Selected Objects
procedure TCustomCAD.ChangeSelectedShape(newShape: TDrawMode);
var i: integer;
begin
  UndoSave;
  if (SelectedCount=0) and (Selected<>nil) then
  begin
     Selected.Shape := newShape;
     Selected.Closed := newShape in [dmPolygon];
  end
  else
  for i:=0 to Pred(FCADSource.FCurveList.Count) do
  begin
      if Curves[i].Selected then
         Curves[i].Shape := newShape;
      Curves[i].Closed := newShape in [dmPolygon];
  end;
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Invalidate;
  if AutoUndo then UndoSave;
end;

procedure TCustomCAD.EnabledAll(all: boolean);
begin
if FCADSource<> nil then
begin
  FCADSource.FCurveList.EnabledAll(all);
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Recall;
end;
end;

procedure TCustomCAD.SignedAll(all: boolean);
begin
if FCADSource<> nil then
begin
  FCADSource.FCurveList.SignedAll(all);
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Invalidate;
end;
end;

procedure TCustomCAD.CrossedAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(FCADSource.FCurveList.Count) do
      Curves[i].Crossed:=all;
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Invalidate;
end;

procedure TCustomCAD.SortedAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(FCADSource.FCurveList.Count) do
      Curves[i].Sorted:=all;
  if not MouseOn then
  if Assigned(FChangeAll) then FChangeAll(Self);
  Invalidate;
end;

procedure TCustomCAD.SignedNotCutting;
var i,j,k: integer;
    BaseCurve,Cuv: TCurve;
    p,p0: TPoint2d;
    R1,R2: TRect2d;
begin
if EnableCutterMethods then
begin
  FCADSource.FCurveList.SignedNotCutting;
  invalidate;
end;
end;

function TCustomCAD.GetSignedCount: integer;
var i: integer;
begin
   Result := FCADSource.FCurvelist.GetSignedCount;
end;

function TCustomCAD.GetCrossedCount: integer;
var i: integer;
begin
   Result := FCADSource.FCurvelist.GetCrossedCount;
end;

// Selected nyílt objektumokat összefűzi egy zárt alakzattá
procedure TCustomCAD.JoinSelected;
var i,n: integer;
    Cuv: TCurve;
    selArray: array of integer;
    NextIdx  : integer;

    function InitSelArray: integer;
    var ii,jj: integer;
    begin
       jj := 0;
       SetLength(selArray,GetSelectedCount);
       for ii:=0 to Pred(FCADSource.FCurveList.Count) do begin
         if Curves[ii].Selected {and (Curves[ii].shape in [dmLine,dmPolyLine])} then
         begin
           selArray[jj] := ii;
           Inc(jj);
         end;
       end;
       Result := jj;
    end;


    (* Megvizsgálja, hogy melyik a legközelebbi selected polyline, ami nem
       signed, és megnézi, hogy az eleje vagy a vége van-e közelebb.
       Felfűzi a Cuv zárt objektumra
    *)
    function AppendNearestSelectedLine: boolean;
    var ii,jj: integer;
        d,d1,d2 : double;
        idx  : integer;
        elore: boolean;
        pv   : TPoint2d;
        p0,p1: TPoint2d;
    begin
      Result := true;
      idx := -1; elore := true;
      d := MaxDouble;
      for II := 0 to High(selArray) do
      begin
        if selArray[ii]>-1 then
        begin
          pv := Cuv.Points[Cuv.Count-1];
          p0 := Curves[selArray[ii]].Points[0];
          p1 := Curves[selArray[ii]].Points[Curves[selArray[ii]].Count-1];
          d1 := RelDist2d(pv,p0);
          d2 := RelDist2d(pv,p1);
          if RelDist2d(pv,p0)<d then
          begin
            d := RelDist2d(pv,p0);
            idx := ii;
            elore := true;
          end;
          if RelDist2d(pv,p1)<d then
          begin
            d := RelDist2d(pv,p1);
            idx := ii;
            elore := false;
          end;
        end;
      end;

        if idx>-1 then
        begin
           if not elore then
              Curves[selArray[idx]].InversPointOrder;
           for jj := 0 to Pred(Curves[selArray[idx]].Count) do
               Cuv.AddPoint(Curves[selArray[idx]].Points[jj]);
           selArray[idx] := -1;
           Result := False;
        end;

    end;

begin
if GetSelectedCount>1 then
Try
  PoligonizeAllSelected(0);
  n := InitSelArray;
  Cuv := Tcurve.Create;
  Cuv := Curves[selArray[0]];
  selArray[0]  := -1;
  Cuv.Shape    := dmPolygon;
  Cuv.Closed   := True;
  Cuv.Selected := False;

  While not AppendNearestSelectedLine do;

Finally
//  AddCurve(Cuv);
  SetLength(selArray,0);
  DeleteSelectedCurves;
  Changed := True;
  RePaint;
  if AutoUndo then UndoSave;
End;
end;

// Mirror selected object. Centrum is the midle line of select area
procedure TCustomCAD.MirrorSeledted(Horiz, Vert: boolean);
var
   i,j: integer;
   xx,yy: double;
   rSel: TRect2d;
   P: TPoint2d;
begin
  if GetSelectedCount > 0 then
  begin
    if GetSelectArea( rSel ) then
    begin
         // Centrum of the selected rect
         xx := (rSel.x1+rSel.x2)/2;
         yy := (rSel.y1+rSel.y2)/2;

         For i:=0 to FCADSource.FCurveList.Count-1 do begin
             FCurve:=Curves[i];
             if FCurve.Selected then
             for j:=0 to Pred(FCurve.Count) do begin
                 P := FCurve.Points[j];
                 if Horiz then
                    FCurve.ChangePoint(j,xx+(xx-P.X),P.y);
                 P := FCurve.Points[j];
                 if Vert then
                    FCurve.ChangePoint(j,p.x,yy+(yy-P.Y));
             end;
         end;
    Changed := True;
    Recall;
    end;
  end;
end;

procedure TCustomCAD.Normalisation(Down: boolean);
var r: TRect2d;
begin
  r := GetDrawExtension;
  if ShapeCount(dmPolyLine)>0 then
  begin
     if down then Eltolas(-r.x1,-r.y1)
     else Eltolas(-r.x1,Paper.y-r.y2);
  end else
  begin
     if down then Eltolas(-r.x1+Grid.margin,-r.y1+Grid.margin)
     else Eltolas(-r.x1+Grid.margin,Paper.y-r.y2-Grid.margin);
  end;
  Recall;
end;

procedure TCustomCAD.NormalisationEx(Down: boolean);
var r: TRect2d;
begin
  r := GetDrawExtension;
  if down then Eltolas(-r.x1,-r.y1)
  else Eltolas(-r.x1,Paper.y-r.y2);
  Recall;
end;

procedure TCustomCAD.Eltolas(dx, dy: double);
begin
  FCADSource.FCurvelist.Eltolas(dx, dy);
  Recall;
end;

procedure TCustomCAD.Nyujtas(tenyezo: double);
begin
  FCADSource.FCurvelist.Nyujtas(tenyezo);
  Recall;
end;

procedure TCustomCAD.CentralisNyujtas(Cent: TPoint2d; tenyezo: double);
begin
  FCADSource.FCurvelist.CentralisNyujtas(Cent,tenyezo);
  Recall;
end;

procedure TCustomCAD.MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
begin
  FCADSource.FCurvelist.MagnifySelected(Cent,Magnify);
  Recall;
end;

procedure TCustomCAD.MirrorHorizontal;
begin
  FCADSource.FCurvelist.MirrorHorizontal;
  Recall;
end;

procedure TCustomCAD.MirrorVertical;
begin
  FCADSource.FCurvelist.MirrorVertical;
  Recall;
end;

procedure TCustomCAD.MirrorCentral;
begin
  FCADSource.FCurvelist.MirrorCentral;
  Recall;
end;

procedure TCustomCAD.CopySelectedToVirtClipboard;
var i: integer;
    Cuv: TCurve;
    GraphData: TNewGraphData;
begin
try
      ClipboardStr := '';
      GraphData.GraphTitle:=FGraphTitle;
      GraphData.Curves:=0;
      VirtualClipboard.Clear;
      VirtualClipboard.Write(GraphData,SizeOf(GraphData));
      i:=0;
      if FCADSource.FCurveList.Count>0 then
      While i<=FCADSource.FCurveList.Count-1 do begin
            Cuv:=FCADSource.FCurveList.Items[i];
            if Cuv.Selected then begin
               Cuv.SaveCurveToStream(VirtualClipboard);
               ClipboardStr := ClipboardStr + Cuv.CurveToText;
               Inc(GraphData.Curves);
            end;
      Inc(i);
      end;
      VirtualClipboard.Seek(0,0);
      VirtualClipboard.Write(GraphData,SizeOf(GraphData));
      Clipboard.AsText := ClipboardStr;
except
end;
end;

procedure TCustomCAD.CutSelectedToVirtClipboard;
begin
  CopySelectedToVirtClipboard;
  DeleteSelectedCurves;
  If AutoUndo then UndoSave;
  Changed := True;
  Invalidate;
end;

procedure TCustomCAD.PasteSelectedFromVirtClipboard;
begin
  IF VirtualClipboard.Size>0 then begin
    Append := True;
    LoadGraphFromMemoryStream(VirtualClipboard);
    If AutoUndo then UndoSave;
    Changed := True;
    Invalidate;
  end;
end;

procedure TCustomCAD.CopyScreenToClipboard;
begin
     Clipboard.Assign(DrawBmp);
end;

procedure TCustomCAD.SaveGraphToMemoryStream(var stm: TMemoryStream);
var
  GraphData: TNewGraphData;
  N: Integer;
begin
if FCADSource<>nil then
    try
      FCADSource.SaveToStream(stm);
(*      GraphData.GraphTitle:=FGraphTitle;
      GraphData.Curves:=FCADSource.FCurveList.Count;
      stm.Clear;
      stm.Write(GraphData,SizeOf(GraphData));

      for N:=0 to Pred(GraphData.Curves) do
          FCADSource.FCurveList.SaveCurveToStream(stm,N);
*)
    except
      exit;
    end;
end;

function TCustomCAD.LastCurve: TCurve;
begin
  Result := CADSource.FCurveList.LastCurve;
end;

procedure TCustomCAD.LoadGraphFromMemoryStream(stm: TMemoryStream);
var
  GraphData: TNewGraphData;
  N: Integer;
begin
  if stm=nil then Exit;
  try
      Loading := True;
      FCADSource.LoadFromStream(stm);
  except
      Loading := False;
      exit;
  end;
  Loading := False;
  Recall;
end;

procedure TCustomCAD.UndoInit;
begin
  UR.UndoInit;     // Initialize UndoRedo system
  UndoSave;        // Saves this situation
  Changed := False;
end;

procedure TCustomCAD.Undo;
begin
  if (not Locked) and UR.Enable then begin
    Loading := True;
    FCADSource.FCurveList.Clear;
    UR.Undo;
    SelectedFrame.Visible := false;
    SignedNotCutting;
    Changed := True;
    Loading := False;
    invalidate;
  end;
end;

procedure TCustomCAD.Redo;
begin
  if (not Locked) and UR.Enable then begin
    Loading := True;
    Clear;
    UR.Redo;
    SelectedFrame.Visible := false;
    SignedNotCutting;
    Changed := True;
    Loading := False;
  end;
end;

procedure TCustomCAD.UndoSave;
begin
  if (not Locked) and UR.Enable then
    UR.UndoSave;  // Felhasználói mentés undo-hoz
end;

procedure TCustomCAD.UndoRedo(Sender: TObject; Undo, Redo: boolean);
begin
  If Assigned(FUndoRedoChangeEvent) then
     FUndoRedoChangeEvent(Self,Undo,Redo);
end;

// aSave = True: akkor beteszi a rajzba a torzított képet
// aSave = False: visszaadja az eredeti kpet
procedure TCustomCAD.DoSelectedFrame(aSave: boolean);
begin
  if SelectedFrame.Visible then
  begin
     Ur.Enable := true;
     if aSave then
     begin
        LoadSelectedFrame;
        UndoSave;
     end else
     begin
        Undo;
     end;
     AutoUndo := true;
     SetAllContour;
     SelectedFrame.Visible := False;
     EnableSelectedFrame := False;
     SignedNotCutting;
  end;
  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCustomCAD.Draw_Curve(Cuv: TCurve; co: TColor);
var i: integer;
    x,y: integer;
    R: TRect2d;
    p: TPoint;

    function IsOverlapedRects(R1,R2: TRect2d): boolean;
    begin
         R1 := CorrectRealRect(R1);
         R2 := CorrectRealRect(R2);
         Result := not ( (R2.x2<R1.x1) or (R2.x1>R1.x2) or
                         (R2.y2<R1.y1) or (R2.y1>R1.x2));
    end;

begin
if Cuv<>nil then begin
//  if IsOverlapedRects(Window,Cuv.BoundsRect) then
  With Canvas do begin
       Pen.Mode  := pmCopy;
       Pen.Color := co;
       Pen.Width := 2;
       For i:=0 to Pred(Cuv.Count) do begin
           p:=WToS(Cuv.GetPoint2d(i));
           If i=0 then
              MoveTo(p.x,p.y)
           else
              LineTo(Round(p.x),Round(p.y))
       end;
       if Cuv.Closed then begin
          p:=WToS(Cuv.GetPoint2d(0));
          LineTo(p.x,p.y)
       end;
  end;
end;
end;

procedure TCustomCAD.DrawNode(p: TPoint2d; m: integer; Fill: boolean; co: TColor
  );
var pp: TPoint;
begin
  With Canvas do begin
       Pen.Color := co;
       Pen.Width := 2;
       Pen.Mode  := pmCopy;
       Brush.Color := co;
       if Fill then
          Brush.Style := bsSolid
       else
          Brush.Style := bsClear;
       pp := WToS(p);
       Rectangle(pp.X-m,pp.Y-m,pp.X+m,pp.Y+m);
  end;
end;

procedure TCustomCAD.DrawSelectedFrame;
Var
   Cuv,dCuv: TCurve;
   i: integer;
   m: integer;
begin
if SelectedFrame<>nil then
if SelectedFrame.Visible then
Try
  m := 4; //SensitiveRadius;

  Cuv := TCurve.Create;
  Cuv.Shape  := dmPolygon;
  Cuv.Closed := true;
  With SelectedFrame do begin
       for I := 0 to 3 do
           Cuv.AddPoint( Nodes[i] );
       Draw_Curve( Cuv, clBlue );

       // Támpontok rajzolása

       // Sarokpontok
       for I := 0 to 3 do
           DrawNode(Nodes[i],m,true,clBlue);
       // Felezőpontok
       for I := 4 to 7 do
           DrawNode(Nodes[i],m,false,clBlue);

       // RC forgatási pont
       DrawNode(RCent,m,true,clRed);
       ShowLine(Canvas,XToS(Nodes[4].x),YToS(Nodes[4].y),XToS(RC.X),YToS(RC.Y));
       DrawNode(RC,m,true,clRed);

       // Draw Curves from DestList
       for i := 0 to Pred(SelectedFrame.DestList.Count) do
       begin
         dCuv := DestList.Items[i];
         dCuv.Selected := false;
         Draw_Curve(dCuv,clMaroon);
       end;

  end;
finally
  Cuv.Free;
end;
end;

procedure TCustomCAD.LoadSelectedFrame;
var i: integer;
    Cuv: TCurve;
begin
  if SelectedFrame.Visible then
  begin
    for I := 0 to Pred(SelectedFrame.DestList.Count) do
    begin
      Cuv := TCurve.Create;
      Cuv := SelectedFrame.DestList.Items[i];
      FCADSource.FCurveList.Add(Cuv);
    end;
    invalidate;
    AutoUndo := True;
  end;
end;

procedure TCustomCAD.AdjustSelectedFrame;
begin

end;

function TCustomCAD.IsNode(p: TPoint2d; Radius: double; var idx: integer
  ): boolean;
var i: integer;
    d: double;
    re: TRect2d;
begin
  // Radius : graviti radius
  // idx : 0..3 nodes, 4..7: midpoints, 8: RC, 9: RCent
Try
  Result := False;
  idx := -1;
  re := Rect2d(p.x-Radius,p.y+Radius,p.x+Radius,p.y-Radius);
  for i := 0 to 9 do begin
      if PontInKep(SelectedFrame.Nodes[I].X,SelectedFrame.Nodes[I].Y,re) then
      begin
        idx := i;
        Result := true;
        Break;
      end;
  end;
except

End;
end;

procedure TCustomCAD.SetContour(AIndex: Integer; Radius: double);
var isDir: boolean;
    idx,N  : integer;
begin
if Curves[AIndex].Closed then begin
   Curves[AIndex].SetContour(Radius);
   Curves[AIndex].FContour.GetNearestPoint( Curves[AIndex].Points[0],idx );
   Curves[AIndex].FContour.SetBeginPoint( idx );
//   Curves[AIndex].FContour.InversPointOrder;
end;
end;

procedure TCustomCAD.SetAllContour;
var i: integer;
begin
   For i:=0 to Pred(FCADSource.FCurvelist.Count) do begin
       if Curves[i].Count>3 then begin
          SetContour(i,FContourRadius);
//          Vektorisation(VecCoeff,Curves[i]);
       end;
   end;
   invalidate;
end;

procedure TCustomCAD.SetAllDirect;
begin
  Cadsource.FcurveList.SetAllDirect;
end;

function TCustomCAD.GetClipperContour(Cuv: TCurve; dist: double): TCurve;
begin
  Cuv.SetContour(ContourRadius);
end;

procedure TCustomCAD.ClipperBool(ClipType: TClipType);
var Clip: TClipper;
    clipI,subjI: TPath;
    solution: TPaths;
    n: integer;
    Cuv: TCurve;
    sIdx,I: Integer;

    procedure AddSolution(solution : TPaths);
    var ii,jj : integer;
        p2  : TPoint2d;
        cCuv: TCurve;
    begin
        for ii := 0 to High(solution) do
        Try
            cCuv := TCurve.Create;
            cCuv.Shape := Cuv.Shape;
            cCuv.Selected := False;
            for jj := 0 to High(solution[ii]) do
            begin
               p2 := Point2d( solution[ii][jj].X/100, solution[ii][jj].Y/100 );
               cCuv.AddPoint(p2);
            end;
            AddCurve(cCuv);
        Finally
        End;
    end;

begin
  if SelectedCount>0 then begin
     PoligonizeAll(0);
     VektorisationAll(0.1);
     sIdx := 0;
  For sIdx := 0 to Pred(FCADSource.FCurveList.Count) do
  if (SelectedCount>0) and (Curves[sIdx]<>nil) then
      if Curves[sIdx].Selected then begin
         Cuv := Curves[sIdx];
  Try
     Cuv.ToPath(clipI,100);

     Clip := TClipper.Create;

     Clip.AddPath(clipI, ptClip, true);
     for I := 0 to Pred(FCADSource.FCurveList.Count) do
       if i<>sIdx then
          if CheckForOverLaps(Cuv, Curves[i]) then begin
            Curves[i].ToPath(subjI,100);
            Clip.AddPath(subjI, ptSubject, true);
            Curves[i].Visible := False;
          end;

     Clip.Execute (ClipType, solution, pftNonZero, pftNonZero);

     if solution<>nil then begin
        AddSolution(solution);
        Curves[FCADSource.Fcurvelist.Count-1].Name := 'Clipper';
        Cuv.Visible := False;
        DeleteInvisibleCurves;
     end;
  Finally
     Clip.Free;
  End;
  end;
     DeleteInvisibleCurves;
     Repaint;
  end;
end;

procedure TCustomCAD.cUnion;
begin
     ClipperBool(ctUnion);
end;

procedure TCustomCAD.cIntersection;
begin
     ClipperBool(ctIntersection);
end;

procedure TCustomCAD.cDifference;
begin
     ClipperBool(ctDifference);
end;

procedure TCustomCAD.cXor;
begin
     ClipperBool(ctXor);
end;

procedure TCustomCAD.ReCall;
begin
  if CADSource<>nil then
  if fEnableRecall then
     CADSource.Recall;
  Repaint;
end;

procedure TCustomCAD.SetActionMode(AValue: TActionMode);
begin
  oldActionMode := fActionMode;
//  DrawMode    := dmNone;
  fActionMode := AValue;
  pFazis      := 0;
  if fActionMode <> amDrawing then FDrawMode := dmNone;
  Case AValue of
    amNone           :
    begin
         Cursor := crDefault;
         SignedAll(False);
    end;
    amInsertPoint    : Cursor := crInsertPoint;
    amDeletePoint    : Cursor := crDeletePoint;
    amNewBeginPoint  : Cursor := crNewBeginPoint;
    amRotateSelected : Cursor := crRotateSelected;
    amSelectFrame    :
       begin
            Cursor := crCross;
       end;
    amManualOrder:
       begin
            Cursor := crUpArrow;
            ShowPoints := False;
            PoligonizeAll(0);
            SignedAll(False);
       end;
  end;

  Case oldActionMode of
    amManualOrder:
       begin
  //       VektorisationAll(0.05);
       end;
  End;
  oldCursor := Cursor;

  if Assigned(fChangeMode) then fChangeMode(Self,AValue,FDrawMode);
  invalidate;
end;

procedure TCustomCAD.AfterPaint(Sender: TObject);
var BR : TCurve;
    R  : TRect2d;
    I: Integer;
begin
  DrawSelectedFrame;
  if ShowBoxes then
  for I := 0 to Pred(Count) do
  if Curves[i].Shape=dmInsert then
  begin
     R := Curves[i].BoundsRect;
     Canvas.Pen.Color := clRed;
     Canvas.Pen.Width := 4;
     BR := TCurve.Create;
     BR.Shape := dmRectangle;
     BR.AddPoint(R.x1,R.y1);
     BR.AddPoint(R.x2,R.y1);
     BR.AddPoint(R.x2,R.y2);
     BR.AddPoint(R.x1,R.y2);
     Draw_Curve(BR,clRed);
     BR.Free;
  end;
  inherited;
end;

function TCustomCAD.GetDisabledCount: integer;
begin

end;

procedure TCustomCAD.SetBackImage(AValue: TBMPObject);
begin
  if FBackImage=AValue then Exit;
  FBackImage:=AValue;
end;

procedure TCustomCAD.SetBoxing(AValue: boolean);
begin
  if FBoxing=AValue then Exit;
  FBoxing:=AValue;
  invalidate;
end;

procedure TCustomCAD.SetContourRadius(AValue: double);
var i: integer;
begin
  FContourRadius:=AValue;
  if fCADSource<>nil then
  for i:=0 to Pred(FCADSource.FCurveList.Count) do
      FCADSource.FCurveList.Curves[i].ContourRadius:=AValue;
  invalidate;
end;

procedure TCustomCAD.SetDefaultLayer(AValue: Byte);
begin
  if fDefaultLayer=AValue then Exit;
  fDefaultLayer:=AValue;
end;

procedure TCustomCAD.SetDesigneMode(AValue: boolean);
begin
  FDesigneMode:=AValue;
end;

procedure TCustomCAD.SetDrawMode(AValue: TDrawMode);
begin
  FDrawMode:=AValue;
  if Locked then FDrawMode := dmNone;
  if AValue = dmNone then
     FActionMode := amNone
  else
     FActionMode := amDrawing;
  pFazis    := 0;
  MaxPointsCount := High(integer);
  Case AValue of
       dmNone     : Cursor := crDefault;
       dmPolyline : Cursor := crPolyline;
       dmPolygon  : Cursor := crPolygon;
       dmPoint    : MaxPointsCount := 1;
       dmLine     : MaxPointsCount := 2;
       dmCircle,dmEllipse:
         begin
              MaxPointsCount := 2;
              Cursor := crCircle;
         end;
       dmArc      :
         begin
              MaxPointsCount := 3;
              Cursor := crArc;
         end;
       dmRectangle:
         begin
              MaxPointsCount := 4;
              Cursor := crRectangle;
         end;
       dmText :
         begin
              pFazis    := 0;
              MaxPointsCount := 1;
         end;
       dmFreeHand : Cursor := crFreeHand;
  else MaxPointsCount := High(integer);
  end;
  oldCursor := Cursor;
  if Assigned(fChangeMode) then
     fChangeMode(Self,ActionMode,AValue);
  invalidate;
end;

procedure TCustomCAD.SetEnableSelectedFrame(AValue: boolean);
begin
  FEnableSelectedFrame := AValue;
  if (SelectedFrame.Visible) and (not AValue) then
     SelectedFrame.Visible := false;
  if Assigned(fOnSelectedFrame) then fOnSelectedFrame(Self);
  if AValue then Cursor := crCross
  else Cursor := crDefault;
end;

procedure TCustomCAD.SetLoading(AValue: boolean);
begin
  if FLoading=AValue then Exit;
  FLoading:=AValue;
end;

procedure TCustomCAD.SetLocked(AValue: boolean);
begin
  if fLocked=AValue then Exit;
  fLocked:=AValue;
end;

procedure TCustomCAD.SetOrtho(AValue: boolean);
begin
  if FOrtho=AValue then Exit;
  FOrtho:=AValue;
  SelectedFrame.Ortho := AValue;
end;

procedure TCustomCAD.SetpClosed(AValue: TPen);
begin
  if fpClosed=AValue then Exit;
  fpClosed:=AValue;
end;

procedure TCustomCAD.SetpCrossed(AValue: TPen);
begin
  if fpCrossed=AValue then Exit;
  fpCrossed:=AValue;
end;

procedure TCustomCAD.SetpFazis(AValue: integer);
begin
  if fpFazis=AValue then Exit;
  fpFazis:=AValue;
end;

procedure TCustomCAD.SetpOpened(AValue: TPen);
begin
  if fpOpened=AValue then Exit;
  fpOpened:=AValue;
end;

procedure TCustomCAD.SetpSelected(AValue: TPen);
begin
  if fpSelected=AValue then Exit;
  fpSelected:=AValue;
end;

procedure TCustomCAD.SetpSigned(AValue: TPen);
begin
  if fpSigned=AValue then Exit;
  fpSigned:=AValue;
end;

procedure TCustomCAD.SetpSorted(AValue: TPen);
begin
  if fpSorted=AValue then Exit;
  fpSorted:=AValue;
end;

procedure TCustomCAD.SetSablonSzinkron(AValue: boolean);
begin
  if FSablonSzinkron=AValue then Exit;
  FSablonSzinkron:=AValue;
end;

procedure TCustomCAD.SetSensitiveRadius(AValue: integer);
begin
  FSensitiveRadius:=AValue;
  Delta := AValue;
  If AValue<4 then Delta := 4;
end;

procedure TCustomCAD.SetShowNumbers(AValue: boolean);
begin
  if fShowNumbers=AValue then Exit;
  fShowNumbers:=AValue;
end;

procedure TCustomCAD.SetSTOP(AValue: boolean);
begin
  FSTOP:=AValue;
end;

procedure TCustomCAD.SetTitleFont(AValue: TFont);
begin
  if FTitleFont=AValue then Exit;
  FTitleFont:=AValue;
end;

procedure TCustomCAD.SetVisibleContours(AValue: Boolean);
begin
  if FVisibleContours=AValue then Exit;
  FVisibleContours:=AValue;
  Visible_Contours:=AValue;
  SetAllContour;
end;

procedure TCustomCAD.SetWorkArea(AValue: TRect);
begin
  if FWorkArea=AValue then Exit;
  FWorkArea:=AValue;
end;

procedure TCustomCAD.SetWorking(const Value: boolean);
begin
  fWorking := Value;
  Paint;
end;

procedure TCustomCAD.KeyDown(var Key: Word; Shift: TShiftState);
var dx,dy: integer;
    k:integer;
begin
  k:=16;
  dx := 0; dy:=0;
  Case Key of
    VK_ESCAPE  : begin
                   UndoStop;
                   STOP := True;
                   ActionMode := amNone;
                   DrawMode   := dmNone;
                   SelectAll(False);
                   UndoStart;
                   if SelectedFrame.Visible then begin
                      DoSelectedFrame(false);
                  end;
                 end;
    VK_RETURN,190  :
                 if SelectedFrame.Visible then begin
                    DoSelectedFrame(true);
                 end else
                    ZoomDrawing;

    VK_DELETE : DeleteSelectedCurves;

    18: // Alt
                if EnableSelectedFrame then Cursor := crCross;
    VK_ADD     :
                 if SelectedFrame.Visible then
                 begin
                    SelectedFrame.Magnify(1.01);
                    repaint;
                 end else
                 begin
                      FCentralisZoom := True;
                      Zoom:=1.1*Zoom;
                      FCentralisZoom := False;
                 end;
    VK_SUBTRACT,189:
                 if SelectedFrame.Visible then
                 begin
                    SelectedFrame.Magnify(0.99);
                    repaint;
                 end else
                 begin
                      FCentralisZoom := True;
                      Zoom:=0.9*Zoom;
                      FCentralisZoom := False;
                 end;
    //VK_F4      : ShowPoints := not ShowPoints;
(*
    VK_LEFT    : dx:=k;
    VK_RIGHT   : dx:=-k;
    VK_UP      : dy:=-k;
    VK_DOWN    : dy:=k;*)
  End;
//  if (dx<>0) or (dy<>0) then MoveWindow(dx,dy);
  inherited;
end;

procedure TCustomCAD.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var xx,yy : TFloat;    // Mouse world coordinates
    pr    : TPointRec;
    s     : Str32;
    sel   : boolean;
    RR    : extended;    // Radius for magnify
    InputString : string;
    pp    : TPoint2d;
    p     : TPoint;
    sl    : string;
    szog  : double;
    MouseCoor: TPoint;
    cuvIdx,ptIdx: integer;
begin

  if (Shift = [ssMiddle]) {or (Shift = [ssCtrl,ssLeft])} then begin
     Paning := True;
     oldCursor := Cursor;
     Cursor := crKez1;
     Repaint;
     exit;
  end;

  if (ActionMode=amDrawing) and Grid.aligne then
  begin
     x := XToS(Round(XToW(x)));
     y := YToS(Round(YToW(y)));
  end;
  xx := origo.x + x / Zoom;
  yy := origo.y + (Height-y) / Zoom;
  MapXY := SToW(Point(x,y));
  Origin := Point(x,y);
  MovePt := Point(x,y);
  Mouse_Pos := Origin;
  If pFazis=0 then oldOrigin := Origin;


  if Gravity then
    if CPMatch then
    begin
       pp := Curves[CPCurve].GetPoint2d(CPIndex);
       xx := pp.X;
       yy := pp.y;
    end;

  if SelectedFrame.Visible then
  begin
       if (Shift = [ssCtrl,ssLeft]) or (Shift = [ssLeft]) then begin
          if IsNode(SToW(MovePt),SensitiveRadius/Zoom,SelectedFrame.ActualNode) then
          Case SelectedFrame.ActualNode of
          -1   : Cursor := crCross;
          0..3 : Cursor := crHandpoint;
          4..7 : Cursor := crSize;
          8    : Cursor := crRotateSelected;
          end;
       End;
  End

  else

  begin

  if (DrawMode<>dmNone) then begin

  if (ssLeft in Shift) then begin

     Case FDrawMode of

     dmNone :
       if (ActionMode in [amSelectArea,amSelectAreaEx]) then
       Case pFazis of
       0: begin
          Canvas.Pen.Style:=psDash;
          Canvas.Rectangle(Origin.x,Origin.y,Origin.x,Origin.y);
          end;
       1: pFazis := -1;
       end;

     dmFreeHand :
     Case pFazis of
     0: h:=MakeCurve('Drawing',-1,dmPolyline,True,True,False);
     else begin
         FCurve := FCADSource.FCurveList.Items[h];
         Selected := FCurve;
         pFazis := -1;
     end;
     end;

     dmPoint :
     Case pFazis of
     0: h:=MakeCurve('Point',-1,DrawMode,True,True,False);
     1: pFazis := -1;
     end;

     dmLine, dmArrow :
     Case pFazis of
     0: h:=MakeCurve('Line',-1,DrawMode,True,True,False);
     1: begin
           AddPoint(H,xx,yy);
           pFazis := -1;
        end;
     end;

     dmRectangle :
     Case pFazis of
     0: h:=MakeCurve('Rectangle',-1,DrawMode,True,True,True);
     1: begin
          FCurve := FCADSource.FCurveList.Items[h];
          FCurve.GetPoint(0,pr.x,pr.y);
          FCurve.ClearPoints;
          if (ssCtrl in Shift) or Ortho then // négyzetté deformálás
          begin
               FCurve.AddPoint(pr.x,pr.y);
               FCurve.AddPoint(xx,pr.y);
               FCurve.AddPoint(xx,pr.y-(xx-pr.x));
               FCurve.AddPoint(pr.x,pr.y-(xx-pr.x));
          end else
          begin
               FCurve.AddPoint(pr.x,pr.y);
               FCurve.AddPoint(xx,pr.y);
               FCurve.AddPoint(xx,yy);
               FCurve.AddPoint(pr.x,yy);
          end;
          pFazis := -1;
        end;
     end;

  dmRotRectangle:
      Case pFazis of
      0: begin
         h:=MakeCurve('rotRectangle',-1,DrawMode,True,True,True);
         tegla.a := MapXY; tegla.b := MapXY; tegla.c := MapXY; tegla.d := MapXY;
         end;
      1: begin
          FCurve := Curves[h];
          FCurve.ClearPoints;
          tegla.b := MapXY;
          FCurve.AddPoint(tegla.a.X,tegla.a.Y);
          FCurve.AddPoint(tegla.b.X,tegla.b.Y);
         end;
      2: begin
          tegla := HaromPontbolTeglalap(tegla.a,tegla.b,MapXY);
          FCurve.ClearPoints;
          FCurve.AddPoint(tegla.a.X,tegla.a.Y);
          FCurve.AddPoint(tegla.b.X,tegla.b.Y);
          FCurve.AddPoint(tegla.c.X,tegla.c.Y);
          FCurve.AddPoint(tegla.d.X,tegla.d.Y);
          pFazis := -1;
         end;
      end;

     dmPolyLine  :
     Case pFazis of
     0: h:=MakeCurve('PolyLine',-1,DrawMode,True,True,False);
     end;

     dmPolygon  :
     Case pFazis of
     0:
     begin
       h:=MakeCurve('Polygon',-1,DrawMode,True,True,True);
       polygonContinue := True;
     end;
     end;

     dmCircle    :
     Case pFazis of
     0: h:=MakeCurve('Circle',-1,DrawMode,True,True,True);
     1: begin
        if AutoPoligonize then
           Poligonize( FCADSource.FCurvelist.Items[h],0);
        pFazis := -1;
        end;
     end;

     dmEllipse   :
     Case pFazis of
     0: h:=MakeCurve('Ellipse',-1,DrawMode,True,True,True);
     1: begin
        if (ssCtrl in Shift) or Ortho then
        begin
           DrawMode:=dmCircle;
           Curves[h].Name := 'Circle'+Copy(Curves[h].Name,AnsiPos('_',Curves[h].Name),10);
           Curves[h].Shape := dmCircle;
        end
        else begin
           DrawMode:=dmEllipse;
           Curves[h].Shape := dmEllipse;
        end;
        if AutoPoligonize then
           Poligonize( FCADSource.FCurvelist.Items[h],0);
        pFazis := -1;
        end;
     end;

     dmArc:
           case pfazis of
           0: h:=MakeCurve('Arc',-1,DrawMode,True,True,False);
           1: begin
              Curves[h].GetPoint(0,pr.x,pr.y);
              pp:=FelezoPont(Point2d(XToS(pr.x),YToS(pr.y)),Point2d(x,y));
              MovePt:=ClientToScreen(Point(Trunc(pp.x),Trunc(pp.y)));
              SetCursorPos(MovePt.x,MovePt.y);
              end;
           2: begin
              ChangePoint(h,1,xx,yy);
              if AutoPoligonize then
                 Poligonize( FCADSource.FCurvelist.Items[h],0);
              pfazis:=-1;
              end;
           end;

     dmSpline:
     Case pFazis of
     0:
     begin
       h:=MakeCurve('Spline',-1,DrawMode,True,True,False);
       polygonContinue := True;
     end;
     end;

     dmBSpline:
     Case pFazis of
     0:
     begin
       h:=MakeCurve('BSpline',-1,DrawMode,True,True,True);
       polygonContinue := True;
     end;
     end;

     dmText:
     Case pFazis of
     0:
     begin
       H := MakeCurve('Text',-1,dmText,True,True,False);
       Curves[H].AddPoint(MapXY);
       Curves[H].Text := StrCurve.Text;
       Curves[H].Font := StrCurve.Font;
       Curves[H].FontHeight := StrCurve.FontHeight;
       pFazis := 0;
     end;
     end;

    end;

     // Adott hosszúságú szakaszok rajzolása

     if pFazis>-1 then begin
        pp := Curves[h].GetPoint2d(Curves[h].Count-1);

        if (XYCompulsion or Ortho) and (pFazis>0) then      // XY kényszer
             if Abs(pp.X-xx)>Abs(pp.y-yy) then
                yy := pp.y
             else
                xx := pp.x;

        if FDefiniedLength and (pFazis>0) then
        if (XYCompulsion or Ortho) then
        begin
           if pp.Y=yy then
              if pp.X>xx then
                 xx:=pp.X-defLength else xx:=pp.X+defLength;
           if pp.X=xx then
              if pp.Y>yy then
                 yy:=pp.y-defLength else yy:=pp.y+defLength;
           szog := RelAngle2d(pp,Point2d(xx,yy));
           Move2d(pp,szog,defLength);
        end
        else
        begin
          szog := RelAngle2d(pp,SToW(MovePt));
          Move2d(pp,szog,defLength);
          xx := pp.X; yy := pp.y;
        end;

        if DrawMode<>dmText then
           AddPoint(h,xx,yy);

        if Curves[h].Shape in [dmLine,dmPolygon,dmPolyLine] then
        if FDefiniedLength then
        begin
        p := ClientToScreen(Point(x,y));
        sl := FloatToStr(defLength);
(*        if InputQuery(p.x+10,p.y-10,'Vector Length','Length [mm] :',true,sl)
          then
               defLength := strtofloat(sl)
          else *)
               pFazis := -1;
        end;

     end;

  end;
  end
  else
     begin

       // Choice selected curve
       if (ActionMode = amNone) and (CurveMatch or CPMatch or CurveIn) then
       begin

          Selected := Curves[CPCurve];
          SelectedIndex := CPCurve;

          if (Shift = [ssAlt,ssCtrl,ssLeft]) or (Shift = [ssLeft]) then begin
             if CPMatch and ShowPoints Then begin
                ActionMode := amNone;
                pr := Curves[CPCurve].PointRec[CPIndex];
                if (Shift = [ssLeft]) then
                   ActionMode := amMovePoint
                else
                   Curves[CPCurve].ChangePoint(CPIndex,pr.x,pr.y,not pr.Selected);
             end;

             if CurveMatch or CurveIn or (CPMatch and not ShowPoints) then
             if (Shift = [ssLeft]) then
             begin
                   SelectAll(false);
                   Curves[CPCurve].Selected := True;
                   if Assigned(fChangeSelected) then fChangeSelected(Self,Curves[CPCurve],CPIndex);
             end;

          end else
          begin
              // Csoportos kijelölés Shift-el
              if Shift = [ssShift,ssLeft]
              then begin
                   sel := Curves[CPCurve].Selected;
                   if CurveMatch then ActionMode := amMoveSelected;
                      Curves[CPCurve].Selected := not sel;
                      ActionMode := amMoveSelected;
                   if Assigned(fChangeSelected) then fChangeSelected(Self,Curves[CPCurve],CPIndex);
              end;
          end;

       end;
(*
       else
       if ActionMode <> amNone then
       begin
          Selected := nil;
          // Ha van kijelölt objektum, akkor a kijelölést törli
          if Shift = [ssLeft] then
             SelectAll(False);
       end;
*)

       // Insert point
       if (ActionMode = amInsertPoint) and CurveMatch then
       begin
          Selected := Curves[CPCurve];
          InsertPoint(CPCurve,Selected.CPIndex,xx,yy);
       end;
       // Delete Point
       if (ActionMode = amDeletePoint) and CPMatch then
       begin
          FCurve := FCADSource.FCurveList.Items[CPCurve];
          DeletePoint(CPCurve,CPIndex);
       end;
       // Select Curve
       if (ActionMode = amSelect) and (CurveMatch or CPMatch) then
       begin
          FCurve := FCADSource.FCurveList.Items[CPCurve];
          sel := FCurve.Selected;
          SelectedIndex := CPCurve;
          if CPMatch then TPointRec(FCurve.FPoints.Items[CPIndex]^).Selected := True
          else
              FCurve.Selected := not Sel;
          Selected := FCurve;
       end;

       if (ActionMode = amMagnifySelected) then
       if Button=mbLeft then
       Case pFazis of
       0 : begin
             RotCentrum    := Point2d(xx,yy);
           end;
       1:  begin
           (*
             RR := Trunc(Sqrt(Sqr(xx-RotCentrum.x)+Sqr(yy-RotCentrum.y)));
             if yy<RotCentrum.y then RR := 1/RR;
             MagnifySelected(RotCentrum,RR/100);
             pFazis := -1;
           *)
           end;
       end;

       if (ActionMode = amRotateSelected) then
       if Button=mbLeft then
       Case pFazis of
       0 : begin
                RotCentrum    := Point2d(xx,yy);
                RotStartAngle := 0;
                RotAngle      := 0;
                InputString:= InputBox('Rotate Selected Curves', 'Rotate Angle : ','0');
                RotAngle := -StrToFloat(InputString);
                if RotAngle<>0 then
                   RotateSelectedCurves(RotCentrum,RotAngle*pi/180);
                pFazis := 1;
                ActionMode := amNone;
           end;
       1:  begin
                RotAngle := RelAngle2d(RotCentrum,Point2d(xx,yy));
                pFazis := 0;
           end;
       end;

       if (ActionMode = amNewBeginPoint) then
          if CPMatch then SetBeginPoint(CPCurve,CPIndex);

       if (ActionMode = amOutherBegin) then
          SetOutherBeginPoint(xx,yy);

     if (ActionMode = amManualOrder) then
     if (CurveMatch or CPMatch or CurveIn) then
     begin
        if Curves[CPCurve].Signed then
        begin
          Curves[CPCurve].Signed := False;
          pFazis := pFazis-1;
        end
        else
        begin
          Curves[CPCurve].Signed := True;
          if Curves[CPCurve].Shape=dmPolygon then
          begin
            pp := Curves[pFazis-1].LastPoint;
            Curves[CPCurve].SetBeginPoint(Curves[CPCurve].GetNearestPoint(pp));
          end;
          FCADSource.FCurveList.Move(CPCurve,pFazis);
//          FSelectedIndex := pFazis;
        end;
        if Assigned(fChangeSelected) then fChangeSelected(Self,Curves[CPCurve],CPIndex);
        if pFazis=FCADSource.FCurveList.Count-1 then
        begin
           ActionMode := amNone;
           DrawMode := dmNone;
        end;
     end;

     end;

       if BlockMatch and not (CurveMatch or CPMatch) then
       begin
          if (Shift = [ssLeft]) then
             SelectAll(false);
          Selected := Curves[CPBlock];
          SelectedIndex := CPBlock;
          Curves[CPBlock].Selected := True;
          if Assigned(fChangeSelected) then fChangeSelected(Self,Curves[CPBlock],CPIndex);
       end;

  end;


  oldMovePt := Origin;
  if pFazis=-1 then
     Recall;
  if Button<>mbRight then begin
     pFazis := pFazis+1;
     MouseOn := True;
  end else begin
     if DrawMode=dmNone  then
        SelectAll(False);
     MouseOn := False;
  end;
  Repaint;
  CheckCurvePoints(X,Y);

end;

procedure TCustomCAD.MouseMove(Shift: TShiftState; X, Y: Integer);
Var xx,yy: TFloat;
  pr: TPointRec;
  Hintstr: string;
  HintRect: TRect;
  p: TPoint;
  w,he: integer;
  nyil: boolean;
  szog: double;
  szorzo: double;
  ap: integer;
  pp: TPoint2d;
  dx,dy: TFloat;
  MouseCoor: TPoint;
  FCurve : TCurve;
  d: double;
  cur : TCursor;
label 111;
begin
  MouseOn := Shift <> [ssRight];

if not (ActionMode in [amInsertPoint,amMovePoint,amMoveSelected]) then
  CheckCurvePoints(X,Y);

  if Grid.aligne then
  begin
     x := XToS(Round(XToW(x)));
     y := YToS(Round(YToW(y)));
  end;

  MovePt := Point(x,y);
  Mouse_Pos := MovePt;
  MouseOn := Shift <> [ssRight];

  xx := origo.x + x / Zoom;
  yy := origo.y + (Height-y) / Zoom;

  if CPMatch and Gravity then
  begin
       pp := Curves[CPCurve].GetPoint2d(CPIndex);
       xx := pp.X;
       yy := pp.y;
  end;

  case Grid.Metric of
  meMM : szorzo:=1.0;
  meInch : szorzo:=1/inch;
  else
    szorzo:=1.0;
  end;
  MapPoint := Point2d(xx*szorzo,yy*szorzo);

  If (CoordLabel<>nil) then begin
     CoordLabel.Caption:=Trim(Format('%6.2f : %6.2f',[xx*szorzo,yy*szorzo]));
     CoordLabel.Repaint;
  end;

  if (Shift = [ssMiddle]) or (Shift = [ssCtrl,ssLeft]) then begin
     MoveWindow(x-oldMovePt.x,-(y-oldMovePt.y));
     Paning := True;
     oldMovePt:=MovePt;
     Cursor := crKez2;
     Repaint;
     exit;
  end;

  If (DrawMode = dmNone) and (ActionMode in [amNone,amManualOrder]) then begin
     if (CPMatch and ShowPoints) then begin
        Cursor:=crHandPoint;
     end else
     if CurveMatch then Cursor:=crDrag else
     if BlockMatch then Cursor:=crSizeAll else
     if CurveIn then Cursor:=crMultiDrag else
     if ActionMode=amManualOrder then
        Cursor := crUpArrow
     else
        Cursor := crDefault;
  end;

  // Automatic Move window, if cursor is near the border of window
  if (ActionMode<>amNone) and (pFazis>0) then begin
     if x<20 then // Cursor := crNyilLeft;
     MoveWindow(4,0);
     if (Width-x)<20 then // Cursor := crNyilRight;
     MoveWindow(-4,0);
     if y<20 then // Cursor := crNyilUp;
     MoveWindow(0,-4);
     if (Height-y)<20 then // Cursor := crNyilDown;
     MoveWindow(0,4);
  end;

  if EnableSelectedFrame and (Shift = [ssAlt]) then
     Cursor := crCross;

  if SelectedFrame.Visible then
  begin
       if SelectedFrame.IsInPoint(Point2d(xx,yy)) then
          Cursor := crSizeAll
       else
          Cursor := crDefault;
       if (Shift = []) or (Shift = [ssCtrl]) then
          if IsNode(SToW(MovePt),SensitiveRadius/Zoom,ap) then
          Case ap of
          -1     : Cursor := crCross;
          0..3   : Cursor := crHandpoint;
          4..7,9 : Cursor := crHandpoint;
          8      : Cursor := crRotateSelected;
          end;
  End;

  if (ActionMode = amSelectFrame) and (Shift = [ssLeft])
          then begin
             Canvas.Pen.Style:=psSolid;
             Canvas.Brush.Style := bsSolid;
             Canvas.Brush.Color := clSilver;
             Canvas.Pen.Mode:=pmNotXor;
             Canvas.Rectangle(Origin.x,Origin.y,oldMovePt.x,oldMovePt.y);
             Canvas.Rectangle(Origin.x,Origin.y,MovePt.x,MovePt.y);
             Canvas.Pen.Style:=psSolid;
          end;

  if (ssLeft in Shift) then begin
     Case ActionMode of
     amNone,amMovePoints :
       begin

          if (Shift = [ssShift,ssLeft]) or (Shift = [ssCtrl,ssLeft]) or (Shift = [ssLeft]) then begin
//             Moving := True;
             if Ortho then begin
               dx := x-oldMovePt.x;
               dy := y-oldMovePt.y;
               if Abs(dx)>Abs(dy) then
                  MoveSelectedCurves(dx,0)
               else
                  MoveSelectedCurves(0,-(y-oldMovePt.y))
             end
             else
             MoveSelectedCurves(x-oldMovePt.x,-(y-oldMovePt.y));
          end;

          if Shift = [ssShift,ssCtrl,ssLeft] then
             if CurveMatch or CurveIn then begin
                MoveCurve(CPCurve,x-oldMovePt.x,-(y-oldMovePt.y));
//                Moving := True;
             end;

          if (Shift = [ssAlt,ssLeft]) or (Shift = [ssCtrl,ssAlt,ssLeft])
             or (ActionMode = amSelectFrame)
          then begin
             Canvas.Pen.Style:=psSolid;
             Canvas.Brush.Style := bsSolid;
             Canvas.Brush.Color := clSilver;
             Canvas.Pen.Mode:=pmNotXor;
             Canvas.Rectangle(Origin.x,Origin.y,oldMovePt.x,oldMovePt.y);
             Canvas.Rectangle(Origin.x,Origin.y,MovePt.x,MovePt.y);
             Canvas.Pen.Style:=psSolid;
          end;

          if (Shift = [ssCtrl,ssLeft]) or (Shift = [ssLeft]) then
          begin
             if SelectedFrame.Visible then
             begin
                case  SelectedFrame.ActualNode of
                -1,9: SelectedFrame.Move(XToW(x)-XToW(oldMovePt.x),YToW(Height-oldMovePt.y)-YToW(Height-y));
                0..3: if Ortho then
                      SelectedFrame.MoveEdge( SelectedFrame.ActualNode,
                          XToW(x)-XToW(oldMovePt.x),YToW(Height-oldMovePt.y)-YToW(Height-y) )
                      else
                      SelectedFrame.SetNode(SelectedFrame.ActualNode,SToW(MovePt));
                4..7: SelectedFrame.MoveEdge( SelectedFrame.ActualNode,
                          XToW(x)-XToW(oldMovePt.x),YToW(Height-oldMovePt.y)-YToW(Height-y) );
                8: with SelectedFrame do
                      RotAngle := -90+RadToDeg(Angle2D( Point2d(xx-RCent.X,yy-RCent.y) ));
                end;
                Paint;
             end;
          end;

       end;

     amMovePoint      : DoMove(x,y);
     amMoveSelected   : MoveCurve(CPCurve,x-oldMovePt.x,-(y-oldMovePt.y));
     amInsertPoint    : DoMove(x,y);
     amRotateSelected :
         if (pFazis>1) then begin
          RotAngle := RelAngle2d(RotCentrum,Point2d(xx,yy));
          szog := -Szogdiff(RotAngle,RelAngle2d(RotCentrum,SToW(oldMovePt.x,oldMovePt.y)));
          RotateSelectedCurves(RotCentrum,szog);
          pFazis := pFazis + 1;
         end;
       amSelectArea,amSelectAreaEx :
         begin
         Canvas.Pen.Style:=psDash;
         Canvas.Pen.Mode:=pmNotXor;
         Canvas.Rectangle(Origin.x,Origin.y,oldMovePt.x,oldMovePt.y);
         Canvas.Rectangle(Origin.x,Origin.y,MovePt.x,MovePt.y);
         Canvas.Pen.Style:=psSolid;
         end;
     end;
     goto 111;
  end;

  // Searching point, line or inside area
  CheckCurvePoints(X,Y);

  // Draw a shape
  if Shift <> [ssRight] then
  if (DrawMode<>dmNone) and (pFazis>0) {and (not Zooming)} then begin
     FCurve := FCADSource.FCurveList.Items[h];
     if MaxPointsCount>=FCurve.FPoints.Count then begin
        Case DrawMode of

        dmRectangle :
        begin
             FCurve.GetPoint(0,pr.x,pr.y);
             FCurve.ClearPoints;
          if (ssCtrl in Shift) or Ortho then // négyzetté deformálás
          begin
               FCurve.AddPoint(pr.x,pr.y);
               FCurve.AddPoint(xx,pr.y);
               FCurve.AddPoint(xx,pr.y-(xx-pr.x));
               FCurve.AddPoint(pr.x,pr.y-(xx-pr.x));
          end else
          begin
               FCurve.AddPoint(pr.x,pr.y);
               FCurve.AddPoint(xx,pr.y);
               FCurve.AddPoint(xx,yy);
               FCurve.AddPoint(pr.x,yy);
          end;
          Paint;
        end;

        dmRotRectangle:
        begin
          FCurve.ClearPoints;
          case pFazis of
          1: begin
               tegla.b := MapXY;
               FCurve.AddPoint(tegla.a.X,tegla.a.Y);
               FCurve.AddPoint(tegla.b.X,tegla.b.Y);
             end;
          2: begin
               tegla := HaromPontbolTeglalap(tegla.a,tegla.b,MapXY);
               FCurve.AddPoint(tegla.a.X,tegla.a.Y);
               FCurve.AddPoint(tegla.b.X,tegla.b.Y);
               FCurve.AddPoint(tegla.c.X,tegla.c.Y);
               FCurve.AddPoint(tegla.d.X,tegla.d.Y);
             end;
          end;
          Paint;
        end;

        dmArc :
        begin
          if FCurve.FPoints.Count=1 then
             FCurve.AddPoint(xx,yy)
          else
             ChangePoint(h,1,xx,yy);
          Paint;
        end;

        dmCircle, dmEllipse:
        begin
          if (ssCtrl in Shift) or Ortho then
             FCurve.Shape := dmCircle
          else
             FCurve.Shape := dmEllipse;
          if FCurve.FPoints.Count=1 then
             AddPoint(H,xx,yy)
          else
             ChangePoint(h,1,xx,yy);
          paint;
        end;

        dmFreeHand :
        begin
          FCurve.AddPoint(xx,yy);
          Paint;
        end;
        else begin

          pp := Curves[h].GetPoint2d(Curves[h].Count-1);

          XYCompulsion := Shift = [ssCtrl];
          if (XYCompulsion or Ortho) and (pFazis>0) then      // XY kényszer
          begin
             if Abs(pp.X-xx)>Abs(pp.y-yy) then
                yy := pp.y
             else
                xx := pp.x;
          end;
          if FDefiniedLength then
          begin
               if (XYCompulsion or Ortho) then      // XY kényszer
               begin
                    if (pFazis>0) then begin
                       if pp.Y=yy then
                          if pp.X>xx then
                             xx:=pp.X-defLength else xx:=pp.X+defLength;
                       if pp.X=xx then
                          if pp.Y>yy then
                             yy:=pp.y-defLength else yy:=pp.y+defLength;
                       szog := RelAngle2d(pp,Point2d(xx,yy));
                       Move2d(pp,szog,defLength);
                    end;
               end
               else
               begin
                    szog := RelAngle2d(pp,SToW(MovePt));
                    Move2d(pp,szog,defLength);
               end;
               xx := pp.X; yy := pp.y;
          end;

          AddPoint(H,xx,yy);
          Recall;
          RePaint;
          if FCurve<>nil then
          if (DrawMode<>dmFreeHand) then
               FCADSource.FCurveList.DeletePoint(H,Pred(FCurve.FPoints.Count));
       end;
        end;
     end else pFazis:=0;
  end;

  if DrawMode = dmText then
  begin
     StrCurve.Points[0] := MapPoint;
     Paint;
     DrawCurve(Canvas,StrCurve,1,0,0,0)
  end;

  {Hint ablak rajzolása}
  Try

  Hint_Label.Visible := fCoordHint;
  if fCoordHint then begin
     Hint_Label.Transparent := false;
     Hint_Label.Color := clWhite;
     Hint_Label.Left := X+24;
     Hint_Label.Top  := Y+24;
     pp := Curves[h].Points[0];
     Hint_Label.Caption := Format('x:y = ( %6.1f : %6.1f )',[xx,yy]);
     if pFazis>0 then
     Case DrawMode of
     dmPolygon, dmPolyline, dmLine:
     begin
        d := KetpontTavolsaga(Curves[h].LastPoint,Point2d(xx,yy));
        Hint_Label.Caption := Format('d = %6.1f',[d]);
     end;
     dmRectangle:
        Hint_Label.Caption := Format('A : B = %6.1f : %6.1f',[Abs(xx-pp.X),Abs(yy-pp.y)]);
     dmCircle:
     begin
        d := 2*KetpontTavolsaga(pp,Point2d(xx,yy));
        Hint_Label.Caption := Format('d = %6.1f',[d]);
     end;
     dmEllipse:
        if Ortho then begin
        d := 2*KetpontTavolsaga(pp,Point2d(xx,yy));
        Hint_Label.Caption := Format('d = %6.1f',[d]);
        end else
        Hint_Label.Caption := Format('A : B = %6.1f : %6.1f',[2*Abs(xx-pp.X),2*Abs(yy-pp.y)]);
     end;
     Hint_Label.Update;
  end;

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

  except
  end;

  if CursorCross then begin
     DrawMouseCross(oldMovePt,pmNotXor);
     DrawMouseCross(MovePt,pmNotXor);
     MouseInout:=0;
  end;


111: oldMovePt := Point(x,y);
  oldCursor := Cursor;
  if ShowPoints and Gravity then
     ShowMagneticCircle(CPx,CPy,CPMatch);
inherited MouseMove(Shift, X, Y);
end;

procedure TCustomCAD.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var pr    : TPointRec;
    R     : TRect2d;
    ap    : integer;
begin
  Mouse_Pos := Point(x,y);
  MouseOn  := False;

  if (Button = mbMiddle) {or (Shift = [ssCtrl,ssLeft])} then begin
     Paning := False;
     oldMovePt:=Mouse_Pos;
     Cursor := oldCursor;
     Repaint;
     exit;
  end;

  if Button = mbRight then
  if AutoPoligonize then
     Case DrawMode of
     dmSpline, dmBSpline:
           Poligonize( Curves[h],0);
     End;

  if (not Zooming) and (Button=mbRight) then begin
     pFazis := 0;
     Recall;
  end
     else invalidate;

  If (not Paning) and (not Zooming) and (not Painting) and (Button<>mbRight) and AutoUndo then
     UndoSave;
  if (ActionMode=amSelectArea) or (Shift = [ssAlt]) then begin
     R := Rect2d(XToW(Origin.x),YToW(Origin.y),XToW(x),YToW(y));
     if EnableSelectedFrame then
     with SelectedFrame do begin
          UndoSave;
          SelectAll(False);
          AutoUndo := false;
          Ur.Enable := false;
          init;
          SelectedFrame.Visible := True;
          OrigRect := R;
          SelectAllInArea(R);
          DeleteSelectedCurves;
          Cursor := crDefault;
     end else
         SelectAllInArea(R);
//     ActionMode:=amNone;
  end;

  if SelectedFrame.Visible and (ActionMode=amNone) then
  begin
     IsNode(SToW(MovePt),SensitiveRadius/Zoom,ap);
     if SelectedFrame.ActualNode=-1 then
     if (Button = mbRight) then
        DoSelectedFrame(false)
     else begin
        if not SelectedFrame.IsInPoint(SToW(x,y)) then begin
           DoSelectedFrame(true);
        end;
     end;
     ReCall;
     ActionMode:=amNone;
  end
  else
  if (ActionMode=amSelectAreaEx) or (ActionMode = amSelectFrame)
     or (Shift = [ssAlt,ssCtrl])
  then begin
     ActionMode:=amMovePoints;
     R := Rect2d(XToW(Origin.x),YToW(Origin.y),XToW(x),YToW(y));
     if ShowPoints then
        SelectAllInAreaEx(R)
     else
        SelectAllInArea(R);
     // Terület kijelölés után a SelecTedFrame megjelenítése
     if GetSelectArea(R) then
     begin
     SelectedFrame.Init;
     SelectedFrame.OrigRect := R;
     SelectedFrame.Visible := True;
     AddSelectedToSelectedFrame;
     UndoSave;
     AutoUndo := false;
     DeleteSelectedCurves;
     Cursor := crDefault;
     end;
  end;

  if (ActionMode in [amMovePoint,amMoveSelected]) then
     ActionMode := amNone;

  if Paning then Cursor := crDefault;
       Paning   := False;
       Zooming  := False;
       Painting := False;
       Moving   := False;

//  if FDrawmode<>dmNone then
//  if ActionMode=amNone then
     SignedNotCutting;
  SelectedFrame.ActualNode:=-1;
  Recall;
end;

function TCustomCAD.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  oldCursor := Cursor;
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  Cursor := oldCursor;
end;

procedure TCustomCAD.UndoStart;
begin
  UR.Enable := AutoUndo;
end;

procedure TCustomCAD.UndoStop;
begin
  UR.Enable := False;
end;

// ------------------------------------------------------------------------

{ TCustomCADView }

constructor TCustomCADView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks, csReplicatable];
  Width  := 200;
  height := 200;
//  FCurveList        := TCurveList.Create(Self);
//  FBlockList        := TBlockList.Create;
  fZoom             := 1;
  FCentralisZoom    := True;
  FCursorCross      := False;
  DrawBmp           := TBitMap.Create;
  fPaper            := TPoint2DObj.Create;
  fPaper.x          := 210;
  fPaper.y          := 297;
  fPaper.OnChange   := Change;
  fBackColor        := clSilver;
  fPaperColor       := clWhite;
  fPaperVisible     := True;
  fGrid             := TGrid.Create;
  fGrid.Margin      := 0;
  fGrid.Visible     := False;
  fGrid.OnChange    := Change;
  fPointWidth       := 3;
  FFilled           := False;
  fCentrum          := TPoint2DObj.Create;
  fCentrum.OnChange := ChangeCentrum;
  fCentrum.x        := fPaper.x / 2;
  fCentrum.y        := fPaper.y / 2;
  fFillBrush        := TBrush.Create;
  fFillBrush.Color  := clYellow;
  fFillBrush.Style  := bsSolid;
  FCadPens          := TCadPens.Create;
  FCadPens.OnChange := Change;
  FVisibleObjects   := True;
  Hinted            := False;
  Hint1             := THintWindow.Create(Self);
  Hint1.parent      := Self;
  Hint1.Canvas.Brush.Color:=clWhite;
  Hint1.Visible     := False;
  Hint_Label        := TLabel.Create(Self);
  Hint_Label.Parent := Self;
  Hint_Label.Name   := 'HintLabel';
  Hint_Label.Caption:= '';
  Hint_Label.Transparent := True;
  Hint_Label.Canvas.Brush.Color  := clWhite;
  Hint_Label.Visible:= False;
  fCoordHint        := False;
  FSelectedIndex    := -1;
  MouseInOut        := 1;
  FContourRadius    := 2.0;
  FVisibleBeginPoint:= False;
  FBackImage          := TBMPobject.Create;
  FBackImage.OnChange := Change;
  Transparent         := False;
  fEnableRecall     := True;
  fEnablePaint      := True;
  DoubleBuffered    := True;
  ZoomPaper;
end;

procedure TCustomCADView.Loaded;
begin
  inherited;
  if CADSource=nil then
  begin
    CADSource := TCADSource.Create(Self);
  end;
end;


destructor TCustomCADView.Destroy;
begin
  if Self <> nil then begin
    Hint1.Free;
    FCadPens.Free;
    fFillBrush.Free;
    fPaper.Free;
    fGrid.Free;
    fCentrum.Free;
    DrawBmp.Free;
    if FBackImage<>nil then
       FBackImage.Free;
//    FBlockList.Free;
  end;
  inherited Destroy;
end;

procedure TCustomCADView.SetBackColor(AValue: TColor);
begin
  if fBackColor=AValue then Exit;
  fBackColor:=AValue;
  invalidate;
end;

procedure TCustomCADView.WMSize(var Msg: TWMSize);
begin
     ChangeCentrum(nil);
     inherited;
end;

function TCustomCADView.GetBlock(idx: integer): TBlock;
begin
  Result := FCADSource.Blocks[idx];
end;

function TCustomCADView.GetBlockCount: integer;
begin
  if FCADSource.FBlockList<>nil then
     Result := FCADSource.FBlockList.Count
  else
     Result := 0;
end;

function TCustomCADView.GetCount: integer;
begin
  Result := GetCurveCount;
end;

procedure TCustomCADView.SetBackImage(AValue: TBMPObject);
begin
  if FBackImage=AValue then Exit;
  FBackImage:=AValue;
  Repaint;
end;

procedure TCustomCADView.SetBlock(idx: integer; const Value: TBlock);
begin
  fCADSource.Blocks[idx] := Value;
end;

procedure TCustomCADView.SetCADSource(AValue: TCADSource);
begin
  fCADSource:=AValue;
  (*
  if fCADSource = nil then
     FCurveList.ClearList
  else begin
    FCurveList.Assign(fCADSource.FCurveList);
  *)
  if fCADSource <> nil then
    fCADSource.OnChange:=Change
end;

{ Enable/Disable reaction for changing of DataSource }
procedure TCustomCADView.SetEnableRecall(AValue: boolean);
begin
  fEnableRecall:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetVisibleBeginPoint(AValue: boolean);
begin
  FVisibleBeginPoint:=AValue;
  invalidate;
end;

procedure TCustomCADView.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  MouseInOut:=1;
  if Hinted then
     Hint_Label.Visible := CoordHint;
  if not Focused then
     SetFocus;
//  DrawMouseCross(oldMovePt,pmNotXor);
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomCADView.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  MouseInOut:=-1;
  If HintActive then begin
     Hint1.ReleaseHandle;
     HintActive := False;
  end;
  Hint_Label.Visible := False;
//  invalidate;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

(*
procedure TCustomCADView.DblClick(Sender: TObject);
begin
  FCentrum.x := MapXY.X;
  Centrum.y := MapXY.Y;
  invalidate;
  inherited DblClick;
end;
*)
procedure TCustomCADView.SetShowPoints(AValue: boolean);
begin
  if fShowPoints=AValue then Exit;
  fShowPoints:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetTransparent(const Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
//  FTransparentSet := True;
  DrawBmp.Transparent := Value;
  DrawBmp.Canvas.CopyMode := cmMergeCopy;

end;

procedure TCustomCADView.SetVisibleObjects(AValue: boolean);
begin
  FVisibleObjects:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetPointWidth(AValue: integer);
begin
  fPointWidth:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetSelected(AValue: TCurve);
begin
  if fSelected=AValue then Exit;
  fSelected:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetSelectedIndex(AValue: integer);
begin
  FSelectedIndex := AValue;
  if AValue<0 then begin
     FSelectedIndex:=-1;
     Selected := nil;
     exit;
  end;
  if FSelectedIndex>Count-1 then FSelectedIndex:=Count-1;
  Selected := Curves[FSelectedIndex];
  invalidate;
end;

procedure TCustomCADView.SetSelectedVisible(AValue: boolean);
begin
  FSelectedVisible:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetAppend(AValue: boolean);
begin
  FAppend:=AValue;
  if FCADSource<>nil then
     FCADSource.Append:=AValue;
end;

procedure TCustomCADView.SetCoordHint(AValue: boolean);
begin
  fCoordHint:=AValue;
  invalidate;
end;

function TCustomCADView.GetCurve(idx: integer): TCurve;
begin
  if ((Idx>-1) and (idx < FCADSource.FCurveList.Count)) then
     Result := FCADSource.FCurveList.Items[idx]
  else
     Result := nil;
end;

function TCustomCADView.GetCurveCount: integer;
begin
  if FCADSource.FCurveList<>nil then
     Result := FCADSource.FCurveList.Count
  else
     Result := 0;
end;

procedure TCustomCADView.SetCurve(idx: integer; AValue: TCurve);
begin
  if ((Idx>-1) and (idx < FCADSource.FCurveList.Count)) then
  begin
     FCADSource.FCurveList.Items[idx] := AValue;
     Curves[idx] := AValue;
  end
  else
     FCADSource.FCurveList.AddCurve(AValue);
  invalidate;
end;

procedure TCustomCADView.SetFilename(AValue: string);
begin
  FFilename:=AValue;
  fCADSource.LoadFromFile(FFilename);
  invalidate;
end;

procedure TCustomCADView.SetFillBrush(AValue: TBrush);
begin
  if fFillBrush=AValue then Exit;
  fFillBrush:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetGraphTitle(AValue: Str32);
begin
  if FGraphTitle=AValue then Exit;
  FGraphTitle:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetLayer(idx: integer; const Value: TLayer);
begin
  FCADSource.Layers[idx]:=Value;
end;

procedure TCustomCADView.SetLayerPaint(const Value: boolean);
begin
  fLayerPaint := Value;
  invalidate;
end;

(*
procedure TCustomCADView.SetLType(idx: integer; const Value: TLineType);
begin
  FCADSource.Linetypes[idx]:=Value;
end;
*)
function TCustomCADView.GetWindow: TRect2d;
begin
    Result := Rect2d(Origo.x,origo.y,XToW(width),YToW(0));
end;

procedure TCustomCADView.doResize(Sender: TObject);
begin
  ChangeCentrum(nil);
end;

procedure TCustomCADView.SetCentralCross(AValue: boolean);
begin
  if fCentralCross=AValue then Exit;
  fCentralCross:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetCursorCross(AValue: boolean);
begin
  fCursorCross:=AValue;
  if fCursorCross then
     DrawMouseCross(MovePt,pmNotXor);
  invalidate;
end;

procedure TCustomCADView.SetEnablePaint(AValue: boolean);
begin
if Self<>nil then begin
  if fEnablePaint=AValue then Exit;
  fEnablePaint:=AValue;
  invalidate;
end;
end;

procedure TCustomCADView.SetFilled(AValue: boolean);
begin
  if FFilled=AValue then Exit;
  FFilled:=AValue;
  Invalidate;
end;

procedure TCustomCADView.SetPaperColor(AValue: TColor);
begin
  if fPaperColor=AValue then Exit;
  fPaperColor:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetPaperVisible(AValue: boolean);
begin
  if FPaperVisible=AValue then Exit;
  FPaperVisible:=AValue;
  invalidate;
end;

procedure TCustomCADView.SetWindow(AValue: TRect2d);
begin
  fOrigo.x := AValue.x1;
  fOrigo.y := AValue.y1;
  Zoom     := 1;
end;

procedure TCustomCADView.SetZoom(AValue: extended);
var felx,fely: extended;
begin
// Paning := True;
 if FCentralisZoom then begin
    felx := Width/(2*Zoom);
    fely := Height/(2*Zoom);
 end else begin
    felx := MovePt.x/(Zoom);
    fely := (Height-MovePt.y)/(Zoom);
 end;
  forigo.x := forigo.x+felx*(1-(fZoom/AValue));
  forigo.y := forigo.y+fely*(1-(fZoom/AValue));
  fZoom := AValue;
  if Assigned(fChangeWindow) then fChangeWindow(Self,FOrigo,FZoom,Mouse_Pos);
// Paning := False;
  Repaint;
end;

procedure TCustomCADView.Change(Sender: TObject);
begin
// if fEnableRecall then begin
  if CadSource<>nil then begin
    if Assigned(FOnReCall) then FOnReCall(Self);
  end;
  Repaint;
// end;
end;

procedure TCustomCADView.ChangeCentrum(Sender: TObject);
var p: TPoint2d;
begin
  Origo := CentToOrigo(Point2d(Centrum.x,Centrum.y));
  Invalidate;
end;

procedure TCustomCADView.ChangePaperExtension(Sender: TObject);
begin
  ZoomPaper;
end;

procedure TCustomCADView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Mouse_Pos := Point(x,y);
  MapXY  := SToW(Point(x,y));
  oldMovePt := Point(x,y);
  if (Shift = [ssMiddle]) or (Shift = [ssCtrl,ssLeft]) then begin
     oldCursor := Cursor;
     Cursor := crKez1;
  end;
end;

procedure TCustomCADView.MouseMove(Shift: TShiftState; X, Y: Integer);
var szorzo: TFloat;
    cStr  : string;
    xx,yy : TFloat;
begin

  if (Shift = [ssMiddle]) or (Shift = [ssCtrl,ssLeft]) then begin
     MoveWindow(x-oldMovePt.x,-(y-oldMovePt.y));
     Paning := True;
     Cursor := crKez2;
  end;

  Mouse_Pos := Point(x,y);
  MovePt := Point(x,y);
  MapXY  := SToW(Point(x,y));
  xx := MapXY.X;
  yy := MapXY.y;

  case Grid.Metric of
  meMM : szorzo:=1.0;
  meInch : szorzo:=1/inch;
  else
    szorzo:=1.0;
  end;
  MapPoint := Point2d(xx*szorzo,yy*szorzo);

  If (CoordLabel<>nil) then begin
     cStr := Format('%6.2f : %6.2f',[MapPoint.x,MapPoint.y]);
     CoordLabel.Caption:=Trim(cStr);
     CoordLabel.Repaint;
  end;

  if CursorCross then begin
     DrawMouseCross(oldMovePt,pmNotXor);
     DrawMouseCross(MovePt,pmNotXor);
     MouseInout:=0;
  end;

  inherited MouseMove(Shift, X, Y);

  oldMovePt := Point(x,y);
end;

procedure TCustomCADView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Paning := False;
  Mouse_Pos := Point(x,y);
  if (Button = mbMiddle) or (Shift = [ssCtrl,ssLeft]) then
  begin
     Cursor := oldCursor;
     Paning := False;
     Repaint;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TCustomCADView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var N: TFloat;
    pv: boolean;
    wd: single;
begin
    pv := ShowPoints;
    if pv then ShowPoints:=False;
//    wd := Abs(WheelDelta/110);
    if WheelDelta<0 then Zoom:=0.9*FZoom
    else Zoom:=1.1*FZoom;
    Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
    ShowPoints := pv;
end;

function TCustomCADView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
end;

function TCustomCADView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
end;


procedure TCustomCADView.GeneratePoinsArray(AIndex: integer; var pArr: TPointArray);
begin
if fCADSource<>nil then
  GeneratePoinsArray( FCADSource.FCurveList.Curves[AIndex],pArr );
end;

procedure TCustomCADView.GeneratePoinsArray(ACurve: TCurve;
  var pArr: TPointArray);
Var i,Size: integer;
    PA    : PPointArray;
    X,Y   : TFloat;
    p     : TPoint;
begin
  Size:=ACurve.Count * SizeOf(TPointArray);
  GetMem(PA,Size);
//  Size:=ACurve.Count * SizeOf(TPoint);
//  GetMem(pArr,Size);
  for i:=0 to Pred(ACurve.Count) do begin
      ACurve.GetPoint(I,X,Y);
      p := WtoS(x,y);
      PA^[I].x:= p.x;
      PA^[I].y:= p.y;
      pArr[i] := PA^[I];
  end;
end;


procedure TCustomCADView.DrawCurve(ca:TCanvas;FC:TCurve;Magnify,Angle,dx,dy:double);
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
  oke     : boolean;
  szog    : double;
begin

  if Visible_Contours then
     II := 1
  else
     II := 0;

   if LayerCount>0 then
      oke := (FC.Visible) and (Layers[FC.Layer].Visible)
   else
      oke := (FC.Visible);
   if oke then
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
         p := WtoS(p2.x,p2.y);
         PA[I].x:= p.x;
         PA[I].y:= p.y;
     end;

     if FL.Count>0 then
     BEGIN
      // Set Pens and Brushes
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
          If FC.Count>2 then
          begin
            FC.FillTempCurve;
            FC.TempCurve.Poligonize(0);
            DrawCurve(ca,FC.TempCurve,1,0,0,0);
            I := fc.Count;
          end;

      dmSpline:
          begin
              if FC.Closed then K:=3 else K:=4;
              SplineXP(ca,PA,36,TBSplineAlgoritm(K));
              Ca.Pen.Color := clRed;
//              ca.PolyBezier(PA);
          end;

      dmBSpline:
          begin
              if FC.Closed then K:=1 else K:=2;
              SplineXP(ca,PA,36,TBSplineAlgoritm(K));
              Ca.Pen.Color := clRed;
//              ca.PolyBezier(PA);
          end;

      dmText :
          begin
            if (FC.FontHeight * Zoom)<-1 then
            begin
                 ca.Font.Assign(FC.Font);
                 ca.Font.Height := Round(FC.Font.Height * Zoom);
                 if FC.BlockParams.Angle = 0.0 then
                    ca.TextOut(PA[0].x,PA[0].y-ca.Font.Height,FC.Text)
                 else begin
                    szog := RadToDeg(FC.BlockParams.Angle);
                    RotText(ca,PA[0].x,PA[0].y-ca.Font.Height,FC.Text,
                            Round(szog));
                 end;

            end;
          end;

      end; // End Case


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
     END;

      if Assigned(fAfterDraw) then fBeforeDraw(Self,DrawBmp.Canvas,nil);

  end;
  SetLength(PA,0);
end;

procedure TCustomCADView.Paint;
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
Try
  Try
  painting := True;
  R := Clientrect;

  beginpaint(DrawBmp.Canvas.Handle,tps );

  DrawBmp.SetSize(Self.Width,Self.Height);

  if not DrawBmp.Transparent then
  begin

  {Clear Canvas}
  DrawBmp.Canvas.Pen.Width:=1;
  DrawBmp.Canvas.Pen.Style := psSolid;
  DrawBmp.Canvas.Pen.Mode:=pmCopy;
  DrawBmp.Canvas.Brush.Color:=FBackColor;
  DrawBmp.Canvas.Brush.Style:=bsSolid;
  DrawBmp.Canvas.Rectangle(R);

  {Draw Paper}
  If {IsPaperInWindow and} PaperVisible then begin
    RP:=Rect(XToS(0),YToS(0),XToS(Paper.x),YToS(Paper.y));
    DrawBmp.Canvas.Brush.Color:=PaperColor;
    DrawBmp.Canvas.FillRect(RP);
    DrawBmp.Canvas.Pen.Color := clBlack;
    DrawBmp.Canvas.Rectangle(RP);
  end;

  end;

  if FBackImage.Visible and (not FBackImage.BMP.Empty) then
  begin
     // Draw the backimage
     RE := FBackImage.BoundsRect;
     R  := Rect(XToS(RE.x1),YToS(RE.y1),XToS(RE.x2),YToS(RE.y2));
     with DrawBmp.Canvas do begin
          Pen.Style := psDash;
          Brush.Style := bsClear;
          Rectangle(R);
          {$ifdef WINDOWS}
          SetStretchBltMode(DrawBmp.Canvas.Handle, STRETCH_DELETESCANS);
          StretchBlt(DrawBmp.Canvas.Handle,R.Left,R.Top,R.Right-R.Left,R.Bottom-R.Top,
             FBackImage.BMP.Canvas.Handle,
             0,0,FBackImage.BMP.Width,FBackImage.BMP.Height,
             SRCCOPY);
          {$endif}
     end;
  end;

  if not FGrid.Above then GridDraw;

  if not EnablePaint then begin
     endpaint(DrawBmp.Canvas.Handle,tps);
     exit;
  end;

  {Draw objects}
if FVisibleObjects then
begin

  if Assigned(fBeforePaint) then fBeforePaint(Self);

  if FCADSource.FCurveList<>nil then
  for H:=0 to Pred(Count) do
  begin
   FC := Curves[H];

   if FC.Visible then
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
               DrawCurve(DrawBmp.Canvas,FB,FC.BlockParams.Magnify,
                   FC.BlockParams.Angle,FC.Points[0].X,FC.Points[0].Y);

        end;
     end;
   else
     DrawCurve(DrawBmp.Canvas,FC,1,0,0,0);
   End;

      if Assigned(fAfterDraw) then fAfterDraw(Self,DrawBmp.Canvas,nil);

//   Application.ProcessMessages;
//   if STOP then
//      exit;
  end;

end;

  if FGrid.Above then GridDraw;

  {CentalCross - Középkereszt}
  If CentralCross then
  begin
    R := Clientrect;
    DrawBmp.Canvas.Pen.Color := clRed;
    DrawBmp.Canvas.Pen.Style := psSolid;
    DrawBmp.Canvas.Pen.Width := 2;
    DrawBmp.Canvas.MoveTo((R.Left+R.Right) div 2,R.Top);
    DrawBmp.Canvas.LineTo((R.Left+R.Right) div 2,R.Bottom);
    DrawBmp.Canvas.MoveTo(R.Left,(R.Top+R.Bottom) div 2);
    DrawBmp.Canvas.LineTo(R.Right,(R.Top+R.Bottom) div 2);
  end;

  finally
    Canvas.CopyRect(R,DrawBmp.Canvas,R);
    endpaint(DrawBmp.Canvas.Handle,tps);
    If CursorCross {and (MouseInOut=1)} then DrawMouseCross(oldMovePt,pmNotXor);
    painting := False;
  end;

except
  painting := False;
  Exit;
end;
  if Assigned(fAfterPaint) then fAfterPaint(Self);
  inherited Paint;
end;

function TCustomCADView.XToW(x: integer): TFloat;
begin
     Result := Forigo.x + x / FZoom;
end;

function TCustomCADView.YToW(y: integer): TFloat;
begin
     Result := Forigo.y + (Height - y) / FZoom;
end;

function TCustomCADView.XToS(x: TFloat): integer;
begin
     Result:=Round(Zoom*(x-origo.x));
end;

function TCustomCADView.YToS(y: TFloat): integer;
begin
     Result:=Height-Round(Zoom*(y-origo.y));
end;

function TCustomCADView.WToS(p: TPoint2d): TPoint;
begin
Try
   Result.x:= XToS(p.x);
   Result.y:= YToS(p.y);
except
   Result:= Point(0,0);
end;
end;

function TCustomCADView.WToS(x, y: TFloat): TPoint;
begin
Try
   Result.x:= XToS(x);
   Result.y:= YToS(y);
except
   Result:= Point(0,0);
end;
end;

function TCustomCADView.SToW(x, y: integer): TPoint2d;
begin
     Result.x := XToW(x);
     Result.y := YToW(y);
end;

function TCustomCADView.SToW(p: TPoint): TPoint2d;
begin
  Result := SToW(p.X,p.Y);
end;

function TCustomCADView.OrigoToCent: TPoint2D;
begin
     Result.x := origo.x+Width/(2*Zoom);
     Result.y := origo.y+Height/(2*Zoom);
end;

function TCustomCADView.CentToOrigo(c: TPoint2D): TPoint2D;
begin
     Result.x := c.x-Width/(2*Zoom);
     Result.y := c.y-Height/(2*Zoom);
end;

procedure TCustomCADView.NewOrigo(x, y: extended);
var  c : TPoint2d;
begin
    FOrigo.x:=x;
    FOrigo.y:=y;
    c := OrigoToCent;
    fCentrum.x := c.x;
    Centrum.y := c.y;
  if Assigned(fChangeWindow) then fChangeWindow(Self,FOrigo,FZoom,Mouse_Pos);
end;

procedure TCustomCADView.NewCentrum(x, y: extended);
begin
  fCentrum.x := x;
  Centrum.y  := y;
  if Assigned(fChangeWindow) then fChangeWindow(Self,FOrigo,FZoom,Mouse_Pos);
  Repaint;
end;

procedure TCustomCADView.NewCentrum(p: TPoint2d);
begin
  NewCentrum(p.x,p.y);
end;

procedure TCustomCADView.ZoomPaper;
var nagyx,nagyy : extended;
    cz: boolean;
begin
  cz := FCentralisZoom;
  FCentralisZoom := True;
  If PaperVisible then begin
  Try
     nagyx := Width /(Paper.x +20);
     nagyy := Height/(Paper.y +20);
  except
     nagyx:=1; nagyy:=1;
  end;
  If nagyx > nagyy Then nagyx:= nagyy;
  fCentrum.x := Paper.x/2;
  Centrum.y := Paper.y/2;
  Zoom:= nagyx;
  end;
  FCentralisZoom := cz;
  invalidate;
end;

procedure TCustomCADView.ZoomDrawing;
var nagyx,nagyy : extended;
    I,J: integer;
    BR: TRect2d;
    x1,x2,y1,y2: TFloat;
    cz : boolean;
begin
if Count>0 then begin
 cz := FCentralisZoom;
 FCentralisZoom := True;
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
  fCentrum.x := (BR.x2+BR.x1)/2;
  fCentrum.y := (BR.y2+BR.y1)/2;
  Zoom:= 0.9*nagyx;
 end;
end else ZoomPaper;
FCentralisZoom := cz;
repaint;
end;

procedure TCustomCADView.MoveWindow(dx, dy: integer);
begin
  if not painting then begin
    fOrigo.x := fOrigo.x - dx/Zoom;
    fOrigo.y := fOrigo.y - dy/Zoom;
    oldCentrum := OrigoToCent;
    fCentrum.x := oldCentrum.x;
    Centrum.y := oldCentrum.y;
   if Assigned(fChangeWindow) then fChangeWindow(Self,FOrigo,FZoom,Mouse_Pos);
   Repaint;
  end;end;

procedure TCustomCADView.MoveCentrum(fx, fy: double);
begin
  fCentrum.x := fx;
  Centrum.y := fy;
end;

procedure TCustomCADView.MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
var n,i,j: integer;
    x,y: double;
    FCurve: TCurve;
begin
  if fCADSource<>nil then
  For n:=0 to Pred(Count) do begin
      FCurve:=FCADSource.FCurveList.Items[n];
      J:=Pred(FCurve.FPoints.Count);
      if FCurve.Selected then begin
         FCurve.MagnifyCurve(Cent, Magnify);
//         Changed := True;
      end;
   end;
//   If AutoUndo then UndoSave;
   invalidate;
end;

procedure TCustomCADView.CurveToCent(AIndex: Integer);
var R : TRect2d;
begin
  R := TCurve(FCADSource.FCurveList.Items[Aindex]).BoundsRect;
  MoveCentrum((R.x1+R.x2)/2,(R.y1+R.y2)/2);
end;

function TCustomCADView.GetDrawExtension: TRect2d;
begin
  Result := FCADSource.FCurveList.GetDrawExtension;
end;

function TCustomCADView.GetLayer(idx: integer): TLayer;
begin
  Result := FCADSource.Layers[idx];
end;

function TCustomCADView.GetLayerCount: integer;
begin
  Result := CadSource.FLayerList.Count;
end;
(*
function TCustomCADView.GetLType(idx: integer): TLineType;
begin
  Result := FCADSource.Linetypes[idx];
end;
*)
function TCustomCADView.GetTransparent: Boolean;
begin
  Result := (csOpaque in ControlStyle);
end;

function TCustomCADView.IsRectInWindow(R: TRect2d): boolean;
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

function TCustomCADView.IsPaperInWindow: boolean;
var
    RP: TRect2d;     // Paper rectangle
    RR: HRgn;
begin
  if PaperVisible then begin
     RP:= Rect2d(0,0,Paper.x,Paper.y);
     Result := IsRectInWindow(RP);
  end;
end;

function TCustomCADView.IsPointInWindow(p: TPoint2d): boolean;
begin
     Result := PontInKep(XToS(p.x),YToS(p.y),Window);
end;

procedure TCustomCADView.GridDraw;
var
    kp,kp0: TPoint2d;
    tav,kpy,mar,marx,mary: extended;
    i,x,y: integer;
    GridTav : integer;     // Distance between lines
    szorzo  : double;
    R : TRect;
begin
If Grid.Visible then begin
   if Grid.Metric = meMM then
      szorzo := 1
   else
      szorzo := inch/10;

  GridTav := Grid.SubDistance;

  With DrawBmp.Canvas do

  if Grid.OnlyOnPaper then begin
  For i:=0 to 2 do begin
      tav  := Gridtav*szorzo;
      if (Zoom*tav)>8 then begin

      Pen.Color := Grid.SubgridColor;
      Case GridTav of
      1:   Pen.Width := 1;
      10:
      begin
         Pen.Width := 2;
         if (Zoom*tav)<64 then Pen.Width := 1;
      end;
      100:
      begin
         Pen.Color := Grid.MaingridColor;
      end;
      end;

      kp.x := 0;
      kp.y := 0; kp0:=kp;

      if Grid.Style=gsLine then begin
      While kp.x<=Paper.x do begin
            MoveTo(XToS(kp.x),YToS(0));
            LineTo(XToS(kp.x),YToS(Paper.y-0.1));
            kp.x:=kp.x+tav;
      end;
      While kp.y<=Paper.y do begin
            MoveTo(XToS(0),YToS(kp.y));
            LineTo(XToS(Paper.x-0.1),YToS(kp.y));
            kp.y:=kp.y+tav;
      end;
      end;

    end;
    GridTav := GridTav * 10;

  end;

  end else
  begin
  For i:=0 to 2 do begin
      tav  := Zoom * Gridtav * szorzo;
      if tav>5 then begin

      Pen.Color := Grid.SubgridColor;
      Case GridTav of
      1:  Pen.Width := 1;
      10: Pen.Width := 2;
      100: Pen.Color := Grid.MaingridColor;
      end;

      marx := -Frac(origo.x/GridTav);
      mary := -Frac(origo.y/GridTav);
      kp.x := tav*marx;
      kp.y := tav*mary; kp0:=kp;

      if Grid.Style in [gsDot,gsCross] then
      While kp.x<=Width do begin
      While kp.y<=Height do begin
       Case Grid.Style of
       gsDot: begin
           Pixels[Trunc(kp.x),Height-Trunc(kp.y)]:= clGreen;
          end;
       gsCross: begin
           MoveTo(Trunc(kp.x)-4,Height-Trunc(kp.y));
           LineTo(Trunc(kp.x)+5,Height-Trunc(kp.y));
           MoveTo(Trunc(kp.x),Height-Trunc(kp.y)-4);
           LineTo(Trunc(kp.x),Height-Trunc(kp.y)+4);
          end;
       end;
       kp.y := kp.y+tav;
      end;
       kp.x := kp.x+tav;
       kp.y := kp0.y;
      end;

      if Grid.Style=gsLine then begin
      While kp.x<=Width do begin
            MoveTo(Trunc(kp.x),0);
            LineTo(Trunc(kp.x),Height);
            kp.x:=kp.x+tav;
      end;
      While kp.y<=Height do begin
            MoveTo(0,Height-Trunc(kp.y));
            LineTo(Width,Height-Trunc(kp.y));
            kp.y:=kp.y+tav;
      end;
      end;

      end; //if tav>3

    GridTav := GridTav * 10;

  end;
  end;

  // Coord Axis color = clRed
  With DrawBmp.Canvas do begin
       Pen.Style := psSolid;
       Pen.Width := 2;
       Pen.Color := clRed;
       X := XtoS(0); Y:=YtoS(0);
       ShowLine(DrawBmp.Canvas,0,y,DrawBmp.Width,y);
       ShowLine(DrawBmp.Canvas,x,0,x,DrawBmp.Height);
  end;
end;
  // Margin draws
  if (Grid.Margin>0) and PaperVisible then begin
       DrawBmp.Canvas.Brush.Style:=bsClear;
       DrawBmp.Canvas.Pen.Style := psSolid;
       DrawBmp.Canvas.Pen.Width := 2;
       DrawBmp.Canvas.Pen.Color := clTeal;
       R:=Rect(XToS(Grid.Margin),YToS(Grid.Margin),XToS(Trunc(Paper.x-Grid.Margin)),
               YToS(Trunc(Paper.y-Grid.Margin)));
       DrawBmp.Canvas.Rectangle(R);
 end;
end;

procedure TCustomCADView.DrawMouseCross(o: TPoint; PenMode: TPenMode);
var
    oldPen: TPen;
begin
Try
    oldPen:=Canvas.Pen;
    Canvas.pen.Color := clBlue;
    Canvas.pen.Width := 1;
    DrawShape(Canvas,Point(0,o.y),Point(Width,o.y),dmLine,PenMode);
    DrawShape(Canvas,Point(o.x,0),Point(o.x,Height),dmLine,PenMode);
Finally
    Canvas.Pen := oldPen;
end;
end;

function TCustomCADView.FTransparentSet: Boolean;
begin

end;

procedure TCustomCADView.Clear;
begin
  if FCADSource.FCurveList<>nil then
     FCADSource.Clear;
end;

function TCustomCADView.LoadGraphFromFile(const FileName: string): Boolean;
begin
  if FCADSource.FCurveList<>nil then
     Result := fCADSource.LoadFromFile(FileName);
  FSelectedIndex := -1;
  invalidate;
end;

function TCustomCADView.SaveGraphToFile(const FileName: string): Boolean;
begin
  if FCADSource.FCurveList<>nil then
     Result := fCADSource.SaveToFile(FileName);
end;
(*
procedure TCustomCADView.Recall;
begin
  invalidate;
  if Assigned(FOnReCall) then FOnReCall(Self);
end;
*)

// =============== Other routins ==============================================

// Draw a shape to Canvas
procedure DrawShape(Canvas: TCanvas; T,B: TPoint; DrawMode: TDrawMode;
                            AMode: TPenMode);
var DC:HDC;
    DX,DY : integer;
begin
  {$IFDEF MSWINDOWS}
          DC := GetDC(Canvas.Handle);
  {$ENDIF}

  With Canvas do
  begin
    Pen.Mode    := AMode;
    Brush.Color := clWhite;
    Brush.style := bsClear;
    If (T.X<>B.x) OR (T.Y<>B.Y) then
    begin
        case DrawMode of
        dmPoint:
            Rectangle(T.X-2,T.Y-2,T.X+2,T.Y+2);
        dmLine,dmPolyline,dmPolygon:
        begin
            MoveTo(T.X, T.Y); LineTo(B.X, B.Y);
        end;
        dmRectangle : Rectangle(T.X, T.Y, B.X, B.Y);
        dmCircle,dmEllipse :
        begin
            dx := Abs(T.X-B.X);
            dy := Abs(T.Y-B.Y);
            if DrawMode=dmCircle then begin
               dx:=Trunc(sqrt(dx*dx+dy*dy));
               dy:=dx;
            end;
            Ellipse(T.X-dx, T.Y-dy, T.X+dx, T.Y+dy);
        end;
        end;
    end;
  end;
  {$IFDEF MSWINDOWS}
          RestoreDC(Canvas.Handle,DC);
  {$ENDIF}
end;

procedure FillActionStrings(st: TStrings);
{ Fill a stringlist wirh ActionModeTexts }
var i: integer;
begin
  st.Clear;
  for i := 0 to High(ActionModeText) do
      st.Add(ActionModeText[i]);
end;

procedure FillDrawmodeStrings(st: TStrings);
{ Fill a stringlist wirh ActionModeTexts }
var i: integer;
begin
  st.Clear;
  for i := 0 to High(DrawModeText) do
      st.Add(DrawModeText[i]);
end;


function CheckForOverLaps( Cuv1, Cuv2 : TCurve): boolean;
var Clip: TClipper;
    shape1,shape2: TPath;
    solution: TPaths;
begin
Try
    Result := false;
    Clip := TClipper.Create;
    Cuv1.ToPath( shape1,100);
    Cuv2.ToPath( shape2,100);
    Clip.AddPath (shape1, ptClip, true);
    Clip.AddPath (shape2, ptSubject, true);
    if Clip.Execute (ctIntersection, solution, pftNonZero, pftNonZero)
    then
    if solution<>nil then
       Result := High(solution[0])>0;
Finally
    Clip.Free;
End;
end;

procedure Register;
begin
  RegisterComponents('ALCad',[TalCADSource, TalCADView, TalCAD, TalSablon]);
end;


initialization

  {$R cursors.res}

  Screen.Cursors[crKez1]          :=  LoadCursor(HInstance, 'SKEZ_1');
  Screen.Cursors[crKez2]          :=  LoadCursor(HInstance, 'SKEZ_2');
  Screen.Cursors[crRealZoom]      :=  LoadCursor(HInstance, 'SREAL_ZOOM');
  Screen.Cursors[crNyilUp]        :=  LoadCursor(HInstance, 'SNYIL_UP');
  Screen.Cursors[crNyilDown]      :=  LoadCursor(HInstance, 'SNYIL_DOWN');
  Screen.Cursors[crNyilLeft]      :=  LoadCursor(HInstance, 'SNYIL_LEFT');
  Screen.Cursors[crNyilRight]     :=  LoadCursor(HInstance, 'SNYIL_RIGHT');
  Screen.Cursors[crZoomIn]        :=  LoadCursor(HInstance, 'SZOOM_IN');
  Screen.Cursors[crZoomOut]       :=  LoadCursor(HInstance, 'SZOOM_OUT');
  Screen.Cursors[crKereszt]       :=  LoadCursor(HInstance, 'SKERESZT');
  Screen.Cursors[crHelp]          :=  LoadCursor(HInstance, 'SHELP_CUR');
  Screen.Cursors[crPolyline]      :=  LoadCursor(HInstance, 'SPOLYLINE');
  Screen.Cursors[crPolygon]       :=  LoadCursor(HInstance, 'SPOLYGON');
  Screen.Cursors[crInsertPoint]   :=  LoadCursor(HInstance, 'SINSERTPOINT');
  Screen.Cursors[crDeletePoint]   :=  LoadCursor(HInstance, 'SDELETEPOINT');
  Screen.Cursors[crNewbeginPoint] :=  LoadCursor(HInstance, 'SNEWBEGINPOINT');
  Screen.Cursors[crRotateSelected]:=  LoadCursor(HInstance,'SROTATESELECTED');
  Screen.Cursors[crFreeHand]      :=  LoadCursor(HInstance,'SFREEHAND');
  Screen.Cursors[crCircle]        :=  LoadCursor(HInstance,'SCIRCLE');
  Screen.Cursors[crRectangle]     :=  LoadCursor(HInstance,'SRECTANGLE');

  VirtualClipboard := TMemoryStream.Create;

finalization
  VirtualClipboard.Free;
end.


unit AL_CADTypes;

interface

uses
  Windows, Classes, SysUtils, Graphics, Forms, System.Generics.Collections,
  Controls, Math,
  Clipper, AL_Objects, NewGeom, B_spline, Szoveg;

Type

  TCadEvent = (ceNone, ceNew, ceAdd, ceChange, ceInsert, ceDelete);

  TDrawMode = (dmNone, dmPoint, dmLine, dmRectangle, dmPolyline, dmPolygon,
               dmCircle, dmEllipse, dmArc, dmChord, dmSpline, dmBspline, dmText,
               dmFreeHand, dmBitmap, dmPath, dmCubicBezier, dmQuadraticBezier,
               dmRotRectangle, dmArrow, dmInsert, dmRoad, dmSolid);

  TActionMode = (amNone, amDrawing, amPaning, amZooming, amPainting, amSelect,
                 amInsertPoint, amDeletePoint, amMovePoint,amSelectPoint,
                 amChangePoint, amDeleteSelected, amMoveSelected, amRotateSelected,
                 amNewBeginPoint, amMagnifySelected,  amSelectArea, amSelectAreaEx,
                 amAutoPlan, amTestWorking, amOutherBegin, amSelectFrame,
                 // Egyenként rámutatok a sorban következõ objektumra
                 // amiket szépen sorba rendezünk az FCurveList-ben
                 amManualOrder, amMovePoints);


Const

  EoLn  : string = chr(13)+chr(10);
  Inch  : double = 25.4500; // mm

  DrawModeText : Array[0..Ord(High(TDrawMode))] of String =
              ('None', 'Point', 'Line', 'Rectangle', 'Polyline', 'Polygon',
               'Circle', 'Ellipse', 'Arc', 'Chord', 'Spline', 'BSpline', 'Text',
               'FreeHand','Bitmap','Path','CubicBezier', 'QuadraticBezier',
               'RotRect', 'Arrow','Insert','Road','Solid');

  ActionModeText : Array[0..23] of String =
                ('None', 'Drawing', 'Paning', 'Zooming', 'Painting','Select',
                 'InsertNode', 'DeleteNode', 'MoveNode','SelectNode',
                 'ChangeNode', 'DeleteSelected', 'MoveSelected', 'RotateSelected',
                 'NewBeginNode', 'MagnifySelected', 'SelectArea', 'SelectAreaEx',
                 'AutoPlan','TestWorking','OutherBeginNode','Frame', 'ManualOrder',
                 'SelectNodes');

  ShapeClosed : Array[0..Ord(High(TDrawMode))] of Boolean =
              (False, False, False, True, False, True,
               True, True, False, True, False, True, False,
               False, False, False, False, True,
               True, True, False, False, True);

// Constants for enum Color
type
  TgaColor = LongWord;
const
  gaByBlock  = $00000000;
  gaRed      = $00000001;
  gaYellow   = $00000002;
  gaGreen    = $00000003;
  gaCyan     = $00000004;
  gaBlue     = $00000005;
  gaMagenta  = $00000006;
  gaWhite    = $00000007;
  gaByLayer  = $00000100;


Type

  TMarkType = (mtBox,mtCircle,mtCross);
  TMarkSize = 2..8;

  TInCode = (icIn,        // Cursor in Curve
             icOnLine,    // Cursor on Curve's line
             icOnPoint,   // Cursor on any Point;
             icOut        // Cursor out of Curve
             );

  TInOutRec = record           // Metszési pont rekord  .margin
       mPont   : TPoint2d;     // metszéspont koordinátái
       idx     : integer;      // idx indexû pont után beszúrni
       d       : double;       // d távolság a kezdõponttól
  end;

  {Gyártási pozíció}
  TWorkPosition = record
    CuvNumber   : integer;      {Aktuális obj. sorszáma}
    PointNumber : integer;      {Aktuális pont sorszáma}
    WorkPoint   : TPoint2d;    {Aktuális pont koordinátái}
  end;


  PPointRec = ^TPointRec;
  TPointRec = record
             ID       : integer;   // Unique number
             funccode : byte;      // 0=Line; 1=Arc; 2=Spline 3=BSpline
             X        : TFloat;
             Y        : TFloat;
             Selected : boolean;
           end;


  TNewGraphData = record //Graphstructur for SaveGraphToFile/LoadGraphFromFile
    Copyright   : Str32;
    Version     : integer;
    GraphTitle  : Str32;
    Curves      : integer;
    Blocks      : integer;
    Layers      : integer;
    Dummy       : Array[1..120] of byte;
  end;

  TNewCurveData = record
    ID       : Integer;
    Name     : Str32;
    Shape    : TDrawMode;
    Layer    : byte;
    Font     : integer;
    Selected : Boolean;
    Enabled  : Boolean;
    Visible  : Boolean;
    Closed   : boolean;
    Angle    : TFloat;
    Points   : Integer;
  end;

  TNewBlockData = record
    BlockName : Str32;
    BasePoint : TPoint2d;
    Layer     : byte;
    CurveCount: integer;
  end;


  { TPointList }

  TPointList = class(TList)
  private
    FOnChange: TNotifyEvent;
    PPoint: PPointRec;
    FBoundsRect: TRect2d;
    function GetPoints(AIndex: integer): TPoint2d;
    procedure SetPoints(AIndex: integer; AValue: TPoint2d);
    procedure Change;
  protected
  public
    destructor Destroy; override;

    function GetBoundsRect: TRect2d;

    procedure ClearPoints;
    function  FirstPoint: TPoint2d;
    function  LastPoint: TPoint2d;
    procedure AddPoint(Ax,Ay: TFloat); overload;
    procedure AddPoint(P: TPoint2d); overload;
    procedure GetPoint(AIndex: Integer; var Ax,Ay: TFloat);
    function  GetPoint2d(AIndex: Integer): TPoint2d;
    function  GetPointRec(AIndex: Integer): TPointRec;
    procedure ChangePoint(AIndex: Integer; Ax,Ay: TFloat); overload;
    procedure ChangePoint(AIndex: Integer; Ax, Ay: TFloat; Sel: boolean); overload;
    procedure SelectPoint(AIndex: Integer; Sel: boolean);
    procedure SelectAllPoints(Sel: boolean); overload;
    function  SelectedPointsCount: integer;
    procedure InsertPoint(AIndex: Integer; Ax,Ay: TFloat);
    procedure DeletePoint(AIndex: Integer); overload;
    procedure DeletePoint(Ax,Ay,delta: TFloat); overload;
    procedure DeletePoints(AIndex1,AIndex2: Integer);
    procedure DeleteSelectedPoints;
    function  GetNearestPoint(p: TPoint2d; var pIdx: integer): TFloat;
    procedure SetBeginPoint(AIndex: Integer);
//    procedure InversPointOrder;
    function  EquelPoints(AIndex1,AIndex2: integer):boolean; overload;
    function  EquelPoints(P1,P2: TPoint2d):boolean; overload;
    function  GetNextID: integer;

    procedure MovePoints(Ax,Ay: TFloat);
    procedure MoveSelectedPoints(Ax,Ay: TFloat);
    procedure Magnify(Cent: TPoint2d; Magnify: TFloat);
    procedure Rotate(Cent : TPoint2d; Angle: TFloat);
    procedure MirrorHorizontal(BR: TRect2d);
    procedure MirrorVertical(BR: TRect2d);

    property BoundsRect: TRect2d read FBoundsRect write FBoundsRect;
    property Points[AIndex: integer] : TPoint2d read GetPoints write SetPoints;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TBlockParams }

  TBlockParams = class(TPersistent)
  private
    fTranslateX: TFloat;
    fTranslateY: TFloat;
    fAngle: TFloat;
    FBasePoint: TPoint2dObj;
    FBlockName: Str32;
    fMagnify: TFloat;
    procedure SetAngle(const Value: TFloat);
    procedure SetBlockName(const Value: Str32);
  public
    constructor Create;
    destructor Destroy(); override;
    function SaveToStream(FileStream: TStream): Boolean;
    function LoadFromStream(FileStream: TStream): Boolean;
  published
    property BlockName  : Str32  read FBlockName write SetBlockName;
    property BasePoint  : TPoint2dObj read FBasePoint write FBasePoint;
    property Angle      : TFloat read fAngle write SetAngle;
    property Magnify    : TFloat read fMagnify write fMagnify;
    property TranslateX : TFloat read fTranslateX write FTranslateX;
    property TranslateY : TFloat read fTranslateY write FTranslateY;
  end;

  { TCurve }

  TCurve = class(TPersistent)
  private
    FBoundsRect: TRect2d;
    FClone_Contour: double;
    fClosed: boolean;
    FContourRadius: double;
    fCrossed: boolean;
    FEnabled: Boolean;
    FFont: TFont;
    FID: Integer;
    FLayer: byte;
    FLineColor: TColor;
    FLineWidth: integer;
    FName: Str32;
    FOnChange: TNotifyEvent;
    fOutBase: boolean;
    FParentID: integer;
    FPause: boolean;
    fSelected: boolean;
    fShape: TDrawMode;
    fSigned: boolean;
    fSorted: boolean;
    FText: Str32;
    FVisible: Boolean;
    FVisibleContour: Boolean;
    fFontHeight: TFloat;
    FBlockParams: TBlockParams;
    function GetCenter: TPoint2d;
    function GetContour: TCurve;
    function GetContourPoints(AIndex: integer): TPoint2d;
    function GetCount: integer;
    procedure SetClosed(AValue: boolean);
    procedure SetContourPoints(AIndex: integer; AValue: TPoint2d);
    procedure SetContourRadius(AValue: double);
    procedure SetCrossed(AValue: boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetLayer(AValue: byte);
    procedure SetName(AValue: Str32);
    procedure SetOutBase(AValue: boolean);
    procedure SetPause(AValue: boolean);
    procedure SetPointRec(AIndex: integer; AValue: TPointRec);
    procedure SetPoints(AIndex: integer; AValue: TPoint2d);
    procedure SetSelected(AValue: boolean);
    procedure SetShape(AValue: TDrawMode);
    procedure SetSigned(AValue: boolean);
    procedure SetSorted(AValue: boolean);
    procedure SetVisible(AValue: Boolean);
    function GetBoundsRect: TRect2d;
    procedure SetBoundsRect(const Value: TRect2d);
  protected
    Loading       : boolean;
  public
    FPoints       : TPointList;          // List of Points
    FContour      : TPointList;          // List of Contour Points
    CPIndex       : Integer;             // Matching point index
    PPoint        : PPointRec;
    PointsArray   : array of TPoint2d;
    TempCurve     : TCurve;

    constructor Create;
    destructor Destroy; override;
    procedure Change;

    procedure ClearPoints;
    function  FirstPoint: TPoint2d;
    function  LastPoint: TPoint2d;
    procedure AddPoint(Ax,Ay: TFloat); overload;
    procedure AddPoint(P: TPoint2d); overload;
    procedure GetPoint(AIndex: Integer; var Ax,Ay: TFloat);
    function  GetPoint2d(AIndex: Integer): TPoint2d;
    function  GetPointRec(AIndex: Integer): TPointRec;
    procedure ChangePoint(AIndex: Integer; Ax,Ay: TFloat); overload;
    procedure ChangePoint(AIndex: Integer; Ax, Ay: TFloat; Sel: boolean); overload;
    procedure InterchangePoints(AIndex1, AIndex2 : Integer);
    procedure SelectPoint(AIndex: Integer; Sel: boolean); //overload;
    procedure SelectAllPoints(Sel: boolean); overload;
    function  SelectedPointsCount: integer;
    procedure InsertPoint(AIndex: Integer; Ax,Ay: TFloat);
    procedure DeletePoint(AIndex: Integer); overload;
    procedure DeletePoint(Ax,Ay: TFloat); overload;
    procedure DeletePoint(Ax,Ay,delta: TFloat); overload;
    procedure DeletePoints(AIndex1,AIndex2: Integer);
    procedure DeleteSelectedPoints;
    procedure SetBeginPoint(AIndex: Integer);
    procedure SetOutherBeginPoint(Ax,Ay: TFloat);
    procedure InversPointOrder;
    procedure AbsolutClosed;

    procedure MoveCurve(Ax,Ay: TFloat);
    procedure MoveSelectedPoints(Ax,Ay: TFloat);
    procedure MagnifyCurve(Cent: TPoint2d; Magnify: TFloat);
    procedure RotateCurve(Cent : TPoint2d; Angle: TFloat);
    procedure MirrorHorizontal;
    procedure MirrorVertical;

    function  IsInBoundsRect(Ax, Ay, delta: TFloat): boolean;
    function  IsOnPoint(Ax, Ay, delta: TFloat): Integer;
    function  IsInCurve(Ax, Ay, delta: TFloat): TInCode; overload;
    function  IsInCurve(P: TPoint2d; delta: TFloat): TInCode; overload;
    function  IsCutLine(P1,P2: TPoint2d): boolean; overload;
    function  IsCutLine(P1, P2: TPoint2d; var d : double): boolean; overload;
    function  IsCutLine(P1, P2: TPoint2d; var idx: integer; var mp: TPoint2d): boolean; overload;
    function  GetKerulet: double;
    function  GetKeruletSzakasz(Aindex1,Aindex2: integer): double;
    function  GetNearestPoint(p: TPoint2d; var pIdx: integer): TFloat; overload;
    function  GetNearestPoint(p: TPoint2d): integer; overload;
    function  IsDirect: boolean;
    function  GetDistance(p: TPoint2d): double;

    procedure Poligonize(PointCount: integer); overload;
    procedure Poligonize(Cuv: TCurve; PointCount: integer); overload;
    procedure Vektorisation(MaxDiff: TFloat);

    procedure FillPointArray; overload;
    procedure FillPointArray(var aList: array of TPoint2d); overload;
    procedure FillTempCurve;
    procedure FillTempCurveByContour;
//    procedure GetContourCurve(Cuv: TCurve);
//    function  GetContourCurve: TCurve;

    function  GetCurveData: TNewCurveData;
    procedure SetCurveData(Data: TNewCurveData);
    function LoadFromStream(FileStream: TStream): Boolean;
    function LoadFromFile(const FileName: string; AIndex: integer): Boolean;
    function SaveCurveToStream(FileStream: TStream): Boolean;
    function LoadCurveFromStream(FileStream: TStream): Boolean;

    //
    procedure CurveToRect(R: TRect2d);
    function CurveToText: String;

    // For Clipper routines
    // Converts the curve points to a int array, with * multiplier
    procedure ToPath(var aList: TPath; multiplier: double);
    // Converts the path int points to the curve points, with /: multiplier
    procedure FromPath(var aList: TPath; multiplier: double);
    procedure SetContour(dist: double);         // Creates a contour into Contour curve

    property Count: integer      read GetCount;   // Pontok száma
    property Center: TPoint2d    read GetCenter;
    property BoundsRect: TRect2d read GetBoundsRect write SetBoundsRect;
    property Pause : boolean     read FPause write SetPause;  // Pause events
    property Contour: TCurve     read GetContour;
    property ContourPoints[AIndex: integer] : TPoint2d read GetContourPoints write SetContourPoints;
    property PointRec[AIndex: integer] : TPointRec read GetPointRec write SetPointRec;
    property Points[AIndex: integer]   : TPoint2d  read GetPoint2d write SetPoints;

    property FontHeight: TFloat read fFontHeight write FFontHeight;
    property Clone_Contour : double read FClone_Contour write FClone_Contour;
    property LineWidth: integer read FLineWidth write FLineWidth;
    property LineColor: TColor  read FLineColor write FLineColor;
    property ParentID: integer read FParentID write FParentID;
    property Crossed: boolean read fCrossed write SetCrossed;
    property Sorted: boolean read fSorted write SetSorted;
    property OutBase: boolean read fOutBase write SetOutBase;
    property VisibleContour: Boolean read FVisibleContour write FVisibleContour;
    property ContourRadius: double read FContourRadius write SetContourRadius;
  published
    property ID: Integer read FID write FID;
    property Name: Str32 read FName write SetName;
    property Shape: TDrawMode read fShape write SetShape;
    property BlockParams: TBlockParams read FBlockParams write FBlockParams;
    property Layer: byte read FLayer write SetLayer default 0;
    property Font: TFont read FFont write SetFont;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property Closed: boolean read fClosed write SetClosed;
    property Selected: boolean read fSelected write SetSelected;
    property Signed: boolean read fSigned write SetSigned;
    property Text: str32 read FText write FText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TCurveList }

  TCurveList = class(TList)
  private
    fChanged: boolean;
    fEnableRecall: boolean;
    FOnChange: TNotifyEvent;
    function GetCurve(idx: integer): TCurve;
    function GetDisabledCount: integer;
    procedure SetCurve(idx: integer; AValue: TCurve);
    procedure Change;
  protected
    FCurve      : TCurve;
  public
    Loading     : boolean;

    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    { Curves and process}
    function MakeCurve(const AName: Str32; ID: integer; Shape: TDrawMode;
                       AClosed: Boolean): Integer; overload;
    function MakeCurve(const AName: Str32; ID: integer; Shape: TDrawMode;
             AEnabled, AVisible, AClosed: Boolean): Integer; overload;
    procedure MakeNewCurve(AIndex: integer; var cuv: TCurve);
    function  GetCurveName(H: Integer): Str32;
    function  GetCurveHandle(AName: Str32): Integer; overload;
    function  GetCurveHandle(AName: Str32; var H: Integer): Boolean; overload;
    function  GetCurveIndex(AName: Str32): Integer;
    function  LastCurve(): TCurve;

    procedure ClearList;
    function  AddCurve(ACurve: TCurve):integer;
    procedure DeleteCurve(AItem: Integer);
    procedure DeleteSelectedCurves;
    procedure DeleteInvisibleCurves;
    procedure DeleteEmptyCurves;
    procedure InsertCurve(AIndex: Integer; Curve: TCurve);
    function  ShapeCount(Shape: TDrawMode): Integer;
    procedure CloneCurve(AIndex: integer);
    procedure CloneSeledted;

    procedure AddPoint(AIndex: Integer; X, Y: TFloat); overload;
    procedure AddPoint(AIndex: Integer; P: TPoint2d); overload;
    procedure InsertPoint(AIndex,APosition: Integer; X,Y: TFloat); overload;
    procedure InsertPoint(AIndex,APosition: Integer; P: TPoint2d); overload;
    procedure DeletePoint(AIndex,APosition: Integer);
    procedure DeleteSamePoints(diff: TFloat);
    procedure ChangePoint(AIndex,APosition: Integer; X,Y: TFloat);
    procedure GetPoint(AIndex,APosition: Integer; var X,Y: TFloat);

    function  GetMaxPoints: Integer;
    function  GetNearestPoint(p: TPoint2d; var cuvIdx, pIdx: integer): TFloat;
    procedure SetNearestBeginPoint(p: TPoint2d);
    procedure SetBeginPoint(ACurve,AIndex: Integer);

    function LoadCurvesFromStream(FileStream: TStream):boolean;
    function SaveCurvesToStream(FileStream: TStream):boolean;
    function LoadCurveFromStream(FileStream: TStream;
      Item: Integer): Boolean;
    function SaveCurveToStream(FileStream: TStream;
      Item: Integer): Boolean;

    procedure MoveCurve(AIndex :integer; Ax, Ay: TFloat);
    procedure MoveSelectedCurves(Ax,Ay: TFloat);
    procedure RotateSelectedCurves(Cent : TPoint2d; Angle: TFloat);
    procedure InversSelectedCurves;
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
    procedure SetAllDirect;

    function  GetDrawExtension: TRect2d;
    procedure SelectAll(all: boolean);
    procedure SelectAllInArea(R: TRect2D);
    procedure SelectAllInAreaEx(R: TRect2d); // Select only points
    procedure ClosedAll(all: boolean);
    procedure BombAll;                      // For selected objects: Bomb for lines
    procedure SelectAllShape(shape: TDrawMode; all: boolean);
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
    procedure Eltolas(dx,dy: double);
    procedure Nyujtas(tenyezo:double);
    procedure CentralisNyujtas(Cent: TPoint2d; tenyezo: double);
    procedure MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
    procedure MirrorHorizontal;
    procedure MirrorVertical;
    procedure MirrorCentral;

    procedure InitParentObjects;
    function  IsParent(AIndex: Integer): boolean; overload;
    function  IsParent(x, y: TFloat): boolean; overload;
    function  GetInnerObjectsCount(AIndex: Integer): integer; overload;
    function  GetInnerObjectsCount(Cuv: TCurve): integer; overload;
    function  GetParentObject(AIndex: Integer): integer; overload;
    function  GetParentObject(x,y: TFloat): integer; overload;
    function  GetRealParentObject(AIndex: Integer): integer;

    property SelectedCount: integer read GetSelectedCount;
    property DisabledCount: integer read GetDisabledCount;

    property Curves[idx: integer]: TCurve read GetCurve write SetCurve;
    property Changed       : boolean      read fChanged write fChanged;
    property OnChange      : TNotifyEvent read FOnChange write FOnChange;
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


// --------------------------------------------------------------------

  TLayerName = String[30];

  TLayer = class(TPersistent)
  private
    fVisible: Boolean;
    fHomogen: Boolean;
    fModified: Boolean;
    fActive: Boolean;
    fTag: LongInt;
    FNote: string;
    fBrush: TBrush;
    fName: TLayerName;
    fPen: TPen;
    fId: Byte;
    procedure SetBrush(const Value: TBrush);
    procedure SetName(const Value: TLayerName);
    procedure SetPen(const Value: TPen);
  published
    constructor Create(Idx: Byte);
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream); virtual;
    procedure LoadFromStream(const Stream: TStream); virtual;
    property ID: Byte read fId;
    property Name: TLayerName read fName write SetName;
    property Pen: TPen read fPen write SetPen;
    property Brush: TBrush read fBrush write SetBrush;
    property Active: Boolean read fActive write FActive;
    property Modified: Boolean read fModified;
    property Homogen: Boolean read fHomogen write fHomogen;
    property Visible: Boolean read fVisible write fVisible;
    property Note: string read FNote write fNote;
    property Tag: LongInt read fTag write fTag;
  end;

  TLayerList = class(TObjectList<TLayer>)
  private
    function GetLayer(idx: integer): TLayer;
    procedure SetLayer(idx: integer; const Value: TLayer);
  public
    function GetLayerIndex(Name: string): byte;
    function GetLayerByName(Name: string): TLayer;
    property Layer[idx: integer] : TLayer read GetLayer
             write SetLayer;
  end;

  TDXFOut = class(TPersistent)
  private
    FromXMin: TFloat;
    FromXMax: TFloat;
    FromYMin: TFloat;
    FromYMax: TFloat;
    ToXMin: TFloat;
    ToXMax: TFloat;
    ToYMin: TFloat;
    ToYMax: TFloat;
    TextHeight: TFloat;
    Decimals: Byte;
    LayerName: Str32;
  public
    StringList: TStringList;
    constructor Create(AFromXMin,AFromYMin,AFromXMax,AFromYMax,AToXMin,AToYMin,
                       AToXMax,AToYMax,ATextHeight: TFloat; ADecimals: Byte);
    destructor Destroy; override;
//    procedure SaveToFile(fn: string);
    function FToA(F: TFloat): Str32;
    function ToX(X: TFloat): TFloat;
    function ToY(Y: TFloat): TFloat;
    procedure Header;
    procedure Trailer;
    procedure SetLayer(const Name: Str32);
    procedure Line(X1,Y1,Z1,X2,Y2,Z2: TFloat);
    procedure Point(X,Y,Z: TFloat);
    procedure StartPolyLine(Closed: Boolean);
    procedure Vertex(X,Y,Z: TFloat);
    procedure EndPolyLine;
    procedure DText(X,Y,Z,Height,Angle: TFloat; const Txt: Str32);
    procedure Layer;
    procedure StartPoint(X,Y,Z: TFloat);
    procedure EndPoint(X,Y,Z: TFloat);
    procedure AddText(const Txt: Str32);
  end;

// --------------------------------------------------------------------

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
  procedure Mirror(idx: integer);  // idx=1-függõleges, 2-vizszintes, 3-középpontos
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

  { TCadPens }

  TCadPens = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    fpBasePen: TPen;
    fpClosed: TPen;
    fpContour: TPen;
    fpCrossed: TPen;
    fpOpened: TPen;
    fpSelected: TPen;
    fpSigned: TPen;
    fpSorted: TPen;
    fbFillBrush: TBrush;
    procedure SetpBasePen(AValue: TPen);
    procedure SetpClosed(AValue: TPen);
    procedure SetpContour(AValue: TPen);
    procedure SetpCrossed(AValue: TPen);
    procedure SetpOpened(AValue: TPen);
    procedure SetpSelected(AValue: TPen);
    procedure SetpSigned(AValue: TPen);
    procedure SetpSorted(AValue: TPen);
    procedure SetbFillBrush(const Value: TBrush);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Change;
  published
    property pBasePen      : TPen read fpBasePen  write SetpBasePen;
    property pClosed       : TPen read fpClosed   write SetpClosed;
    property pOpened       : TPen read fpOpened   write SetpOpened;
    property pSelected     : TPen read fpSelected write SetpSelected;
    property pSigned       : TPen read fpSigned   write SetpSigned;
    property pCrossed      : TPen read fpCrossed  write SetpCrossed;
    property pSorted       : TPen read fpSorted   write SetpSorted;
    property pContour      : TPen read fpContour  write SetpContour;
    property bFillBrush    : TBrush read fbFillBrush write SetbFillBrush;
    property OnChange      : TNotifyEvent read FOnChange write FOnChange;
  end;

  TBlock = class(TList)
  private
    FBlockName: string;
    FBasePoint: TPoint2dObj;
    FLayer: byte;
    function GetBoundsRect: TRect2d;
  public
    FCurveList : TCurveList;
    constructor Create(const AName: String);
    destructor Destroy(); override;
    function LoadBlockFromStream(Stm: TStream): boolean;
    function SaveBlockToStream(Stm: TStream): boolean;

    property BoundsRect: TRect2d  read GetBoundsRect;
    property BlockName : string   read FBlockName write FBlockName;
    property BasePoint : TPoint2dObj read FBasePoint write FBasePoint;
    property Layer     : byte  read FLayer write FLayer;
  end;

  TBlockList = class(TObjectList<TBlock>)
  private
    FBlock : TBlock;
    function GetBlock(idx: integer): TBlock;
    procedure SetBlock(idx: integer; const Value: TBlock);

  public
    function SaveBlockToStream(Stm: TStream; Item: Integer): boolean;
    function LoadBlockFromStream(Stm: TStream; Item: Integer): boolean;
    function GetBlockByName(Name: string): TBlock;
    procedure AddBlock(Value: TBlock);
    property Block[idx: integer] : TBlock read GetBlock
             write SetBlock;
  end;

  TInsertBlock = class(TBlock)
    private
    FRotAngle: TFloat;
    FTranslate: TPoint2dObj;
    FZoom: TPoint2dObj;
    FMagnify: TFloat;
    protected
  public
    constructor Create(const AName: String);
    destructor Destroy(); override;
  published
    property BlockName;
    property BasePoint;
    property Layer     : byte  read FLayer write FLayer;
    property Magnify   : TFloat read FMagnify write FMagnify;
    property RotAngle  : TFloat read FRotAngle write FRotAngle;
    property Translate : TPoint2dObj read FTranslate write FTranslate;
    property Zoom      : TPoint2dObj read FZoom write FZoom;
  end;

  { TCADSource }

  TCADSource = class(TComponent)
  private
    FAppend: boolean;
    fChanged: boolean;
    fEnableRecall: boolean;
    FOnChange: TNotifyEvent;
    FAutoPoligonize: boolean;
    function GetCurve(idx: integer): TCurve;
    procedure SetCurve(idx: integer; AValue: TCurve);
    procedure SetEnableRecall(AValue: boolean);
    function GetBlock(idx: integer): TBlock;
    function GetLayer(idx: integer): TLayer;
    procedure SetBlock(idx: integer; const Value: TBlock);
    procedure SetLayer(idx: integer; const Value: TLayer);
  protected
    Loading       : boolean;
  public
    FCurveList: TCurveList;
    FBlockList: TBlockList;
    FLayerList: TLayerList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Recall : Send a message, an event to every connected class
               to refresh their data, picture, or other. }
    procedure Recall;

    function  LoadFromFile(fn: TFileName): boolean;
    function  SaveToFile(fn: TFileName): boolean;
    function  LoadFromSBN(fn: TFileName): boolean;
    function  SaveToSBN(fn: TFileName): boolean;
    function  LoadFromDXF(const FileName: string): Boolean;
    function  SaveToDXF(const FileName: string):boolean;
    function  LoadFromPLT(const FileName: string): Boolean;
    function  LoadFromDAT(Filename: STRING): boolean;
    function  SaveToDAT(Filename: STRING):boolean;
    function  LoadFromTXT(Filename: STRING):boolean;
    function  SaveToTXT(Filename: STRING):boolean;
    function  SaveToCNC(Filename: STRING):boolean;
    function  LoadFromGKOD(Filename: STRING): boolean;
    function  LoadFromSVG(Filename: STRING): boolean;
    function  SaveToSVG(Filename: STRING):boolean;
    function  GetSVGText: STRING;
    function  LoadFromStream(stm: TStream): boolean;
    function  SaveToStream(stm: TStream): boolean;

    procedure Clear;
    function  GetBlockBoundsRect(BR: TBlock;cuv: TCurve): TRect2d; overload;
    function  GetBlockBoundsRect(cuv: TCurve): TRect2d; overload;
    procedure SetBlockBoundsRect(cuv: TCurve);
    function  GetDrawExtension: TRect2d;

    property Curves[idx: integer]: TCurve read GetCurve write SetCurve;
    property Blocks[idx: integer]: TBlock read GetBlock write SetBlock;
    property Layers[idx: integer]: TLayer read GetLayer write SetLayer;
    {published}
    property Append        : boolean      read FAppend write FAppend;
    property AutoPoligonize: boolean      read FAutoPoligonize write FAutoPoligonize default False;
    property Changed       : boolean      read fChanged write fChanged;
    property EnableRecall  : boolean      read fEnableRecall write SetEnableRecall default True;
    property OnChange      : TNotifyEvent read FOnChange write FOnChange;
  end;

  TalCADSource = class(TCADSource)
  published
    property Append;
    property AutoPoligonize;
    property Changed;
    property EnableRecall;
    property OnChange;
  end;


const
  // User definied cursors in CURSORS.RES
  crKez1           = 18000;
  crKez2           = 18001;
  crRealZoom       = 18002;
  crNyilUp         = 18003;
  crNyilDown       = 18004;
  crNyilLeft       = 18005;
  crNyilRight      = 18006;
  crZoomIn         = 18007;
  crZoomOut        = 18008;
  crKereszt        = 18009;
  crHelp           = 18100;
  crPolyline       = 20000;
  crPolygon        = 20001;
  crInsertPoint    = 20002;
  crDeletePoint    = 20003;
  crNewbeginPoint  = 20004;
  crRotateSelected = 20005;
  crFreeHand       = 20006;
  crCircle         = 20007;
  crRectangle      = 20008;
  crArc            = 20009;
  crSDefault       = 20010;

Const StdColors : Array[0..255] of TColor =
(0,255,65535,65280,16776960,16711680,16711935,16777215,8947848,13158600,255,
8421631,208,6842576,160,5263520,128,4210816,80,2631760,16639,8429823,14544,
6848720,10400,5267616,8320,4214912,6224,2633808,33023,8438015,26832,6856912,
20640,5273760,16512,4219008,10320,2637904,49407,8446207,41168,6863056,30880,
5277856,24704,4223104,16464,2639952,65535,8454143,53456,6869200,41120,5284000,
32896,4227200,20560,2642000,65472,8454112,53408,6869176,41080,5283976,32864,
4227184,20544,2641992,65408,8454080,53352,6869152,41040,5283960,32832,4227168,
20520,2641984,65344,8454048,53304,6869120,41000,5283936,32800,4227152,20504,
2641968,65280,8454016,53248,6869096,40960,5283920,32768,4227136,20480,2641960,
4259584,10551168,3723264,8441960,2662400,6332496,2129920,5275712,1593344,3166248,
8453888,12648320,6868992,10539112,5283840,7905360,4227072,6324288,2641920,4214824,
12648192,14745472,10539008,12111976,7905280,8953936,6324224,7372864,4214784,
4739112,16776960,16777088,13684736,13684840,10526720,10526800,8421376,8421440,
5263360,5263400,16760832,16769152,13672448,13678696,10516480,10520656,8413184,
8417344,5259264,5261352,16744448,16760960,13658112,13672552,10506240,10516560,
8404992,8413248,5253120,5259304,16728064,16752768,13645824,13664360,10496000,
10510416,8396800,8409152,5249024,5255208,16711680,16744576,13631488,13658216,
10485760,10506320,8388608,8405056,5242880,5253160,16711744,16744608,13631544,
13658240,10485800,10506336,8388640,8405072,5242904,5253168,16711808,16744640,
13631592,13658272,10485840,10506360,8388672,8405088,5242920,5253184,16711872,
16744672,13631648,13658296,10485880,10506376,8388704,8405104,5242944,5253192,
16711935,16744672,13631696,13658320,10485920,10506400,8388736,8405120,5242960,
5253200,12583167,14713087,10485968,12085456,7864480,8933536,6291584,7356544,
4194384,4728912,8388863,12615935,6815952,10512592,5243040,7884960,4194432,
6307968,2621520,4204624,4194559,10518783,3670224,8415440,2621600,6312096,
2097280,5259392,1572944,3156048,3684408,6316128,8947848,11579568,14211288,16777215);


Var delta: TFloat = 4;       // Sensitive radius around of points

function AUTOCADStandardColor(X:TgaColor):TColor;
//Function AUTOCADStandardColor(i:integer): TColor;

implementation

USES AL_DXF;

{ TPointList }

function TPointList.GetPoints(AIndex: integer): TPoint2d;
begin
  if Self<>nil then
  Result := GetPoint2d(AIndex);
end;

function TPointList.GetBoundsRect: TRect2d;
begin

end;

procedure TPointList.SetPoints(AIndex: integer; AValue: TPoint2d);
begin
  ChangePoint(AIndex,AValue.x,AValue.y);
end;

procedure TPointList.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

destructor TPointList.Destroy;
begin
//  ClearPoints;
  inherited Destroy;
end;

procedure TPointList.ClearPoints;
var I: Integer;
begin
  if Count>0 then
  For i:=Pred(Count) downto 0 do
      DeletePoint(i);
  Clear;
  Change;
end;

procedure TPointList.AddPoint(Ax, Ay: TFloat);
begin
  GetMem(PPoint,SizeOf(TPointRec));
  PPoint^.ID:=GetNextID;
  PPoint^.X:=Ax;
  PPoint^.Y:=Ay;
  PPoint^.funccode:=0;
  PPoint^.Selected:=False;
  Add(PPoint);
  Change;
end;

procedure TPointList.AddPoint(P: TPoint2d);
begin
  AddPoint(P.x, P.y);
end;

procedure TPointList.GetPoint(AIndex: Integer; var Ax, Ay: TFloat);
begin
  if Self<>nil then
  if InRange(AIndex,0,Pred(Count)) then
  Try
    PPoint:=Items[AIndex];
    Ax:=PPoint^.X;
    Ay:=PPoint^.Y;
  Except
    Ax:=0;
    Ay:=0;
  end;
end;

function TPointList.GetPoint2d(AIndex: Integer): TPoint2d;
begin
  GetPoint( AIndex, Result.X, Result.Y );
end;

function TPointList.GetPointRec(AIndex: Integer): TPointRec;
begin
  if Self<>nil then
  if InRange(AIndex,0,Pred(Count)) then
    Result := TPointRec(Items[AIndex]^);
end;

function TPointList.LastPoint: TPoint2d;
begin
  if Count>0 then
     Result := Points[Count-1];
end;

procedure TPointList.ChangePoint(AIndex: Integer; Ax, Ay: TFloat);
begin
  if Self<>nil then
  if InRange(AIndex,0,Pred(Count)) then
  begin
    PPoint:=Items[AIndex];
    PPoint^.X:=Ax;
    PPoint^.Y:=Ay;
  end;
  Change;
end;

procedure TPointList.ChangePoint(AIndex: Integer; Ax, Ay: TFloat; Sel: boolean);
begin
  if Self<>nil then
  if InRange(AIndex,0,Pred(Count)) then
  begin
    PPoint:=Items[AIndex];
    PPoint^.X:=Ax;
    PPoint^.Y:=Ay;
    PPoint^.Selected:=Sel;
  end;
  Change;
end;

procedure TPointList.SelectPoint(AIndex: Integer; Sel: boolean);
begin
  if Self <> nil then
  if InRange(AIndex,0,Pred(Count)) then
  begin
    PPoint:=Items[AIndex];
    PPoint^.Selected:=Sel;
  end;
  Change;
end;

procedure TPointList.SelectAllPoints(Sel: boolean);
var i: integer;
begin
  if Self <> nil then
    For i:=0 to Pred(Count) do
        SelectPoint(i,Sel);
  Change;
end;

function TPointList.SelectedPointsCount: integer;
var i: integer;
begin
  Result := 0;
  For i:=Pred(Count) downto 0 do begin
    PPoint:=Items[i];
    if PPoint^.Selected then
       Inc(Result);
  end;
end;

procedure TPointList.InsertPoint(AIndex: Integer; Ax, Ay: TFloat);
begin
  if AIndex > -1 then
  begin
    GetMem(PPoint,SizeOf(TPointRec));
    PPoint^.ID:=GetNextID;
    PPoint^.X:=Ax;
    PPoint^.Y:=Ay;
    PPoint^.Selected := False;
    Insert(AIndex,PPoint);
  end;
  Change;
end;

procedure TPointList.DeletePoint(AIndex: Integer);
begin
  if Self<>nil then
    if InRange(AIndex,0,Pred(Count)) then
    begin
    if Items[AIndex]<>nil then
    begin
      FreeMem(Items[AIndex],SizeOf(TPointRec));
      Delete(AIndex);
    end;
    end;
  Change;
end;

procedure TPointList.DeletePoint(Ax, Ay, delta: TFloat);
var i: integer;
    p: TPoint2d;
begin
  if Self<>nil then
  for i:=Pred(Count) downto 0 do begin
      p := GetPoint2d(i);
      if (Abs(Ax-p.x)<delta) and (Abs(Ay-p.y)<delta) then
         DeletePoint(i);
  end;
end;

procedure TPointList.DeletePoints(AIndex1, AIndex2: Integer);
var k,i: integer;
begin
  if Self<>nil then
  begin
  if AIndex2 < AIndex1 then begin
     k := AIndex1;
     AIndex1 := AIndex2;
     AIndex2 := k;
  end;
  if AIndex2>Pred(Count) then
     AIndex2 := Pred(Count);
  if AIndex1<0 then
     AIndex1 := 0;
  for i := AIndex2 downto AIndex1 do
      DeletePoint(i);
  end;
end;

procedure TPointList.DeleteSelectedPoints;
var i: integer;
begin
  if Self<>nil then
  For i:=Pred(Count) downto 0 do begin
    PPoint:=Items[i];
    if PPoint^.Selected then
       DeletePoint(i);
  end;
end;

// Megadja, hogy az alakzat melyik pontja van a legközelebb egy külsõ ponthoz
function TPointList.GetNearestPoint(p: TPoint2d; var pIdx: integer): TFloat;
var
  J   : Integer;
  d   : Double;
  x,y : double;
begin
  Result := 10e+10;
    For J:=0 to Pred(Count) do
    begin
        GetPoint(j,x,y);
        d:=KetPontTavolsaga(p.x,p.y,x,y);
        if d<Result then begin
           pIdx   := J;
           Result := d;
        end;
    end;
end;

procedure TPointList.SetBeginPoint(AIndex: Integer);
var NewPoints: TPointList;
    i,j1,j2: integer;
begin
  if InRange(AIndex,1,Pred(Count)) then
  Try
    NewPoints:=TPointList.Create;
    For i:=AIndex to Pred(Count) do
        NewPoints.AddPoint(Points[i]);
        j1 := 0; j2 := AIndex-1;
        For i:=j1 to j2 do
            NewPoints.AddPoint(Points[i]);
  finally
        ClearPoints;
        For i:=0 to Pred(NewPoints.Count) do
            AddPoint(NewPoints.Points[i]);
        NewPoints.Free;
  end;
end;

function TPointList.EquelPoints(AIndex1, AIndex2: integer): boolean;
begin
  Result := EquelPoints(Points[AIndex1], Points[AIndex2]);
end;

function TPointList.EquelPoints(P1, P2: TPoint2d): boolean;
begin
  Result := IsEqualPoint2d(P1, P2, 0);
end;

function TPointList.FirstPoint: TPoint2d;
begin
  if Count>0 then
     Result := Points[0];
end;

function TPointList.GetNextID: integer;
var i: integer;
begin
  Result := -1;
  For i:=0 to Pred(Count) do
      if TPointRec(Items[i]^).ID>Result then
         Result := TPointRec(Items[i]^).ID;
  Inc(Result);
end;

procedure TPointList.MovePoints(Ax, Ay: TFloat);
var i: integer;
    P: TPoint2d;
begin
    For i:=0 to Pred(Count) do
    begin
      P := Points[i];
      P:=Point2d(P.X+Ax,P.Y+Ay);
      Points[i]:=P;
    end;
    Change;
end;

procedure TPointList.MoveSelectedPoints(Ax, Ay: TFloat);
var i: integer;
begin
  For i:=0 to Pred(Count) do
  begin
    PPoint:=Items[i];
    if PPoint^.Selected then begin
       PPoint^.X:=PPoint^.X+Ax;
       PPoint^.Y:=PPoint^.Y+Ay;
    end;
  end;
end;

procedure TPointList.Magnify(Cent: TPoint2d; Magnify: TFloat);
var i: integer;
begin
  For i:=0 to Pred(Count) do
  begin
    PPoint:=Items[i];
    PPoint^.X := Cent.x + Magnify * (PPoint^.X - Cent.x);
    PPoint^.Y := Cent.y + Magnify * (PPoint^.Y - Cent.y);
  end;
  Change;
end;

procedure TPointList.Rotate(Cent: TPoint2d; Angle: TFloat);
var i : integer;
    pp: Tpoint2d;
begin
  For i:=0 to Pred(Count) do
  begin
    PPoint:=Items[i];
    pp := Point2D(PPoint^.X,PPoint^.Y);
    RelRotate2D(pp,Cent,Angle);
    PPoint^.X:=pp.X;
    PPoint^.Y:=pp.Y;
  end;
  Change;
end;

procedure TPointList.MirrorHorizontal(BR: TRect2d);
var x,y,h: double;
    i: integer;
begin
  BR := BoundsRect;
  h := BR.x2 - BR.x1;     // Középvonal y értéke
  for I := 0 to Pred(Count) do
  begin
    GetPoint(i,x,y);
    ChangePoint(i,h+(h-x),y);
  end;
  Change;
end;

procedure TPointList.MirrorVertical(BR: TRect2d);
var x,y,h: double;
    i: integer;
begin
  BR := BoundsRect;
  h := BR.y2 - BR.y1;     // Középvonal y értéke
  for I := 0 to Pred(Count) do
  begin
    GetPoint(i,x,y);
    ChangePoint(i,x,h+(h-y));
  end;
  Change;
end;

// ------------------------------------------------------------------------

{ TCurve }

constructor TCurve.Create;
begin
inherited Create;
  FPoints        :=TPointList.Create;
  FContour       :=TPointList.Create;
  FBlockParams   :=TBlockParams.Create;
  FFont          :=TFont.Create;
  FFont.Name     :='Arial';
  FFont.Height   := 1;
  FEnabled       :=True;
  FVisible       :=True;
  FClosed        :=True;
  FSelected      :=False;
  FSigned        :=False;
  FShape         :=dmNone;
  FParentID      :=-1;      // Nincs szülõ objektuma
  fOutBase       := false;
  FContourRadius := 4;
  FText          := '';
end;

destructor TCurve.Destroy;
var i,n: integer;
begin
  if Self<>nil then
  begin
    if FPoints<>nil then
    begin
       n:=FPoints.Count;
       if n>0 then
          FPoints.Free;
    end;
    if FContour<>nil then
    begin
       n:=FContour.Count;
       if n>0 then
          FContour.Free;
    end;
    if FFont<>nil then FFont.Free;
    FBlockParams.Free;
  end;
  inherited Destroy;

//  for I:=0 to Pred(FPoints.Count) do FreeMem(FPoints.Items[I],SizeOf(TPointRec));
//  FPoints.Free;
end;

function TCurve.GetBoundsRect: TRect2d;
var
  I: Integer;
  x1,y1,x2,y2: TFloat;
  d : TPoint2d;
  PPoint: PPointRec;
  c,p: TPoint2d;
  block: TBlock;
begin
if Self<>nil then
Try
  if Shape=dmInsert then
     begin
        Result := FBoundsRect;
        Exit;
     end;
  x1:=1E+10;
  y1:=1E+10;
  x2:=-1E+10;
  y2:=-1E+10;
  If Count>0 then
  case Shape of
    dmCircle,dmEllipse:
      begin
         c := Points[0];
         p := Points[1];
         d := Point2d(Abs(c.x-p.x),Abs(c.y-p.y));
         x1 := c.X-d.x;
         x2 := c.X+d.X;
         y1 := c.y-d.Y;
         y2 := c.y+d.Y;
      end;
    dmArc:
      begin
         FillTempCurve;
         Poligonize( TempCurve,10);
         for I:=0 to Pred(TempCurve.Count) do begin
            P:=TempCurve.Points[i];
            if P.x<x1 then x1:=P.x;
            if P.x>x2 then x2:=P.x;
            if P.y<y1 then y1:=P.y;
            if P.y>y2 then y2:=P.y;
         end;
      end;
    else
        for I:=0 to Pred(FPoints.Count) do begin
         P:=Points[i];
         if P.x<x1 then x1:=P.x;
         if P.x>x2 then x2:=P.x;
         if P.y<y1 then y1:=P.y;
         if P.y>y2 then y2:=P.y;
        end;
    end;

      Result.x1 := x1;
      Result.y1 := y1;
      Result.x2 := x2;
      Result.y2 := y2;
      FBoundsRect := Result;
except
end;
end;

function TCurve.GetCenter: TPoint2d;
Var R: TRect2d;
begin
  R := GetBoundsRect;
  Result := Point2d( (R.x2+R.x1)/2, (R.y2+R.y1)/2 )
end;

function TCurve.GetContour: TCurve;
begin
  Result := TCurve.Create;
  Result.FPoints := FContour;
end;

function TCurve.GetContourPoints(AIndex: integer): TPoint2d;
begin
    Result := FContour.Points[AIndex];
end;

function TCurve.GetCount: integer;
begin
  if Self<>nil then
  Result := FPoints.Count;
end;

procedure TCurve.SetClosed(AValue: boolean);
begin
if Enabled then begin
  if fClosed=AValue then Exit;
  fClosed:=AValue;
  Change;
end;
end;

procedure TCurve.SetContourPoints(AIndex: integer; AValue: TPoint2d);
begin
  FContour.Points[AIndex] := AValue;
end;

procedure TCurve.SetContourRadius(AValue: double);
begin
  FContourRadius:=AValue;
  SetContour(FContourRadius);
end;

procedure TCurve.SetCrossed(AValue: boolean);
begin
  if fCrossed=AValue then Exit;
  fCrossed:=AValue;
end;

procedure TCurve.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

procedure TCurve.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont:=AValue;
end;

procedure TCurve.SetLayer(AValue: byte);
begin
  if FLayer=AValue then Exit;
  FLayer:=AValue;
  Change;
end;

procedure TCurve.SetName(AValue: Str32);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TCurve.SetOutBase(AValue: boolean);
begin
  if fOutBase=AValue then Exit;
  fOutBase:=AValue;
end;

procedure TCurve.SetPause(AValue: boolean);
begin
  FPause:=AValue;
  if not FPause then Change;
end;

procedure TCurve.SetPointRec(AIndex: integer; AValue: TPointRec);
begin
  ChangePoint(AIndex,AValue.x,AValue.y,AValue.Selected);
end;

procedure TCurve.SetPoints(AIndex: integer; AValue: TPoint2d);
begin
  ChangePoint(AIndex,AValue.x,AValue.y);
end;

procedure TCurve.SetSelected(AValue: boolean);
begin
  if Enabled then begin
    FSelected := AValue;
    Change;
  end;
end;

procedure TCurve.SetShape(AValue: TDrawMode);
begin
if Enabled then begin
  if fShape=AValue then Exit;
  fShape:=AValue;
end;
end;

procedure TCurve.SetSigned(AValue: boolean);
begin
  if fSigned=AValue then Exit;
  fSigned:=AValue;
  Change;
end;

procedure TCurve.SetSorted(AValue: boolean);
begin
  if fSorted=AValue then Exit;
  fSorted:=AValue;
  Change;
end;

procedure TCurve.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TCurve.Change;
begin
if (not pause) and (not Loading) then begin
  if Assigned(FOnChange) then FOnChange(Self);
end;
end;


procedure TCurve.ClearPoints;
begin
if Enabled then begin
  FPoints.ClearPoints;
  Change;
end;
end;

procedure TCurve.AddPoint(Ax, Ay: TFloat);
begin
if Enabled then begin
  FPoints.AddPoint(Ax,Ay);
  Change;
end;
end;

procedure TCurve.AddPoint(P: TPoint2d);
begin
  FPoints.AddPoint(P);
end;

procedure TCurve.GetPoint(AIndex: Integer; var Ax, Ay: TFloat);
begin
  FPoints.GetPoint(AIndex,Ax,Ay)
end;

function TCurve.GetPoint2d(AIndex: Integer): TPoint2d;
begin
if Self<>nil then
if FPoints.Count>0 then
Try
 if InRange(AIndex,0,Count-1) then
    Result := FPoints.Points[AIndex];
except
end;
end;

function TCurve.GetPointRec(AIndex: Integer): TPointRec;
begin
  Result := FPoints.GetPointRec(AIndex);
end;

function TCurve.LastPoint: TPoint2d;
begin
  Result := FPoints.LastPoint;
end;

procedure TCurve.ChangePoint(AIndex: Integer; Ax, Ay: TFloat);
begin
if Enabled then begin
  FPoints.ChangePoint(AIndex,Ax,Ay);
end;
end;

procedure TCurve.ChangePoint(AIndex: Integer; Ax, Ay: TFloat; Sel: boolean);
begin
if Enabled then begin
  FPoints.ChangePoint(AIndex,Ax,Ay,Sel);
  Change;
end;
end;

procedure TCurve.SelectPoint(AIndex: Integer; Sel: boolean);
begin
  FPoints.SelectPoint(AIndex,Sel);
end;

procedure TCurve.SelectAllPoints(Sel: boolean);
begin
  FPoints.SelectAllPoints(Sel);
end;

function TCurve.SelectedPointsCount: integer;
begin
  Result := FPoints.SelectedPointsCount;
end;

procedure TCurve.InsertPoint(AIndex: Integer; Ax, Ay: TFloat);
begin
if Enabled then begin
  FPoints.InsertPoint(AIndex,Ax,Ay);
  Change;
end;
end;

procedure TCurve.InterchangePoints(AIndex1, AIndex2: Integer);
var p: TPoint2d;
begin
  p := Points[AIndex1];
  Points[AIndex1] := Points[AIndex2];
  Points[AIndex2] := p;
end;

procedure TCurve.DeletePoint(AIndex: Integer);
begin
if Enabled then begin
  FPoints.DeletePoint(AIndex);
  Change;
end;
end;

procedure TCurve.DeletePoint(Ax, Ay: TFloat);
begin
  DeletePoint(Ax, Ay, 0);
end;

procedure TCurve.DeletePoint(Ax, Ay, delta: TFloat);
begin
if Enabled then begin
  FPoints.DeletePoint(Ax, Ay, delta);
  Change;
end;
end;

procedure TCurve.DeletePoints(AIndex1, AIndex2: Integer);
begin
if Enabled then begin
  FPoints.DeletePoints(AIndex1, AIndex2);
  Change;
end;
end;

procedure TCurve.DeleteSelectedPoints;
begin
if Enabled then begin
  FPoints.DeleteSelectedPoints;
  Change;
end;
end;

procedure TCurve.SetBeginPoint(AIndex: Integer);
begin
if Enabled then begin
  FPoints.SetBeginPoint(AIndex);
  Change;
end;
end;

procedure TCurve.SetBoundsRect(const Value: TRect2d);
begin
  FBoundsRect := Value;
end;

procedure TCurve.SetOutherBeginPoint(Ax, Ay: TFloat);
var p: TPoint2d;
    idx: integer;
begin
if Self<>nil then
if Enabled then begin
  GetNearestPoint(Point2d(Ax,Ay),idx);
  p := Points[idx];
  SetBeginPoint(idx);
  InsertPoint(1,p.x,p.y);
  AddPoint(P.x, P.y);
  Points[0]:=Point2d(Ax,Ay);
  fOutBase := true;
  Change;
end;
end;

procedure TCurve.InversPointOrder;
var i: integer;
    x,y: TFloat;
begin
if Enabled then begin
    if Closed then begin
       GetPoint(0,x,y);
       AddPoint(x,y);
       DeletePoint(0);
       For i:=0 to (FPoints.Count div 2)-1 do
           Fpoints.Exchange(i,Fpoints.Count-1-i);
    end else
    For i:=0 to (FPoints.Count div 2)-1 do
        Fpoints.Exchange(i,Fpoints.Count-1-i);
    Change;
end;
end;

// End point := First point
procedure TCurve.AbsolutClosed;
var x,y,x1,y1: double;
begin
if Enabled then begin
  If Closed then begin
     GetPoint(0,x,y);
     GetPoint(FPoints.Count-1,x1,y1);
     if (x<>x1) or (y<>y1) then
        AddPoint(x,y);
     Change;
  end;
end;
end;

procedure TCurve.MoveCurve(Ax, Ay: TFloat);
var i: integer;
    P: TPoint2d;
begin
if Self<>nil then
if Enabled then begin
  FPoints.MovePoints(Ax, Ay);
  FContour.MovePoints(Ax, Ay);
  Change;
end;
end;

procedure TCurve.MoveSelectedPoints(Ax, Ay: TFloat);
var i: integer;
    pr: TPointRec;
begin
if Self<>nil then
if Enabled then begin
   FPoints.MoveSelectedPoints(Ax, Ay);
   SetContour(FContourRadius);
  Change;
end;
end;

procedure TCurve.MagnifyCurve(Cent: TPoint2d; Magnify: TFloat);
var i: integer;
begin
if Self<>nil then
if Enabled then begin
  Loading := True;
  FPoints.Magnify(Cent,Magnify);
  SetContour(FContourRadius);
  Loading := False;
  Change;
end;
end;

procedure TCurve.RotateCurve(Cent: TPoint2d; Angle: TFloat);
var i,j: integer;
    pp: Tpoint2d;
begin
if Self<>nil then
if Enabled then begin
  Loading := True;
  FPoints.Rotate(Cent,Angle);
  SetContour(FContourRadius);
  Loading := False;
  Change;
end;
end;

procedure TCurve.MirrorHorizontal;
begin
if Enabled then begin
  Loading := True;
  FPoints.MirrorHorizontal(BoundsRect);
  SetContour(FContourRadius);
  Loading := False;
  Change;
end;
end;

procedure TCurve.MirrorVertical;
begin
if Enabled then begin
  Loading := True;
  FPoints.MirrorVertical(BoundsRect);
  SetContour(FContourRadius);
  Loading := False;
  Change;
end;
end;

function TCurve.IsInBoundsRect(Ax, Ay, delta: TFloat): boolean;
begin
  Try
    With BoundsRect do
      Result := ((x1-delta)<=Ax) and ((x2+delta)>=Ax) and
             ((y1-delta)<=Ay) and ((y2+delta)>=Ay);
  except
    Exit;
  End;
end;

function TCurve.IsOnPoint(Ax, Ay, delta: TFloat): Integer;
// Result = Point index : if P(Ax,Ay) point in delta radius circle;
// Result = -1          : other else
var
  I: Integer;
begin
  Result := -1;
  for I:=0 to Pred(Count) do begin
      if (Abs(Ax-Points[i].x)<=delta) and (Abs(Ay-Points[i].y)<=delta)
      then begin
           CPIndex := i;
           Result := i;
           exit;
      end;
  end;
end;

function TCurve.IsInCurve(Ax, Ay, delta: TFloat): TInCode;
{Examine that point is in curve or in a point or out of curve}
Var e: TEgyenes;
    i,N: integer;
    arr  : array of TPoint2d;
    PP1,PP2: PPointRec;
    d: double;

function PointInPolygonTest(x, y: real; N: Integer; aList: Array of TPoint2d): Boolean;
Type
   PPoint = ^TPoint;
var
   I, J : Integer;

   Function xp(aVal:Integer):Integer;
   Begin
     Result:= PPoint(@aList[aVal]).X;
   end;

   Function yp(aVal:Integer):Integer;
   Begin
     Result:= PPoint(@aList[aVal]).Y;
   end;

begin
   Result := False;
   {L := Length(aList);}
   if (N = 0) then exit;
   J := N-1;
   for I := 0 to N-1 do
   begin
     if ((((yp(I) <= y) and (y < yp(J))) or
          ((yp(J) <= y) and (y < yp(I)))) and
         (x < (xp(J)-xp(I))*(y-yp(I))/(yp(J)-yp(I))+xp(I)))
     then Result := not Result;
     J:=I;
   end;
end;

Function IsPointOnLine(p, p_1, p_2: TPoint2d; diff: double):boolean;
var d: double;
begin
  {A pontnak az egyenestõl való távolsága = d}
  d := p.x*(p_1.y-p_2.y)-p.y*(p_1.x-p_2.x)+(p_1.x*p_2.y)-(p_1.y*p_2.x);
  if Abs(d)<=diff then Result:=True else Result:=False;
end;

FUNCTION point_dist_to_line(xp,yp,x1,y1,x2,y2: double): double;
// Compute the distance from a point (xp,yp) to a line defined by its
// start (x1,y1) and end (x2,y2) points.
Var dx1p,dx21,dy1p,dy21 : double;
    frac, lambda,xsep,ysep : double;
BEGIN
     dx1p := x1 - xp; dx21 := x2 - x1; dy1p := y1 - yp; dy21 := y2 - y1;
     frac := dx21*dx21 + dy21*dy21;
     if frac=0 then Frac:=1;
     // -- Compute the distance along the line that the normal intersects.
     lambda := -(dx1p*dx21 + dy1p*dy21) / frac;
     // -- Accept if along the line segment, else choose the correct end point.
     lambda := MIN(MAX(lambda,0.0),1.0);
     //-- Compute the x and y separations between the point on the line that is
     //-- closest to (xp,yp) and (xp,yp).
     xsep := dx1p + lambda*dx21;
     ysep := dy1p + lambda*dy21;
     Result := SQRT(xsep*xsep + ysep*ysep);
END;

begin
  Result := icOut;
  if IsInBoundsRect(Ax,Ay, delta) then begin

     // Finds a point
     if IsOnPoint(Ax,Ay,delta)>-1 then begin
        Result := icOnPoint;
     end;

     if FPoints.Count>1 then begin

        // Finds a line
        For i:=0 to FPoints.Count-1 do begin
           PP1:=FPoints.Items[i];
           IF (i=FPoints.Count-1) then begin
              if Closed then PP2:=FPoints.Items[0]
           end else
              PP2:=FPoints.Items[i+1];
           d := point_dist_to_line(Ax,Ay,PP1^.x,PP1^.y,PP2^.x,PP2^.y);
           if d<delta
           then begin
              CPIndex := i+1;
              Result := icOnLine;
//              Exit;
           end;
        end;
        if Result=icOnLine then exit;

        //Point in poligon
        If Closed then begin
        // Fills the arr array with the curve points
        N := FPoints.Count;
        If Closed then N:=N+1;
        SetLength(arr,N);
        For i:=0 to FPoints.Count-1 do begin
           PPoint:=FPoints.Items[i];
           arr[i]:=Point2d(PPoint^.x,PPoint^.y);
        end;
        If Closed then begin
           PPoint:=FPoints.Items[0];
           arr[High(arr)]:=Point2d(PPoint^.x,PPoint^.y);
        end;
        if IsPointInPoligon(arr,Point2d(Ax,Ay)) then
            Result := icIn;
        end;

     end;

  end;
end;

function TCurve.IsInCurve(P: TPoint2d; delta: TFloat): TInCode;
begin
  Result := IsInCurve(p.x,p.y, delta);
end;

function TCurve.IsCutLine(P1, P2: TPoint2d): boolean;
var
  I: Integer;
  mp,pp1,pp2: TPoint2d;
begin
  Result := False;
  i:=0;
  While i<=FPoints.Count-1 do begin
      pp1:=GetPoint2d(i);
      if i<FPoints.Count-1 then
         pp2:=GetPoint2d(i+1)
      else
         pp2:=GetPoint2d(0);
      if SzakaszSzakaszMetszes(pp1,pp2,p1,p2,mp) then begin
           Result := True;
           exit;
      end;
      Inc(i);
  end;
end;

// Megvizsgálja, hogy P1-P2 szakasz áttvágja-e a polygont és
// d metszéspont távolságát adja a P1 elsõ ponttól
function TCurve.IsCutLine(P1, P2: TPoint2d; var d : double): boolean;
var
  I: Integer;
  pp1,pp2,mp: TPoint2d;
  dd: double;
begin
  Result := False;
  i:=0;
  d:=10e+10;
  While i<=Pred(Count) do begin
      pp1:=GetPoint2d(i);
      if i<FPoints.Count-1 then
         pp2:=GetPoint2d(i+1)
      else
         pp2:=GetPoint2d(0);
      if SzakaszSzakaszMetszes(pp1,pp2,p1,p2,mp) then begin
         dd:=RelDist2d(P1,mp);
         if dd<d then d:=dd;
         Result := True;
      end;
      Inc(i);
  end;
end;

// Megvizsgálja, hogy P1-P2 szakasz áttvágja-e a polygont
//               Ha igen, akkor megadja a metszéspont koordinátáit (mp);
//               idx pedig az átmetszett szakasz végpontjának indexe
function TCurve.IsCutLine(P1, P2: TPoint2d; var idx: integer; var mp: TPoint2d): boolean;
var
  I: Integer;
  pp1,pp2: TPoint2d;
begin
  Result := False;
  i:=0;
  While i<=FPoints.Count-1 do begin
      pp1:=GetPoint2d(i);
      if i<FPoints.Count-1 then
         pp2:=GetPoint2d(i+1)
      else
         pp2:=GetPoint2d(0);
      if SzakaszSzakaszMetszes(pp1,pp2,p1,p2,mp) then begin
           idx    := i;
           Result := True;
           exit;
      end;
      Inc(i);
  end;
end;

procedure TCurve.Poligonize(PointCount: integer);
begin
  Poligonize(Self,PointCount);
end;

procedure TCurve.Poligonize(Cuv: TCurve; PointCount: integer);
var pList   : TPointList;
    dd      : CurveDataArray;
    x,y,x1,y1,ArcU,ArcV: TFloat;
    szog, arcR,R1,R2,arcEAngle,deltaFI : extended;
    szog1,szog2,szog3: double;
    i,j,k      : integer;
    pp,pp1,pp2 : TPoint2d;
    p          : pPoints2d;
    Size       : integer;
    arcCirc    : TPoint3d;
    p2d        : TPoint2d;
begin
if Enabled then begin
  if (Cuv<>NIL) and (Cuv.Count>1) then
//  if not (Cuv.Shape in [dmNone, dmPolygon]) then
  Try
     Loading := True;
     // Store the Cuv points in PointList
     pList := TPointList.Create;
     For i:=0 to Pred(Cuv.FPoints.Count) do begin
         p2d := Cuv.Points[i];
         pList.AddPoint(p2d);
     end;
     // First point <> Last point
(*     If Cuv.Closed then
     if pList.EquelPoints(0,Cuv.FPoints.Count-1)
     then pList.DeletePoint(Cuv.FPoints.Count-1);*)


     Case Cuv.Shape of
     dmLine:
          if Cuv.Closed then
             Cuv.Shape := dmPolygon;

     dmPolyline:
       begin
          if Cuv.Closed then
             Cuv.Shape := dmPolygon
          else
          if IsEqualPoint2d(Cuv.FirstPoint,Cuv.LastPoint,0.2) then
          begin
             Cuv.Shape := dmPolygon;
             Cuv.Closed := True;
          end;
       end;

     dmRectangle:
       begin
             Cuv.Shape := dmPolygon;
             Cuv.Closed := True;

       if Count>4 then begin
          if (Count Mod 4)=0 then begin
             p2d := Cuv.Points[0];
             pList.AddPoint(p2d);
             Cuv.ClearPoints;
             Cuv.Shape := dmPolygon;
             Cuv.Closed := True;
             k := Count div 4;
             for i:=0 to 3 do begin
                 pp := pList.Points[i];
                 pp1:= pList.Points[i+1];
                 x  := pp1.x - pp.x;
                 y  := pp1.y - pp.y;
                 for j:=0 to k-1 do begin
                     Cuv.AddPoint(pp.x+x*j/k,pp.y+y*j/k);
                 end;
             end;
          end;
       end;

       end;
     dmCircle:
       begin
         Cuv.ClearPoints;
         Cuv.Shape := dmPolygon;
         Cuv.Closed := True;
         p2d := pList.Points[0];
         ArcU := p2d.x;
         ArcV := p2d.y;
         pp := pList.Points[1];
         arcR := sqrt(sqr(ArcU-pp.x)+sqr(ArcV-pp.y));
         szog := 0;
         if Count<2 then begin
  //          deltaFI := (2*PI)/(2*arcR*PI);
            deltaFI := 1/57;
  //          if deltaFi>pi/180 then deltaFi:=pi/180;
         end else
            deltaFI := (2*PI)/Count;
         While (szog<=(2*pi)) do begin
               x := ArcU + ArcR * cos(szog);
               y := ArcV + ArcR * sin(szog);
               Cuv.AddPoint(x,y);
               szog := szog+deltaFI;
         end;
       end;
     dmEllipse:
       begin
         Cuv.ClearPoints;
         Cuv.Shape := dmPolygon;
         Cuv.Closed := True;
         p2d := pList.Points[0];
         ArcU := p2d.x;
         ArcV := p2d.y;
         p2d := pList.Points[1];
         R1 := Abs(ArcU-p2d.x);
         R2 := aBS(ArcV-p2d.y);
         szog := 0;
         if Count<2 then begin
            deltaFI := (2*PI*2)/(2*R1*PI);
            if deltaFi>pi/180 then deltaFi:=pi/180;
         end else
            deltaFI := (2*PI)/Count;
         While (szog>=0) and (szog<=(2*pi)) do begin
               x := ArcU + R1 * cos(szog);
               y := ArcV + R2 * sin(szog);
               Cuv.AddPoint(x,y);
               szog := szog+deltaFI;
         end;
       end;
     dmArc:
          if Cuv.Count>2 then begin
            pp := Cuv.Points[0];
            pp1:= Cuv.Points[1];
            pp2:= Cuv.Points[2];
            Cuv.ClearPoints;
            arcCirc := HaromPontbolKor(pp,pp1,pp2);
            ArcU := arcCirc.x;
            ArcV := arcCirc.y;
            arcR := arcCirc.z;
            if Cuv.Closed then
               szog := 2*PI
            else
            begin
              Cuv.Shape := dmPolyline;
              szog1:= SzakaszSzog(ArcU,ArcV,pp.x,pp.y);
              szog2:= SzakaszSzog(ArcU,ArcV,pp1.x,pp1.y);
              szog3:= SzakaszSzog(ArcU,ArcV,pp2.x,pp2.y);
              szog := RelSzogdiff(szog1,szog2,szog3);
            end;
            if Count<2 then begin
               deltaFI := Sign(szog)*1/57;
            end else
               deltaFI := szog/Count;
            j := Abs(Trunc(szog/deltaFI));
            for i:=0 to j+1 do begin
                  x := ArcU + arcR * cos(szog1);
                  y := ArcV + arcR * sin(szog1);
                  Cuv.AddPoint(x,y);
                  szog1 := szog1+deltaFI;
            end;
            Cuv.ChangePoint(Cuv.Count-1,pp2.X,pp2.Y);
            cuv.closed := False;
          end;
     dmSpline:
       begin
         j := Pred(Cuv.FPoints.Count);
         for I:=0 to j do
         begin
          Cuv.GetPoint(I,X,Y);
          dd[i+1] := Point2d(x,y);
         end;
         Cuv.ClearPoints;
         Cuv.Shape := dmPolyline;
         Cuv.Closed := False;
         InitdPoints;
         GetSplinePoints(dd,J+1,36,Cuv.Closed);
         for I:=0 to Pred(dPoints.Count) do begin
           p := dPoints[i];
           Cuv.AddPoint(p^.x,p^.y);
         end;
       end;
     dmBSpline:
       begin
         j := Pred(Cuv.FPoints.Count);
         for I:=0 to j do
         begin
          Cuv.GetPoint(I,X,Y);
          dd[i+1] := Point2d(x,y);
         end;
         Cuv.ClearPoints;
         Cuv.Shape := dmPolyline;
         InitdPoints;
         GetBSplinePoints(dd,J+1,10,Cuv.Closed);
         for I:=0 to Pred(dPoints.Count) do begin
           p := dPoints[i];
           Cuv.AddPoint(p^.x,p^.y);
         end;
         Cuv.Shape := dmPolygon;
       end;
     dmCubicBezier:
       begin
          // Virtual Drawing Bitmap
         j := Pred(Cuv.FPoints.Count);
         for I:=0 to j do
         begin
          Cuv.GetPoint(I,X,Y);
          dd[i] := Point2d(x,y);
         end;
         Cuv.ClearPoints;
         Cuv.Shape := dmPolyline;
         Cuv.Closed := False;
         InitdPoints;
         for I:=0 to Pred(dPoints.Count) do begin
           p := dPoints[i];
           Cuv.AddPoint(p^.x,p^.y);
         end;
       end;
     end;
  finally
          if IsEqualPoint2d(Cuv.FirstPoint,Cuv.LastPoint,0.002) then
          begin
             Cuv.Shape := dmPolygon;
             Cuv.Closed := True;
          end;
     pList.Free;
     Loading := False;
  end;
end;
end;

function TCurve.GetKerulet: double;
var
  I: Integer;
  pp1,pp2: TPoint2d;
begin
  Result := 0;
  for I:=0 to FPoints.Count-2 do begin
      pp1:=GetPoint2d(i);
      pp2:=GetPoint2d(i+1);
      Result := Result + KetPontTavolsaga(pp1.X,pp1.y,pp2.x,pp2.y);
  end;
  if Closed then begin
     pp1:=pp2;
     pp2:=GetPoint2d(0);
     Result := Result + KetPontTavolsaga(pp1.X,pp1.y,pp2.x,pp2.y);
  end;
end;

// Meghatározza az objektum kerületi hosszát Aindex1,Aindex2 pontok között;
function TCurve.GetKeruletSzakasz(Aindex1,Aindex2: integer): double;
var
  I: Integer;
  Idx1,Idx2: integer;
  pp1,pp2: TPoint2d;
  Ker: double;
begin
  Result := 0;
  if Aindex2 = Aindex1 then Exit;
  if Aindex2 > Aindex1 then begin
     Idx1 := Aindex1;
     Idx2 := Aindex2;
  end else begin
     Idx1 := Aindex2;
     Idx2 := Aindex1;
     ker := GetKerulet;
  end;
  for I:=Idx1 to Idx2-1 do begin
      pp1:=GetPoint2d(i);
      pp2:=GetPoint2d(i+1);
      Result := Result + KetPontTavolsaga(pp1.X,pp1.y,pp2.x,pp2.y);
  end;
  if Aindex2 < Aindex1 then Result := Ker-Result;
end;

// Megadja, hogy az alakzat melyik pontja van a legközelebb egy külsõ ponthoz
function TCurve.GetNearestPoint(p: TPoint2d; var pIdx: integer): TFloat;
var
  J   : Integer;
  d   : Double;
  x,y : double;
begin
  Result := 10e+10;
    For J:=0 to Pred(FPoints.Count) do
    begin
        GetPoint(j,x,y);
        d:=KetPontTavolsaga(p.x,p.y,x,y);
        if d<Result then begin
           pIdx   := J;
           Result := d;
        end;
    end;
end;

// A p ponthoz legközelebbi támpont indexét adja vissza
function TCurve.GetNearestPoint(p: TPoint2d): integer;
var
  J   : Integer;
  d,dd: Double;
  x,y : double;
begin
  dd := MaxDouble;
  Result := -1;
    For J:=0 to Pred(FPoints.Count) do
    begin
        GetPoint(j,x,y);
        d:=KetPontTavolsaga(p.x,p.y,x,y);
        if d<dd then begin
           dd := d;
           Result := j;
        end;
    end;
end;

function TCurve.IsDirect: boolean;
var ymax: double;
    i,idx: integer;
    Pprior,Pnext: integer;
begin
if Self<>nil then
begin
  // Y max pont megkeresése
  ymax:= -10e+10;
  for i:=0 to Pred(Count) do
      if Points[i].y>ymax then begin
         ymax := Points[i].y;
         idx := i;
      end;
  Pprior := idx-1;
  Pnext  := idx+1;
  if idx=0 then
     Pprior := Pred(Fpoints.Count);
  if idx=Pred(Fpoints.Count) then
     Pnext := 0;
  Result := IsDirectPoligon(Points[Pprior],Points[idx],Points[Pnext]);
end else Result := False;
end;

procedure TCurve.FillPointArray;
var i: integer;
begin
  if Self<>nil then
  SetLength(PointsArray,Count);
  for i:=0 to Count-1 do
      PointsArray[i] := GetPoint2d(i);
end;

procedure TCurve.FillPointArray(var aList: array of TPoint2d);
//(array of TPoint2d);
var i: integer;
begin
  if Self<>nil then
  SetLength(PointsArray,Count);
  for i:=0 to Count-1 do
      PointsArray[i] := GetPoint2d(i);
end;

// A TempCurve-t feltölti a Curve pontjaival és ha kell poligonizálja
procedure TCurve.FillTempCurve;
var i: integer;
begin
  if TempCurve=nil then TempCurve := TCurve.Create;
  TempCurve.Shape := Shape;
  TempCurve.Closed := Closed;
  TempCurve.ClearPoints;
  for i:=0 to Count-1 do
      TempCurve.AddPoint( GetPoint2d(i) );
end;

procedure TCurve.FillTempCurveByContour;
var i: integer;
begin
  if TempCurve=nil then TempCurve := TCurve.Create;
  TempCurve.Shape := Shape;
  TempCurve.Closed := Closed;
  TempCurve.ClearPoints;
  for i:=0 to Count-1 do begin
      TempCurve.AddPoint( GetPoint2d(i) );
  end;
end;

function TCurve.FirstPoint: TPoint2d;
begin
  Result := FPoints.FirstPoint;
end;

// Point distance from Curve
//       Result : distance + out; - in; the curve
function TCurve.GetDistance(p: TPoint2d): double;
var
  J   : Integer;
  d   : Double;
  pj  : TPoint2d;
  mul : double;
begin
  Result := 10e+10;
  mul    := 1;
  if IsInCurve(p,0)=icIn then mul:=-1;
  if FPoints.Count>1 then begin
    For J:=0 to Pred(FPoints.Count) do
    begin
        if j=FPoints.Count-1 then pj:=Points[0]
        else pj:=Points[j+1];
        d := PontSzakaszTavolsaga(p,Points[j],pj);
        if d<Result then begin
           Result := d;
        end;
    end;
        Result := Result * mul;
    end
  else
  if FPoints.Count=1 then
     Result := RelDist2D(p,Points[0]);
end;

function TCurve.GetCurveData: TNewCurveData;
begin
  Result.ID       := ID;
  Result.Name     := Name;
  Result.Shape    := Shape;
  Result.Layer    := Layer;
//  Result.Font     := Font;
  Result.Selected := Selected;
  Result.Enabled  := Enabled;
  Result.Visible  := Enabled;
  Result.Closed   := Closed;
  Result.Points   := fPoints.Count;
end;

procedure TCurve.SetCurveData(Data: TNewCurveData);
begin
  ID          := Data.ID;
  Name        := Data.Name;
  Shape       := Data.Shape;
  Layer       := Data.Layer;
//  Font        := Data.Font;
  Selected    := Data.Selected;
  Enabled     := Data.Enabled;
  Visible     := Data.Visible;
  Closed      := Data.Closed;
end;

function TCurve.LoadFromStream(FileStream: TStream): Boolean;
var
  CurveData : TNewCurveData;
  oShape    : TDrawMode;
  PointRec  : TPointRec;
  N,P       : Integer;
begin
  Result:=False;
  if not Assigned(FileStream) then Exit;
  try
       Loading := True;
       LoadCurveFromStream(FileStream);
       Loading := False;
       Change;
       Result:=True;
  except
    Loading := False;
    exit;
  end;
end;

{ Load a AIndex -th Curve from file }
function TCurve.LoadFromFile(const FileName: string; AIndex: integer): Boolean;
var
  FileStream : TFileStream;
  Header     : TNewGraphData;
  CurveData  : TNewCurveData;
  i          : integer;
begin
  Result:=False;
  if not FileExists(FileName) then Exit;
  try
    Loading := True;
    FileStream:=TFileStream.Create(FileName,fmOpenRead);
    try
      FileStream.Read(Header,SizeOf(TNewGraphData));
      if InRange(AIndex,0,Header.Curves) then
      begin
           For i:=0 to AIndex do
           begin
                if i<Aindex then
                begin
                   FileStream.Read(CurveData,SizeOf(TNewCurveData));
                   FileStream.Seek(CurveData.Points*2*SizeOf(TFloat),1);
                end
                else
                   Result:=LoadFromStream(FileStream);
           end;
      end;
    except
      Result:=False;
    end;
  finally
    Loading := False;
    Change;
    FileStream.Free;
  end;
end;

function TCurve.SaveCurveToStream(FileStream: TStream): Boolean;
var
  NewCurveData: TNewCurveData;
  p : TPointRec;
  N : Integer;
  s : string;
  FH: TFloat;

  procedure WriteStreamStr(Stream : TStream; Str : string);
  var
   StrLen : integer;
  begin
   StrLen := Length(Str);
   Stream.Write(StrLen,SizeOf(Integer));
   Stream.WriteBuffer(Pointer(Str)^, StrLen * SizeOf(Char));
  end;

begin
  Try
    Result:=False;
    NewCurveData := GetCurveData;

    if Shape=dmInsert then
      NewCurveData.Points := 1;

    FileStream.Write(NewCurveData,SizeOf(TNewCurveData));
    for N:=0 to Pred(NewCurveData.Points) do begin
      p := PointRec[N];
      FileStream.Write(p.x,SizeOf(TFloat));
      FileStream.Write(p.y,SizeOf(TFloat));
    end;

    if Shape=dmInsert then
    begin
       FileStream.Write(Self.BlockParams.fBlockName,SizeOf(Self.BlockParams.fBlockName));
       FileStream.Write(Self.BlockParams.fMagnify,SizeOf(TFloat));
       FileStream.Write(Self.BlockParams.fAngle,SizeOf(TFloat));
       FileStream.Write(Self.BlockParams.TranslateX,SizeOf(TFloat));
       FileStream.Write(Self.BlockParams.TranslateY,SizeOf(TFloat));
    end;

    if Shape=dmText then
    begin
       FH := FontHeight;
       FileStream.Write(FH,SizeOf(FH));
       WriteStreamStr(FileStream,Text);
    end;

    Result:=True;
  except
    Exit;
  end;
end;


function TCurve.LoadCurveFromStream(FileStream: TStream): Boolean;
var
  CurveData: TNewCurveData;
  P: TPointRec;
  N: Integer;
  s: string;
  FH: TFloat;

  function ReadStreamStr(Stream : TStream) : string;
  var
     StrLen : integer;
     TempStr : string;
  begin
     TempStr := '';
     Stream.Read(StrLen,SizeOf(integer));
     if StrLen > -1 then
     begin
     SetLength(TempStr, StrLen);
     Stream.ReadBuffer(Pointer(TempStr)^, StrLen * SizeOf(Char));
     result := TempStr;
  end
     else Result := '';
  end;

begin
  Result:=False;
  Loading := True;
  if not Assigned(FileStream) then Exit;
  try
    FileStream.Read(CurveData,SizeOf(TNewCurveData));
    Name  := CurveData.Name;
    Shape := CurveData.Shape;
    Closed:= CurveData.Closed;
    if Shape=dmInsert then
       CurveData.Points := 1;
    for N:=0 to Pred(CurveData.Points) do
    begin
      if FileStream.Read(P.x,SizeOf(TFloat))<SizeOf(TFloat) then
         Exit;
      if FileStream.Read(P.y,SizeOf(TFloat))<SizeOf(TFloat) then
         Exit;
      AddPoint(P.x,P.y);
    end;
    if Shape=dmInsert then
    begin
      FileStream.Read(Self.BlockParams.FBlockName,SizeOf(Self.BlockParams.FBlockName));
      FileStream.Read(Self.BlockParams.fMagnify,SizeOf(TFloat));
      FileStream.Read(Self.BlockParams.fAngle,SizeOf(TFloat));
      FileStream.Read(Self.BlockParams.fTranslateX,SizeOf(TFloat));
      FileStream.Read(Self.BlockParams.fTranslateY,SizeOf(TFloat));
    end;
    if Shape=dmText then
    begin
      FileStream.Read(FH,SizeOf(FH));
      FontHeight := FH;
      Text := ReadStreamStr(FileStream);
    end;
    Loading := False;
    Change;
    Result:=True;
  except
    Loading := False;
    Exit;
  end;
end;

// Az objektumot egy téglalpba torzítja
procedure TCurve.CurveToRect(R: TRect2d);
begin

end;

function TCurve.CurveToText: String;
Var i: integer;

  Function BoolText(b:boolean):string;
  begin
    if b then Result := 'True' else Result := 'False';
  end;

begin
  Result := '';
  Result := Result + '[Curve]'+Eoln;
  Result := Result + 'Name     = '+Name+Eoln;
  Result := Result + 'ID       = '+IntToStr(ID)+Eoln;
  Result := Result + 'Shape    = '+DrawModeText[Ord(Shape)]+Eoln;
  Result := Result + 'Layer    = '+IntToStr(Layer)+Eoln;
  Result := Result + 'Font     = '+Font.Name+','+Inttostr(Font.Size)+Eoln;
  Result := Result + 'Selected = '+BoolText(Selected)+Eoln;
  Result := Result + 'Enabled  = '+BoolText(Enabled)+Eoln;
  Result := Result + 'Visible  = '+BoolText(Visible)+Eoln;
  Result := Result + 'Closed   = '+BoolText(Closed)+Eoln;
  Result := Result + 'Points   = '+IntToStr(FPoints.Count)+Eoln;
  Result := Result + '[Points]'+Eoln;
  for I:=0 to Pred(FPoints.Count) do begin
      PPoint:=FPoints.Items[i];
      Result := Result + '  '+IntToStr(I)+' = '+
             Format('%6.2f',[PPoint^.x])+','+Format('%6.2f',[PPoint^.y])+Eoln;
  end;
  Result := Result + Eoln;
end;

procedure TCurve.ToPath(var aList: TPath; multiplier: double);
{Fills aList array (integer) with multiplier (=100) * Curve point's coordinates }
Var  i: integer;
     pp: TIntPoint;
begin
  SetLength( aList, Count );
  for i := 0 to High(aList) do begin
      pp.x := Round(multiplier*Points[i].X);
      pp.y := Round(multiplier*Points[i].Y);
      aList[i] := pp;
  end;
end;

procedure TCurve.FromPath(var aList: TPath; multiplier: double);
{ Create a contour pointlist from aList }
Var  i,n: integer;
     pp: TIntPoint;
     p: TDoublePoint;
begin
Try
      n := High(aList);
      FContour.ClearPoints;
  for i := 0 to n-1 do begin
      pp := aList[i];
      p  := DoublePoint( pp );
      FContour.AddPoint( Point2d( p.X/multiplier, p.Y/multiplier ) );
  end;
except
End;
end;

procedure TCurve.SetContour(dist: double);
var subj : TPath;
    sol  : TPaths;
    ClipOffset: TClipperOffset;
    i    : integer;
begin
  if Closed and (Count>2) then
  begin
    ClipOffset := TClipperOffset.Create;
    ToPath(subj,100);
    ClipOffset.AddPath(subj, jtRound, etClosedPolygon);
    ClipOffset.Execute(sol, 100*dist);
    if Length(sol)<>0 then
       FromPath(sol[0],100);      // Result in Contour curve
    if ClipOffset<>nil then
       ClipOffset.Free;
  end;
end;

// ------------------------------------------------------------------------

{ TCurveList }

constructor TCurveList.Create(AOwner: TObject);
begin
  inherited Create;
  Loading := False;
end;

destructor TCurveList.Destroy;
begin
  if Self<>nil then
     ClearList;
  inherited Destroy;
end;

function TCurveList.MakeCurve(const AName: Str32; ID: integer;
  Shape: TDrawMode; AClosed: Boolean): Integer;
begin
  MakeCurve(AName,ID,Shape,True,True,AClosed);
end;


function TCurveList.GetCrossedCount: integer;
var i: integer;
begin
   Result := 0;
   For i:=0 to Pred(Count) do
       if Curves[i].Crossed then
         Inc(Result);
end;

function TCurveList.GetCurve(idx: integer): TCurve;
begin
  if ((Idx>-1) and (idx < Count)) then
     Result := Items[idx]
  else
     Result := nil;
end;

function TCurveList.GetCurveHandle(AName: Str32): Integer;
begin
  GetCurveHandle(AName,Result);
end;

function TCurveList.GetDisabledCount: integer;
var i: integer;
begin
   Result := 0;
   For i:=0 to Pred(Count) do
      if Curves[I].Enabled = False then Inc(Result);
end;

procedure TCurveList.SetCurve(idx: integer; AValue: TCurve);
begin
  if ((Idx>-1) and (idx < Count)) then
     Items[idx] := AValue;
end;

procedure TCurveList.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCurveList.MakeCurve(const AName: Str32; ID: integer;
  Shape: TDrawMode; AEnabled, AVisible, AClosed: Boolean): Integer;
var FCurve:TCurve;
begin
Try
  Result := ID;
  IF ID<0 then Result:=Count; //.IndexOf(FCurve)+1;
  FCurve:=TCurve.Create;
  if Pos('_',Aname)>0 then
     FCurve.Name:=AName
  else
     FCurve.Name:=AName+'_'+IntToStr(Result);
  FCurve.ID      := Result;
//  FCurve.Font.Assign(Font);
  FCurve.Enabled := AEnabled;
  FCurve.Visible := AVisible;
  FCurve.Closed  := AClosed;
  FCurve.Shape   := Shape;
//  FCurve.OnChange:= Change;
  Add(FCurve);
  Result:=Count-1; //IndexOf(FCurve);
  Change;
//  if Assigned(fChangeCurve) then fChangeCurve(Self, FCurve, -1);
except
  Result := -1;
end;
end;

procedure TCurveList.MakeNewCurve(AIndex: integer; var cuv: TCurve);
var idx: integer;
    origCuv : TCurve;
begin
Try
  origCuv := Curves[AIndex];
  idx:=Count;
  cuv:=TCurve.Create;
  cuv.Name    := 'NewCurve_'+IntToStr(idx);
  cuv.ID      := idx+1;
  cuv.Closed  := origCuv.Closed;
  cuv.Shape   := origCuv.Shape;
  Change;
except
  cuv := nil;
End;
end;

function TCurveList.GetCurveName(H: Integer): Str32;
begin
  if InRange(H,0,Pred(Count)) then
     Result := Curves[H].Name
  else
     Result := '';
end;

function TCurveList.GetCurveHandle(AName: Str32; var H: Integer): Boolean;
begin
  Result := False;
  Try
     H := GetCurveIndex(AName);
     Result := H>-1;
  except
     H := -1;
  End;
end;

function TCurveList.GetCurveIndex(AName: Str32): Integer;
var
  I,J: Integer;
begin
  Result:=-1;
  J:=Count;
  I:=0;
  AName:=AnsiUpperCase(AName);
  while I < J do
  begin
    FCurve:=Items[I];
    if AnsiUpperCase(FCurve.Name) = AName then
    begin
      Result:=I;
      Exit;
    end;
    Inc(I);
  end;
end;

procedure TCurveList.ClearList;
var
  i,db,n: Integer;
  s: string;
begin
  db := Pred(Count);
  For i:=db downto 0 do
  if Curves[i]<>nil then
     if Curves[i].Count>0 then
        Curves[i].Free;
  Clear;
  Change;
end;

function TCurveList.AddCurve(ACurve: TCurve): integer;
begin
Try
  Pack;
  Add(ACurve);
  Result := Count-1;
  Changed := True;
except
  Result := -1;
end;
end;

procedure TCurveList.DeleteCurve(AItem: Integer);
begin
  if (AItem>-1) and (AItem < Count) then
  begin
    Delete(AItem);
    Changed := True;
  end;
end;

// Deletes all selected curves and selected points
procedure TCurveList.DeleteSelectedCurves;
var i,j: integer;
begin
  i:=0;
  if Count>0 then begin
  While i<Count do begin
      FCurve:=Items[i];
      if FCurve.Selected then begin
         Delete(i);
         Dec(i);
         Changed := True;
      end else
      For j:=Pred(FCurve.Count) downto 0 do
          if FCurve.PointRec[j].Selected then
             FCurve.DeletePoint(j);
      Inc(i);
  end;
  end;
end;

procedure TCurveList.DeleteInvisibleCurves;
var i,j: integer;
begin
  i:=0;
  if Count>0 then begin
  While i<Count do begin
      FCurve:=Items[i];
      if not FCurve.Visible then begin
         DeleteCurve(i);
         Dec(i);
         Changed := True;
      end;
      Inc(i);
  end;
  end;
end;

// A 0 pontot tartalmazó alakzatok törlése
procedure TCurveList.DeleteEmptyCurves;
var i: integer;
begin
  for i:=Pred(Count) downto 0 do
  begin
      FCurve:=Items[i];
      if FCurve.Count=0 then begin
         DeleteCurve(i);
         Changed := True;
      end;
  end;
end;

procedure TCurveList.InsertCurve(AIndex: Integer; Curve: TCurve);
begin
  if (AIndex > -1) and (AIndex < Count-1) then
  begin
    Insert(AIndex,Curve);
    Changed := True;
  end;
end;

// Megszámlálja az adott tipusú objektumokat
function TCurveList.ShapeCount(Shape: TDrawMode): Integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to Pred(Count) do
    if Curves[i].Shape=Shape then Inc(Result);
end;

// Másolatot készít az objektumból és azt a lista végére fûzi
procedure TCurveList.CloneCurve(AIndex: integer);
var h,i: integer;
begin
//  SetContour( AIndex, ContourRadius );
  h:=MakeCurve('Clone',-1,Curves[AIndex].Shape,Curves[AIndex].Enabled,
                          Curves[AIndex].Visible,Curves[AIndex].Closed);
  for I := 0 to Pred(Curves[AIndex].Count) do
      AddPoint(h,Curves[AIndex].Points[i]);
  Changed := True;
//  invalidate;
end;

procedure TCurveList.CloneSeledted;
var
   i: integer;
begin
  if GetSelectedCount > 0 then
  begin
         For i:=0 to Pred(Count) do
             if Curves[i].Selected then
                CloneCurve(i);
    Changed := True;
  end;
end;

procedure TCurveList.AddPoint(AIndex: Integer; X, Y: TFloat);
begin
  if InRange(AIndex,0,Pred(Count)) then
  begin
    Curves[AIndex].AddPoint(X,Y);
    Changed := True;
  end;
end;

procedure TCurveList.AddPoint(AIndex: Integer; P: TPoint2d);
begin
  AddPoint(AIndex,p.X,p.Y);
end;

procedure TCurveList.InsertPoint(AIndex, APosition: Integer; X, Y: TFloat);
begin
  if InRange(AIndex,0,Pred(Count)) then
  begin
    Curves[AIndex].InsertPoint(APosition,X,Y);
    Changed := True;
  end;
end;

procedure TCurveList.InsertPoint(AIndex, APosition: Integer; P: TPoint2d);
begin
  InsertPoint(AIndex, APosition,P.X,P.Y);
end;

procedure TCurveList.DeletePoint(AIndex, APosition: Integer);
begin
  if InRange(AIndex,0,Pred(Count)) then
  begin
    Curves[AIndex].DeletePoint(APosition);
    Changed := True;
  end;
end;

procedure TCurveList.DeleteSamePoints(diff: TFloat);
// Deletes all same points in range of diff: only one point remains.
// Azonos vagy nagyon közeli pontok kiejtése
var i,j,k  : integer;
    x,y    : TFloat;
    x1,y1  : TFloat;
begin
  for i:=0 to Pred(Count) do begin
    FCurve:=Curves[i];
    j:=0;
    if FCurve.FPoints.Count>1 then
    while j<FCurve.FPoints.Count do begin
          FCurve.GetPoint(j,x,y);
          k:=j+1;
          while k<FCurve.FPoints.Count do begin
                FCurve.GetPoint(k,x1,y1);
                if (Abs(x-x1)<diff) and (Abs(y-y1)<diff) then
                   FCurve.DeletePoint(k);
                inc(k);
          end;
          inc(j);
    end;
  end;
end;

procedure TCurveList.ChangePoint(AIndex, APosition: Integer; X, Y: TFloat);
begin
  if InRange(AIndex,0,Pred(Count)) then
  begin
    if APosition < Curves[AIndex].Count then
       Curves[AIndex].ChangePoint(APosition,X,Y);
    Changed := True;
  end;
end;

procedure TCurveList.GetPoint(AIndex, APosition: Integer; var X, Y: TFloat);
begin
  if InRange(AIndex,0,Pred(Count)) then
  begin
    FCurve:=Items[AIndex];
    if InRange(APosition,0,Pred(FCurve.FPoints.Count)) then
      FCurve.GetPoint(APosition,X,Y);
  end;
end;

function TCurveList.GetMaxPoints: Integer;
var
  I,Max: Integer;
begin
  Max:=0;
  for I:=0 to Pred(Count) do
  begin
    FCurve:=Items[I];
    if FCurve<>nil then
    if FCurve.FPoints.Count > Max then Max:=FCurve.FPoints.Count;
  end;
  Result:=Max;
end;

// Searching for the nearest point in graph
// Result : distance from p point
//    VAR : cuvIdx = Curve's number
//          pIdx   = Point's number
function TCurveList.GetNearestPoint(p: TPoint2d; var cuvIdx, pIdx: integer
  ): TFloat;
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
  for I:=0 to Pred(Count) do
  begin
    Cuv:=Items[I];
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
     Cuv := Items[CuvIdx];
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

// A p ponthoz legközelebbi alakzat legközelebbi pontja
// lesz az új kezdõpont
procedure TCurveList.SetNearestBeginPoint(p: TPoint2d);
var CuvIdx,NodeIdx: integer;
begin
  if GetNearestPoint( p,CuvIdx,NodeIdx )>-1 then
     SetBeginPoint( CuvIdx,NodeIdx );
end;

procedure TCurveList.SetBeginPoint(ACurve, AIndex: Integer);
begin
Try
       FCurve:=Items[ACurve];
       FCurve.SetBeginPoint(AIndex);
       Changed  := True;
except
end;
end;


function TCADSource.SaveToFile(fn: TFileName): Boolean;
var
  Ext: string;
begin
  try
      Ext := UpperCase(ExtractFileExt(FN));
      if Ext = '.SBN' then
         Result:=SaveToSBN(fn);
  except
      Result:=False;
  end;
end;

function TCurveList.LoadCurvesFromStream(FileStream: TStream): boolean;
var i: integer;
begin
  FileStream.Seek(0,0);
  While FileStream.Position<FileStream.Size do
      LoadCurveFromStream(FileStream,i);
end;

function TCurveList.SaveCurvesToStream(FileStream: TStream): boolean;
var i: integer;
begin
  for i:=0 to Pred(Count) do
      SaveCurveToStream(FileStream,i);
end;

function TCurveList.SaveCurveToStream(FileStream: TStream; Item: Integer
  ): Boolean;
begin
  Result:=False;
  if not InRange(Item,0,Pred(Count)) or not Assigned(FileStream) then Exit;
  FCurve:=Items[Item];
  Result := FCurve.SaveCurveToStream(FileStream);
end;
(*
function TCurveList.LoadFromDXF(const FileName: string): Boolean;
Type TDXFSec = (POINT,LINE,POLYLINE,LWPOLYLINE,CIRCLE,ARC,SPLINE,DFACE);
var
  f: TEXTFILE;
  sor,s : string;
  NewFileName : string;
  N,i,k: Integer;
  x,y  : real;
  NewCurve : boolean;
  DXFSec   : TDXFSec;
  closed   : boolean;
  elso     : boolean;
  FirstPoint,EndPoint : TPointRec;
  arcU,arcV,arcR,arcSAngle,arcEAngle: TFloat;
  szog,kk,deltaFI  : TFloat;
  arcPPP : T3Point2d;
  kod,adat  : string;
const DXFstr : array[0..7] of string =
          ('POINT','LINE','POLYLINE','LWPOLYLINE','CIRCLE','ARC','SPLINE','3DFACE');
label 111;

  {Alakzat név keresés}
  function DXFKeres(s: string):integer;
  var j: integer;
  begin
    Result := -1;
    For j:=0 to High(DXFstr) do
        If s=DXFstr[j] then begin
            Result := j;
            Break;
        end;
  end;

  function ValidDXF(FN: STRING):boolean;
  Var ff,df: File;
      newFile: string;
      fsor : array[1..128] of Char;
      z,NumRead,NumWritten  : integer;
      buf  : char;
  begin
       result := False;
    Try
       if not FileExists(fn) then Exit;
       try
       Loading := True;
       {$I-}
       AssignFile(ff,FN);
       system.Reset(ff,1);
       {$I+}
       BlockRead(ff,fsor,SizeOf(fsor),NumRead);
       For z:=2 to 128 do
           if (fsor[z] = #10) and (fsor[z-1] = #13) then Result := True;
       newFile:=ExtractFilePath(fn)+'_'+ExtractFileName(fn);
       if not FileExists(newFile) then
       if not Result then begin
          Screen.Cursor := crHourGlass;
          system.Reset(ff,1);
          {$I-}
          AssignFile(df,newFile);
          system.Rewrite(df,1);
          {$I+}
          repeat
                BlockRead(ff, Buf, SizeOf(Buf), NumRead);
                if buf=#10 then begin
                   Buf := #13;
                   BlockWrite(dF, Buf, 1, NumWritten);
                   Buf := #10;
                   BlockWrite(dF, Buf, 1, NumWritten);
                end else
                   BlockWrite(dF, Buf, NumRead, NumWritten);
          until (NumRead = 0) or (NumWritten <> NumRead);
          CloseFile(df);
       end;
       finally
         CloseFile(ff);
       end;
    except
       result := False;
    end;
  end;

begin
  FormatSettings.Decimalseparator := '.';
  Result:=False;
  Loading := True;
  if not FileExists(FileName) then Exit;
  newFileName:=ExtractFilePath(FileName)+'_'+ExtractFileName(FileName);

  if not ValidDXF(FileName) then
     newFileName:=ExtractFilePath(FileName)+'_'+ExtractFileName(FileName)
  else
     newFileName:=FileName;


     newFileName:=FileName;

  try
  {$I-}
    AssignFile(f,NewFileName);
    system.Reset(f);
  {$I+}
    try
      repeat
        ReadLn(f,sor)
      Until sor='ENTITIES';
      NewCurve := False;
      k:=0;
      repeat
111:    ReadLn(f,sor);
        k := DXFKeres(sor);
        elso := True;
        If k>-1 then begin
            DXFSec := TDXFSec(k);
            Case DXFSec of
            LINE :
              N:=MakeCurve('Line',-1,dmLine,True,True,False);
            POLYLINE :
            begin
                repeat
                    ReadLn(f,sor);
                    If (sor = ' 70') then begin
                        ReadLn(f,sor);
                        Closed := (StrToInt(sor) and 1)=1;
                    end;
                until sor='VERTEX';
                N:=MakeCurve('Polyline',-1,dmPolyline,True,True,Closed);
            end;

            LWPOLYLINE :
                N:=MakeCurve('Polyline',-1,dmPolyline,True,True,False);

            CIRCLE :
            repeat
            ReadLn(f,sor);
            If sor = ' 10' then begin
               ReadLn(f,sor);
               arcU := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 20' then begin
               ReadLn(f,sor);
               arcV := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 40' then begin
               ReadLn(f,sor);
               arcR := StrToFloat(Alltrim(sor));
               N:=MakeCurve('Circle',-1,dmCircle,True,True,True);
               AddPoint(N,arcU,arcV);
               AddPoint(N,arcU+arcR,arcV);
               goto 111;
            end;
            Until (sor='  0') or EOF(f);

            ARC :
            begin
            repeat
            ReadLn(f,sor);
            If sor = ' 10' then begin      {Center x}
               ReadLn(f,sor);
               arcU := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 20' then begin      {Center y}
               ReadLn(f,sor);
               arcV := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 40' then begin      {Radius}
               ReadLn(f,sor);
               arcR := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 50' then begin      {Start angle}
               ReadLn(f,sor);
               arcSAngle := StrToFloat(Alltrim(sor));
            end;
            If sor = ' 51' then begin      {End angle}
               ReadLn(f,sor);
               arcEAngle := StrToFloat(Alltrim(sor));
               {Átszámítás 3 kerületi pontra}
               arcPPP := KorivbolHarompont(arcU,arcV,arcR,DegToRad(arcSAngle),DegToRad(arcEAngle));
            end;
            Until (sor='  0') or EOF(f);
               N:=MakeCurve('Arc',-1,dmArc,True,True,False);
               AddPoint(N,arcPPP.p1.x,arcPPP.p1.y);
               AddPoint(N,arcPPP.p2.x,arcPPP.p2.y);
               AddPoint(N,arcPPP.p3.x,arcPPP.p3.y);
               goto 111;
            end;

            SPLINE :
            begin
                Closed := False;
                N:=MakeCurve('Spline',-1,dmPolygon,True,True,Closed);
            end;

            DFACE :
                N:=MakeCurve('Polyline',-1,dmPolyline,True,True,False);

            end;

            repeat
               ReadLn(f,sor);

                if ((DXFSec=SPLINE) or (DXFSec=LINE)) and (sor='  0') then
                   break;

                if (DXFSec=SPLINE) then
                If (sor=' 71') then begin
                    ReadLn(f,sor);
                    i:=StrToInt(sor);
                    case i of
                    1,3: // Nem megy át a támpontokon
                       Curves[N].Shape := dmBSpline;
                    2: // átmegy a támpontokon
                       Curves[N].Shape := dmSpline;
                    end;
                end;
                // Zárt alakzat vizsgálat
                If (sor=' 70') then begin
                    ReadLn(f,sor);
                    if StrToInt(sor)=1 then begin
                       Curves[N].Closed := True;
                       if Curves[N].Shape = dmPolyline then
                          Curves[N].Shape := dmPolygon;
                    end;
                end;
                If (sor=' 10') or (sor=' 11') {or (sor=' 12') or (sor=' 13')} then begin
                    ReadLn(f,sor);
                    x:=StrToFloat(sor);
                end;
                If (sor=' 20') or (sor=' 21') {or (sor=' 22') or (sor=' 23')} then begin
                    ReadLn(f,sor);
                    y:=StrToFloat(sor);
                    AddPoint(N,x,y);
                    If elso then begin
                        FirstPoint.x := x;
                        FirstPoint.y := y;
                        elso := False;
                    end;
                    EndPoint.x := x;
                    EndPoint.y := y;
                end;

            until (sor='SEQEND') or (sor='ENDSEC') or (EOF(f)) or (sor='');

            if (DXFSec=POLYLINE) and (not Closed) then begin
               FCurve := Curves[N];
               FCurve.Closed := (FirstPoint.x=EndPoint.x) and (FirstPoint.y=EndPoint.y);
            end;

        end;

        until (sor='EOF') or EOF(f);


    except
      Result:=False;
    end;
  finally
    CloseFile(f);
//    if AutoUndo then UndoSave;
    Loading := False;
  end;
end;
*)

function TCADSource.SaveToDXF(const FileName: string): boolean;
begin

end;

function TCADSource.LoadFromPLT(const FileName: string): Boolean;
var f     : TEXTFILE;
    sor,S : string;
    N,i,pv,vpoz  : integer;
    x,y   : double;
    KOD   : string;
    xx,yy : string;
    FirstPoint,EndPoint : TPoint;
    elso  : boolean;
    delim : string;
begin
Try
  Loading := True;
  AssignFile(f,FileName);
  system.Reset(f);
  ReadLn(f,sor);
  repeat
    ReadLn(f,sor);
    pv := StrCount(sor,';');

    if pv>0 then
       for I := 1 to pv do
       begin
         s := StrCountD(sor,';',i);
         KOD := UpperCase(Copy(s,1,2));
         if (KOD='PU') or (KOD='PD') then
         begin
           if (KOD='PU') then begin
              N:=FCurveList.MakeCurve('Object',-1,dmPolygon,True,True,True);
              FCurveList.FCurve := Curves[N];
              elso := true;
           end;
           // Pontok kinyerése
           S := Copy(s,3,Length(s));
           delim := ' ';
           if Pos(',',s)>0 then
              delim := ',';
           xx := StrCountD(s,delim,1);
           yy := StrCountD(s,delim,2);
             Try
                x := strtoFloat(xx)/39.37;
                y := strtoFloat(yy)/39.37;
             finally
                If elso then begin
                   FirstPoint.x := StrToInt(xx);
                   FirstPoint.y := StrToInt(yy);
                   elso := False;
                end;
                FCurveList.FCurve.AddPoint(x,y);
                EndPoint.x := StrToInt(xx);
                EndPoint.y := StrToInt(yy);
             end;
         end;
       end;

      if Curves[N]<>nil then begin
        Curves[N].Closed := (Abs(FirstPoint.x-EndPoint.x)<0.5) and (Abs(FirstPoint.y-EndPoint.y)<0.5);
        if Curves[N].Closed then Curves[N].Shape:=dmPolygon else Curves[N].Shape:=dmPolyline;
      end;

  Until EOF(f);
Finally
  CloseFile(f);
  Loading := False;
End;
end;

function TCADSource.LoadFromDAT(Filename: STRING): boolean;
var D: Textfile;
    S,s1,s2: String;
    H,N: Integer;
    x,y: real;
    nCuv  : integer;
BEGIN
  if not FileExists(Filename) then exit;
Try
  Loading := True;
  nCuv := 0;
  AssignFile(D,Filename);
    Reset(D);
    // Read first line for text file examination
    Readln(D, S);

    H:=FCurveList.MakeCurve('DAT0',0,dmPolyline,True,True,False);

    // Ebben a text fileban csak soremelés karakterek vannak
    // azaz az elsõ felolvasással az egész file-t beolvassa
    if Length(s)>15 then begin
       s := trim(s);
       S := Stuff(s,#9,' ');
       S := Stuff(s,#10,' ');
       N := (StrCount(s,' ')+1) div 2;
       if N>0 then
       repeat
         s1 := StrCountD(s,' ',1);
         s2 := StrCountD(s,' ',2);
         x := strtofloat(s1);
         If s2='0.000' then y:=0 else
         y := strtofloat(s2);
         FCurveList.AddPoint(H,x,y);
         N  := CountPos(s,' ',2);
         s  := Trim(Copy(s,N,Length(s)));
       until N<1;
    end

    else

    repeat
      s := trim(s);

      if (Copy(s,1,1)<>';') and (s<>'') then begin
         S := Stuff(s,#9,' ');
         S := Stuff(s,#10,' ');
         s1 := StrCountD(s,' ',1);
         s2 := StrCountD(s,' ',2);
         If s1='0.000' then x:=0 else
         x := strtofloat(s1);
         If s2='0.000' then y:=0 else
         y := strtofloat(s2);
         FCurveList.AddPoint(H,x,y);
      end;

      if eof(D) then Break;

      Readln(D, S);

        if Trim(s)='' then begin
           s1 := ''+IntToStr(nCuv);
           H:=FCurveList.MakeCurve(s1,0,dmPolyline,True,True,False);
           Inc(nCuv);
        end;
    until False;

FINALLY
    CloseFile(D);
    Loading := False;
END;
END;

function TCADSource.SaveToDAT(Filename: STRING): boolean;
var D: Textfile;
    r: TRect2d;
    szorzo: double;
    dx,dy: double;
    H,I,N: Integer;
    xx,yy: TFloat;
    s,s0: string;
    FCurve : TCurve;
BEGIN

Try
  Result := False;
  Loading := True;
  H := 0;
  AssignFile(D,Filename);
    Rewrite(D);
    r := FCurveList.GetDrawExtension;
    dx := r.x2-r.x1;
    dy := r.y2-r.y1;
    FCurvelist.Eltolas(-r.x1,-r.y1);
    szorzo:= 1/dx;
    // if dx>dy then szorzo:= 1/dx else szorzo:= 1/dy;
    for i:=0 to Pred(FCurveList.Count) do begin
      FCurve:=Curves[I];
    for N:=0 to Pred(FCurve.FPoints.Count) do begin
      FCurve.GetPoint(N,xx,yy);
      xx := szorzo * xx;
      yy := szorzo * yy;
      s := Ltrim(format('%6.5f',[xx]))+' '+LTrim(format('%6.5f',[yy]));
      WriteLn(D,s);
      if N=0 then s0 := s;               // Save 0. point
      Inc(H);
    end;
      if FCurve.Closed then
         WriteLn(D,s0);

//         WriteLn(D,'');                  // Üres sor beiktatása

  end;
FINALLY
      if FCurve.Closed then
         WriteLn(D,s0);
    CloseFile(D);
    Loading := False;
    Result := True;
END;
END;

function TCADSource.LoadFromTXT(Filename: STRING): boolean;
begin

end;

function TCADSource.SaveToTXT(Filename: STRING): boolean;
var D: Textfile;
    I,N: Integer;
    xx,yy: TFloat;
    s,s0: string;
    FCurve: TCurve;
BEGIN
Try
  Result := False;
  Loading := True;
  AssignFile(D,Filename);
    Rewrite(D);
    for i:=0 to Pred(FCurvelist.Count) do begin
      FCurve:=Curves[I];
      WriteLn(D,'['+FCurve.Name+']');
      WriteLn(D,'Type='+DrawModeText[Ord(FCurve.Shape)]);
    for N:=0 to Pred(FCurve.FPoints.Count) do begin
      FCurve.GetPoint(N,xx,yy);
      s := IntToStr(N)+'='+Ltrim(format('%6.2f',[xx]))+','+LTrim(format('%6.2f',[yy]));
      WriteLn(D,s);
      if N=0 then s0 := s;               // Save 0. point
    end;
  end;
FINALLY
      if FCurve.Closed then
         WriteLn(D,s0);
    CloseFile(D);
    Loading := False;
    Result := True;
END;
END;

function TCADSource.SaveToCNC(Filename: STRING): boolean;
var D: Textfile;
    I,N: Integer;
    xx,yy: TFloat;
    s,s0: string;
    Num: integer;
    FCurve: TCurve;
BEGIN
Try
  Result := False;
  Loading := True;
  Num := 10;
  AssignFile(D,Filename);
    Rewrite(D);
  for i:=0 to Pred(FCurvelist.Count) do begin
      FCurve:=Curves[I];
      WriteLn(D,'['+FCurve.Name+']');
      WriteLn(D,'Type='+DrawModeText[Ord(FCurve.Shape)]);
    for N:=0 to Pred(FCurve.FPoints.Count) do begin
      FCurve.GetPoint(N,xx,yy);
      s := 'N'+ZeroNum(Num,4)+' X'+Ltrim(format('%6.2f',[xx]))+'Y'+LTrim(format('%6.2f',[yy]));
      WriteLn(D,s);
      Inc(Num,10);
    end;
      if FCurve.Closed then begin
         FCurve.GetPoint(0,xx,yy);
         s := 'N'+ZeroNum(Num,4)+' X'+Ltrim(format('%6.2f',[xx]))+'Y'+LTrim(format('%6.2f',[yy]));
         WriteLn(D,s);
      end;
  end;
FINALLY
    CloseFile(D);
    Loading := False;
    Result := True;
END;
END;

function TCADSource.LoadFromGKOD(Filename: STRING): boolean;
begin

end;

function TCADSource.LoadFromSVG(Filename: STRING): boolean;
begin

end;

function TCADSource.SaveToSVG(Filename: STRING): boolean;
begin

end;

function TCADSource.GetSVGText: STRING;
begin

end;

function TCurveList.LastCurve: TCurve;
begin
     Result := Curves[Count-1];
end;

function TCurveList.LoadCurveFromStream(FileStream: TStream;
  Item: Integer): Boolean;
begin
  Result:=False;
  if not Assigned(FileStream) then Exit;
  FCurve := TCurve.Create;
  if FCurve.LoadCurveFromStream(FileStream) then
     Result := AddCurve(FCurve)>-1;
end;

procedure TCurveList.MoveCurve(AIndex: integer; Ax, Ay: TFloat);
begin
  if InRange(AIndex,0,Pred(Count)) then begin
      Curves[AIndex].MoveCurve(Ax,Ay);
      Changed := True;
  end;
end;

procedure TCurveList.MoveSelectedCurves(Ax, Ay: TFloat);
var i: integer;
begin
  for i:=0 to Pred(Count) do begin
      if Curves[i].Selected then begin
         Curves[i].Pause:=True;
         Curves[i].MoveCurve(Ax,Ay);
         Curves[i].Pause:=False;
         Changed := True;
      end;
  end;
end;

procedure TCurveList.RotateSelectedCurves(Cent: TPoint2d; Angle: TFloat);
var i: integer;
begin
  for i:=0 to Pred(Count) do begin
      FCurve:=Items[i];
      if FCurve.Selected then begin
         FCurve.RotateCurve(Cent, Angle);
         Changed := True;
      end;
  end;
end;


procedure TCurveList.InversSelectedCurves;
var i: integer;
begin
  for i:=0 to Pred(Count) do
      Curves[i].Selected := not Curves[i].Selected;
end;

procedure TCurveList.SelectCurveByName(aName: string);
var n: integer;
begin
   For n:=0 to Pred(Count) do begin
      if Curves[n].Name = aName then
         Curves[n].Selected := True;
   end;
end;

procedure TCurveList.SelectCurve(AIndex: Integer);
begin
  if InRange(AIndex,0,Pred(Count)) then
     Curves[AIndex].Selected := True;
end;

procedure TCurveList.PoligonizeAll(PointCount: integer);
// Total graphic poligonisation (convert to polygon/polyline)
Var i : integer;
begin
  For i:=0 to Pred(Count) do
      Poligonize(Curves[i],PointCount);
  Changed := True;
end;

procedure TCurveList.PoligonizeAllSelected(PointCount: integer);
Var i : integer;
begin
  For i:=0 to Pred(Count) do
  if Curves[i].selected then
      Poligonize(Curves[i],PointCount);
  Changed := True;
end;

// Convert a curve to polygon/polyline:
//    If Count>0 then result curve will be countains count points
procedure TCurveList.Poligonize(Cuv: TCurve; PointCount: integer);
begin
  Cuv.Poligonize(PointCount);
end;

procedure TCurveList.VektorisationAll(MaxDiff: TFloat);
// Total graphic vectorisation
Var i : integer;
begin
Try
  Loading := True;
  For i:=0 to Pred(Count) do
      if Items[i]<>nil then
         Vektorisation(MaxDiff,TCurve(Items[i]));
  Loading := False;
except
end;
end;

procedure TCurveList.VektorisationAllSelected(MaxDiff: TFloat);
Var i    : integer;
begin
Try
  Loading := True;
  For i:=0 to Pred(Count) do
      if Items[i]<>nil then
      if TCurve(Items[i]).selected then
         Vektorisation(MaxDiff,TCurve(Items[i]));
  Loading := False;
except
end;
end;

procedure TCurveList.Vektorisation(MaxDiff: TFloat; Cuv: TCurve);
begin
  Cuv.Vektorisation(MaxDiff);
end;

procedure TCurve.Vektorisation(MaxDiff: TFloat);
var diff    : double;          // eltérés
    i       : integer;
    pp      : pPoints2d;
    kp,vp   : TPoint2D;        // vektor kezdõ és végpontja
    lp      : TPoint2D;
    n0,n,k  : integer;         // n futóindex
    e       : TEgyenesfgv;
    p2d     : TPoint2D;
    pList   : TPointList;
begin

if (Self<>nil) and (Count>2) then
Try
  Try
   lp := LastPoint;
   // Store the Cuv points in dPoints list
   pList := TPointList.Create;
   For i:=0 to Pred(FPoints.Count) do begin
       p2d := Points[i];
       pList.AddPoint(p2d);
   end;
   // First point <> Last point

   If Closed then
   if not pList.EquelPoints(0,pList.Count-1)
   then   pList.AddPoint(pList.Points[0]);

//   then pList.DeletePoint(Cuv.FPoints.Count-1);

   // Push vector points into the Cuv
   ClearPoints;
   n0 := 0;
   n  := 1;
   k  := pList.Count;

   // Ujmódszer
   While (n<=pList.Count) and (n0<=pList.Count) do
   Try
     p2d := pList.Points[n0];
     kp := p2d;
     AddPoint(p2d);
     Inc(n0);
     Dec(k);

     While (n<=pList.Count) and (n0<=pList.Count) do begin
        p2d := pList.Points[n];
        vp  := p2d;
        e := KeTPontonAtmenoEgyenes(kp.x,kp.y,vp.x,vp.y);
        // Vizsgáljuk a közbülsõ pontok eltéréseit az egyenestõl
        For i:=n0 to n do begin
            p2d  := pList.Points[i];
            diff := PontEgyenesTavolsaga(e,p2d);
            if diff>MaxDiff then break;
        end;
        if diff>MaxDiff then begin
           n0 := n-1;      // Az n-1. pont eltérése már jelentõs
           Inc(n);
           break;
        end;
        Inc(n);
     end;
   except
     Break;
   end;

   if not Closed then
      AddPoint(lp);

  finally
     pList.Free;
  end;

except
   pList.Free;
   Exit;
end;
end;



// A pontsûrítés közbülsõ pontok beillesztése Dist távolságonként
procedure TCurveList.PontSurites(Cuv: TCurve; Dist: double);
var x,y         : TFloat;
    d           : TFloat;
    i,j,k       : integer;
    lp          : TPoint2d;  // Last point
    pp,pp1      : TPoint2d;
    dx,dy       : TFloat;
    Angle       : TFloat;
    FC          : TCurve;
begin
Try
   Loading := True;
   // Store the Cuv points in Tempcurve
   Cuv.FillTempCurve;
   FC:=Cuv.TempCurve;

   Case Cuv.Shape of
   dmPolygon,dmPolyLine,dmRectangle,dmLine:
     begin
        if Cuv.Closed then begin
           pp := Cuv.Points[0];
           FC.AddPoint(pp);
        end;
        Cuv.ClearPoints;

        For i:=0 to FC.Count-2 do begin
            pp    := FC.Points[i];
            pp1   := FC.Points[i+1];
            d     := KeTPontTavolsaga(pp.x,pp.y,pp1.x,pp1.y);
            x     := pp.x;
            y     := pp.y;
            k     := Trunc(d/Dist);
            Angle := RelAngle2D(Point2d(x,y),Point2d(pp1.x,pp1.y));
            dx    := Dist * cos(Angle);
            dy    := Dist * sin(Angle);
            if d>Dist then begin
               For j:=0 to k do begin
                 Cuv.AddPoint(x,y);
                 x := x + dx;
                 y := y + dy;
               end;
            end else begin
               Cuv.AddPoint(pp.x,pp.y);
            end;
        end;
       end;
     end;
     If not Cuv.Closed
        then Cuv.AddPoint(FC.LastPoint);
finally
   FC.ClearPoints;
   Loading := False;
end;
end;

procedure TCurveList.PontSuritesAll(Dist: double);
Var
    i    : integer;
begin
  For i:=0 to Pred(Count) do
      PontSurites(TCurve(Items[i]),Dist);
end;

function TCurveList.GetDrawExtension: TRect2d;
var n    : integer;
    x,y  : double;
    R    : TRect2d;
begin
   Result := Rect2d(10e+10,10e+10,-10e+10,-10e+10);
   For n:=0 to Pred(Count) do begin
       R := Curves[n].GetBoundsRect;
        if R.x1<Result.x1 then Result.x1:=R.x1;
        if R.x2>Result.x2 then Result.x2:=R.x2;
        if R.y2>Result.y2 then Result.y2:=R.y2;
        if R.y1<Result.y1 then Result.y1:=R.y1;
   end;
end;

procedure TCurveList.SelectAll(all: boolean);
var i: integer;
    cuv: TCurve;
begin
  Loading := True;
  for i:=0 to Pred(Count) do begin
      Cuv:=Items[i];
      Cuv.Selected := all;
      if not all then
         Cuv.SelectAllPoints(false);
  end;
  Loading := False;
//  if {(not MouseOn) and} (not Loading) then
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.SelectAllInArea(R: TRect2D);
var i: integer;
    cuv: TCurve;
    RR,RC : TRect2d;
begin
  RR := CorrectRealRect(R);
  for i:=0 to Pred(Count) do begin
      Cuv:=Items[i];
      RC := CorrectRealRect(Cuv.BoundsRect);
      If (RR.X1<=RC.X1) and (RR.X2>=RC.X2) and
         (RR.Y1<=RC.Y1) and (RR.Y2>=RC.Y2) then
      begin
         Cuv.Selected := True;
      end;
  end;
//  if not MouseOn then
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.SelectAllInAreaEx(R: TRect2d);
var i,j: integer;
    cuv: TCurve;
    pr: TPointrec;
begin
  for i:=0 to Pred(Count) do begin
      Cuv:=Items[i];
      for j:=0 to Pred(Cuv.Count) do begin
          pr := Cuv.PointRec[j];
          if PontInKep(pr.x,pr.y,R) then
             Cuv.ChangePoint(j,pr.x,pr.y,True);
      end;
  end;
//  if not MouseOn then
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.ClosedAll(all: boolean);
var i: integer;
    cuv: TCurve;
begin
  for i:=0 to Pred(Count) do begin
      Cuv:=Items[i];
      if cuv.Selected then begin
         poligonize(Cuv,0);
         Cuv.Closed := all;
         if all then
            Cuv.Shape := dmPolygon
         else
            Cuv.Shape := dmPolyline
      end;
  end;
//  if Assigned(FChangeAll) then FChangeAll(Self);
  Changed := True;
end;

// A kijelölt objektumokat szakaszokká bontja
procedure TCurveList.BombAll;
var i,j,h: integer;
    cuv: TCurve;
    p1,p2: TPoint2d;
begin
  for i:=Pred(Count) downto 0 do begin
      Cuv:=Items[i];
      if cuv.Selected then begin
      for j:=0 to Pred(cuv.Count)-1 do begin
          h := MakeCurve('Line',-1,dmPolyline,true,true,false);
          p1 := cuv.Points[j];
          p2 := cuv.Points[j+1];
          AddPoint(h,p1);
          AddPoint(h,p2);
      end;
      if cuv.Closed then begin
          h := MakeCurve('Line',-1,dmPolyline,true,true,false);
          p1 := cuv.Points[Pred(cuv.Count)];
          p2 := cuv.Points[0];
          AddPoint(h,p1);
          AddPoint(h,p2);
      end;
      DeleteCurve(i);
      end;
  end;
//  if Assigned(FChangeAll) then FChangeAll(Self);
  Changed := True;
end;

procedure TCurveList.SelectAllShape(shape: TDrawMode; all: boolean);
var i: integer;
    cuv: TCurve;
begin
  for i:=0 to Pred(Count) do begin
      Cuv:=Items[i];
      if cuv.Shape=shape then
         Cuv.Selected := all;
  end;
//  if Assigned(FChangeAll) then FChangeAll(Self);
  Changed := True;
end;

procedure TCurveList.SelectAllPolylines;
begin
  SelectAllShape(dmPolyline,True);
end;

procedure TCurveList.SelectAllPolygons;
begin
  SelectAllShape(dmPolygon,True);
  SelectAllShape(dmRectangle,True);
end;

procedure TCurveList.SelectParentObjects;
var i: integer;
begin
  Loading := True;
  for i:=0 to Pred(Count) do
      Curves[i].Selected := IsParent(i);
  Loading := False;
end;

procedure TCurveList.SelectChildObjects;
var i: integer;
begin
  Loading := True;
  InitParentObjects;
  for i:=0 to Pred(Count) do
      Curves[i].Selected := Curves[i].ParentID>-1;
  Loading := False;
end;

function TCurveList.GetSelectedCount: integer;
var i: integer;
begin
   Result := 0;
   For i:=0 to Pred(Count) do
       if Curves[i].Selected then
         Inc(Result);
end;

// Return the least rect of selected objects
function TCurveList.GetSelectArea(var RArea: TRect2d): boolean;
var i: integer;
    boundsR: TRect2d;
begin
   Result := False;
   RArea := Rect2d(MaxDouble,MaxDouble,MinDouble,MinDouble);
   if GetSelectedCount>0 then
   begin
   For i:=0 to Pred(Count) do begin
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
procedure TCurveList.ChangeSelectedShape(newShape: TDrawMode);
var i: integer;
begin
  for i:=0 to Pred(Count) do
  begin
      if Curves[i].Selected then
         Curves[i].Shape := newShape;
      Curves[i].Closed := newShape in [dmPolygon];
  end;
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.EnabledAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(Count) do
      Curves[i].Enabled:=all;
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.SignedAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(Count) do
      Curves[i].Signed:=all;
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.CrossedAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(Count) do
      Curves[i].Crossed:=all;
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.SortedAll(all: boolean);
var i: integer;
begin
  For i:=0 to Pred(Count) do
      Curves[i].Sorted:=all;
//  if Assigned(FChangeAll) then FChangeAll(Self);
end;

procedure TCurveList.SignedNotCutting;
var i,j,k: integer;
    BaseCurve,Cuv: TCurve;
    p,p0: TPoint2d;
    R1,R2: TRect2d;
begin
  loading := True;
  CrossedAll(False);
  // Crossed=True, ha valamely objektum át van vágva (rajzon piros szín)
  For i:=0 to Pred(Count) do begin
      BaseCurve:=Items[i];
      R1 := BaseCurve.BoundsRect;
      if BaseCurve.Closed then
         For j:=0 to Pred(Count) do
         if (i<>j) then begin
             Cuv:=Items[j];
             R2 := Cuv.BoundsRect;
             if IntersectRect2D(R1,R2) then
             For k:=0 to Cuv.Count-2 do
             begin
                p:=Cuv.GetPoint2d(k);
                p0:=Cuv.GetPoint2d(k+1);
                if BaseCurve.IsCutLine(p0,p) then begin
                   BaseCurve.Crossed:=True;
                   Break;
                end;
             end;
         end;
  end;
  loading := False;
end;

function TCurveList.GetSignedCount: integer;
var i: integer;
begin
   Result := 0;
   For i:=0 to Pred(Count) do
       if Curves[i].Signed then
         Inc(Result);
end;

// All polygon will be direct
// Minden polygon körüljárása az óramutató irányában
procedure TCurveList.SetAllDirect;
var i: integer;
    Cuv: TCurve;
begin
    for i:=0 to Pred(Count) do
    begin
        Cuv := Curves[i];
        if Cuv.Shape=dmPolygon then
        if not Cuv.IsDirect then
           Cuv.InversPointOrder;
    end;
end;


// Selected nyílt objektumokat összefûzi egy zárt alakzattá
procedure TCurveList.JoinSelected;
var i,n: integer;
    Cuv: TCurve;
    selArray: array of integer;
    NextIdx  : integer;

    function InitSelArray: integer;
    var ii,jj: integer;
    begin
       jj := 0;
       SetLength(selArray,GetSelectedCount);
       for ii:=0 to Pred(Count) do begin
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
       Felfûzi a Cuv zárt objektumra
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
  SetLength(selArray,0);
  DeleteSelectedCurves;
  Changed := True;
End;
end;

// Mirror selected object. Centrum is the midle line of select area
procedure TCurveList.MirrorSeledted(Horiz, Vert: boolean);
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

         For i:=0 to Pred(Count) do begin
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
    end;
  end;
end;

procedure TCurveList.Eltolas(dx, dy: double);
var n,j: integer;
    x,y: double;
begin
   For n:=0 to Pred(Count) do begin
      FCurve:=Items[n];
      J:=Pred(FCurve.FPoints.Count);
      FCurve.MoveCurve(dx,dy);
      Changed := True;
   end;
end;

procedure TCurveList.Nyujtas(tenyezo: double);
var n,i,j: integer;
    x,y: double;
begin
   For n:=0 to Pred(Count) do begin
      FCurve:=Items[n];
      If FCurve.Enabled then begin
      J:=Pred(FCurve.FPoints.Count);
      for I:=0 to J do
      begin
        FCurve.GetPoint(i,X,Y);
        x := tenyezo * x;
        y := tenyezo * y;
        FCurve.ChangePoint(i,x,y);
        Changed := True;
      end;
      end;
   end;
end;

procedure TCurveList.CentralisNyujtas(Cent: TPoint2d; tenyezo: double);
var n,i,j: integer;
    x,y: double;
begin
  For n:=0 to Pred(Count) do begin
      FCurve:=Items[n];
      If FCurve.Enabled then
         FCurve.MagnifyCurve(Cent, tenyezo);
  end;
  Changed := True;
end;

procedure TCurveList.MagnifySelected(Cent: TPoint2d; Magnify: TFloat);
var n,i,j: integer;
    x,y: double;
begin
   For n:=0 to Pred(Count) do begin
      FCurve:=Items[n];
      J:=Pred(FCurve.FPoints.Count);
      if FCurve.Selected then begin
         FCurve.MagnifyCurve(Cent, Magnify);
         Changed := True;
      end;
   end;
end;

procedure TCurveList.MirrorHorizontal;
var R: TRect2d;
    i,j: integer;
    x,y,h: double;
begin
  R := GetDrawExtension;
  h := (R.x2 + R.x1)/2;     // Középvonal x értéke
  for i:=0 to Pred(Count) do
    for j := 0 to Pred(Curves[i].Count) do
    begin
      Curves[i].GetPoint(j,x,y);
      Curves[i].ChangePoint(j,h+(h-x),y);
      Changed := True;
    end;
end;

procedure TCurveList.MirrorVertical;
var R: TRect2d;
    i,j: integer;
    x,y,h: double;
begin
  R := GetDrawExtension;
  h := (R.y2 + R.y1)/2;     // Középvonal y értéke
  for i:=0 to Pred(Count) do
    for j := 0 to Pred(Curves[i].Count) do
    begin
      Curves[i].GetPoint(j,x,y);
      Curves[i].ChangePoint(j,x,h+(h-y));
      Changed := True;
    end;
end;

procedure TCurveList.MirrorCentral;
var R: TRect2d;
    i,j: integer;
    x,y,hx,hy: double;
begin
  R := GetDrawExtension;
  hx := (R.x2 + R.x1)/2;     // Középpont x értéke
  hy := (R.y2 + R.y1)/2;     // Középpont x értéke
  for i:=0 to Pred(count) do
    for j := 0 to Pred(Curves[i].Count) do
    begin
      Curves[i].GetPoint(j,x,y);
      Curves[i].ChangePoint(j,hx+(hx-x),hy+(hy-y));
    end;
end;

// A zárt poligonok ParentID-jét beállítja, ha van szülõje
procedure TCurveList.InitParentObjects;
var i: integer;
begin
  for i:=0 to Pred(Count) do begin
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
function TCurveList.IsParent(AIndex: Integer): boolean;
begin
  Result := False;
  if AIndex>-1 then
     Result := GetParentObject(AIndex)=-1;
end;

// Megvizsgálja, hogy a pont körüli objektumnak van-e szüleje, azaz
// olyan objektum, aminek a belselyében található
// True = ha szülõ objektum
function TCurveList.IsParent(x,y: TFloat): boolean;
begin
  Result := False;
  Result := GetParentObject(x,y)=-1;
end;

function TCurveList.GetInnerObjectsCount(AIndex: Integer): integer;
// Result = 0 or inner objects' count;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    i       : integer;
begin
  Result := GetInnerObjectsCount(Items[AIndex]);
end;

function TCurveList.GetInnerObjectsCount(Cuv: TCurve): integer;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    i       : integer;
begin
  Result := 0;
  OurRect:= Cuv.Boundsrect;
  For i:=0 to Pred(Count) do begin
      FCurve := Items[I];
      if FCurve.Visible and (FCurve<>Cuv) then begin
         inRect := FCurve.Boundsrect;
         If RectInRect2D(OurRect,inRect) then
         if Cuv.IsInCurve(FCurve.Points[0],2)=icIn then begin
            Inc(Result);
         end;
      end;
  end;
end;

// Megkeresi, hogy a sokszög melyik legkisebbnek alakzat belselyében van
// Ha nincs befoglalója, akkor Result=-1
// Ha van, akkor annak az ID-jével tér vissza
function TCurveList.GetParentObject(AIndex: Integer): integer;
Var OurRect : TRect2d;
    inRect  : TRect2d;
    oRect   : TRect2d;
    Cuv     : TCurve;
    p,p1,p2 : TPoint2d;
    i,j     : integer;
    maxy    : double;
begin

  Result := -1;
  FCurve := Items[AIndex];
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
function TCurveList.GetParentObject(x,y: TFloat): integer;
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
  For i:=0 to Pred(Count) do begin
      Cuv := Items[I];
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

function TCurveList.GetRealParentObject(AIndex: Integer): integer;
Var CuvParent : TCurve;
    Cuv     : TCurve;
    i       : integer;
begin
  Result := -1;
  Cuv := Curves[AIndex];
  for I := 0 to Pred(Count) do
    if i<>AIndex then begin
       CuvParent := Curves[i];
       if CuvParent.IsInCurve(Cuv.Points[0],2)=icIn then
       begin
          Result := i;
          Exit;
       end;
    end;
end;

// ------------------------------------------------------------------------
{ TBlock }

constructor TBlock.Create(const AName: String);
begin
  BlockName  := AName;
  FCurveList := TCurveList.Create(Self);
  FBasePoint := TPoint2dObj.Create;
end;

destructor TBlock.Destroy;
begin
  FBasePoint.Free;
  if FCurveList<>nil then
     FCurveList.Free;
  inherited;
end;

function TBlock.GetBoundsRect: TRect2d;
begin
   Result := FCurveList.GetDrawExtension;
end;

function TBlock.LoadBlockFromStream(Stm: TStream): boolean;
var
  NewBlockData: TNewBlockData;
  p : TPointRec;
  N,i : Integer;
begin
  Try
    Result:=False;
    N := SizeOf(TNewBlockData);
    I := stm.Size - stm.Position;
    stm.ReadBuffer(NewBlockData,SizeOf(TNewBlockData));
    for i := 0 to Pred(NewBlockData.CurveCount) do
        FCurveList.LoadCurvesFromStream(stm);
    BlockName   := NewBlockData.BlockName;
    BasePoint.x := NewBlockData.BasePoint.X;
    BasePoint.y := NewBlockData.BasePoint.Y;
    Layer       := NewBlockData.Layer;
  except
    Exit;
  end;
end;

function TBlock.SaveBlockToStream(Stm: TStream): boolean;
var
  NewBlockData: TNewBlockData;
  p : TPointRec;
  N : Integer;
begin
  Result:=False;
  if Self<>nil then
  Try
    NewBlockData.BlockName := BlockName;
    NewBlockData.BasePoint := Point2d(BasePoint.x,BasePoint.y);
    NewBlockData.Layer     := Layer;
    NewBlockData.CurveCount:= FCurveList.Count;
    Stm.WriteBuffer(NewBlockData,SizeOf(TNewBlockData));
    Result:=FCurveList.SaveCurvesToStream(stm);
  except
    Exit;
  end;
end;

// ------------------------------------------------------------------------

{ TLayer }

constructor TLayer.Create(Idx: Byte);
begin
  fPen := TPen.Create;
  fBrush := TBrush.Create;
  fID := Idx;
  fVisible := True;
  Tag := 0;
end;

destructor TLayer.Destroy;
begin
  fPen.Free;
  fBrush.Free;
  inherited;
end;

procedure TLayer.LoadFromStream(const Stream: TStream);
begin

end;

procedure TLayer.SaveToStream(const Stream: TStream);
begin

end;

procedure TLayer.SetBrush(const Value: TBrush);
begin
  fBrush := Value;
end;

procedure TLayer.SetName(const Value: TLayerName);
begin
  fName := Value;
end;

procedure TLayer.SetPen(const Value: TPen);
begin
  fPen := Value;
end;

{------------------------------------------------------------------------------}

constructor TDXFOut.Create(AFromXMin,AFromYMin,AFromXMax,AFromYMax,
                           AToXMin,AToYMin,AToXMax,AToYMax,ATextHeight: TFloat; ADecimals: Byte);
begin
  inherited Create;
  FromXMin:=AFromXMin;
  FromYMin:=AFromYMin;
  FromXMax:=AFromXMax;
  FromYMax:=AFromYMax;
  ToXMin:=AToXMin;
  ToYMin:=AToYMin;
  ToXMax:=AToXMax;
  ToYMax:=AToYMax;
  TextHeight:=ATextHeight;
  Decimals:=ADecimals;
  StringList:=TStringList.Create;
end;

destructor TDXFOut.Destroy;
begin
  StringList.Free;
  inherited Destroy;
end;

procedure TDXFOut.Header;
begin
  LayerName:='0';
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('HEADER');
  StringList.Add('9');
  StringList.Add('$LIMMIN');
  StringList.Add('10');
  StringList.Add(FToA(ToXMin));
  StringList.Add('20');
  StringList.Add(FToA(ToYMin));
  StringList.Add('9');
  StringList.Add('$LIMMAX');
  StringList.Add('10');
  StringList.Add(FToA(ToXMax));
  StringList.Add('20');
  StringList.Add(FToA(ToYMax));
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('TABLES');
  StringList.Add('0');
  StringList.Add('TABLE');
  StringList.Add('2');
  StringList.Add('LAYER');
  StringList.Add('70');
  StringList.Add('1');
  StringList.Add('0');
  StringList.Add('LAYER');
  StringList.Add('2');
  StringList.Add('0');
  StringList.Add('70');
  StringList.Add('64');
  StringList.Add('62');
  StringList.Add('7');
  StringList.Add('6');
  StringList.Add('CONTINUOUS');
  StringList.Add('0');
  StringList.Add('ENDTAB');
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('SECTION');
  StringList.Add('2');
  StringList.Add('ENTITIES');
end;

function TDXFOut.FToA(F: TFloat): Str32;
var
  I: Integer;
begin
  Result:=FloatToStrF(F,ffFixed,16,Decimals);
  I:=Pos(',',Result);
  if I > 0 then Result[I]:='.';
end;

function TDXFOut.ToX(X: TFloat): TFloat;
var
  Factor,FromDif: TFloat;
begin
  FromDif:=FromXMax - FromXMin;
  if FromDif <> 0.0 then Factor:=(ToXMax - ToXMin) / FromDif else Factor:=1.0;
  Result:=X * Factor;
end;

function TDXFOut.ToY(Y: TFloat): TFloat;
var
  Factor,FromDif: TFloat;
begin
  FromDif:=FromYMax - FromYMin;
  if FromDif <> 0.0 then Factor:=(ToYMax - ToYMin) / FromDif else Factor:=1.0;
  Result:=Y * Factor;
end;

procedure TDXFOut.SetLayer(const Name: Str32);
begin
  LayerName:=Name;
end;

procedure TDXFOut.Layer;
begin
  StringList.Add('8');
  StringList.Add(LayerName);
end;

procedure TDXFOut.StartPoint(X,Y,Z: TFloat);
begin
  StringList.Add(' 10');
  StringList.Add(FToA(X));
  StringList.Add(' 20');
  StringList.Add(FToA(Y));
  StringList.Add(' 30');
  StringList.Add(FToA(Z));
end;

procedure TDXFOut.EndPoint(X,Y,Z: TFloat);
begin
  StringList.Add(' 11');
  StringList.Add(FToA(X));
  StringList.Add(' 21');
  StringList.Add(FToA(Y));
  StringList.Add(' 31');
  StringList.Add(FToA(Z));
end;

procedure TDXFOut.AddText(const Txt: Str32);
begin
  StringList.Add('1');
  StringList.Add(Txt);
end;

procedure TDXFOut.StartPolyLine(Closed: Boolean);
var
  Flag : Byte;
begin
  StringList.Add('0');
  StringList.Add('POLYLINE');
  Layer;
  StringList.Add('66');
  StringList.Add('1');
  StartPoint(0,0,0);
  Flag:=8;
  if Closed then Flag:=Flag or 1;
  StringList.Add('70');
  StringList.Add(IntToStr(Flag));
end;

procedure TDXFOut.Vertex(X,Y,Z: TFloat);
var
 Flag : Byte;
begin
  StringList.Add('0');
  StringList.Add('VERTEX');
  Layer;
  StartPoint(X,Y,Z);
  StringList.Add('70');
  Flag:=32;
  StringList.Add(IntToStr(Flag));
end;

procedure TDXFOut.EndPolyLine;
begin
  StringList.Add('0');
  StringList.Add('SEQEND');
  Layer;
end;

procedure TDXFOut.Line(X1,Y1,Z1,X2,Y2,Z2: TFloat);
begin
  StringList.Add('0');
  StringList.Add('LINE');
  Layer;
  StartPoint(X1,Y1,Z1);
  EndPoint(X2,Y2,Z2);
end;

procedure TDXFOut.Point(X,Y,Z: TFloat);
begin
  StringList.Add('0');
  StringList.Add('POINT');
  Layer;
  StartPoint(X,Y,Z);
end;

procedure TDXFOut.DText(X,Y,Z,Height,Angle: TFloat; const Txt: Str32);
begin
  StringList.Add('0');
  StringList.Add('TEXT');
  Layer;
  StartPoint(X,Y,Z);
  StringList.Add('40');
  StringList.Add(FToA(Height));
  AddText(Txt);
  StringList.Add('50');
  StringList.Add(FToA(Angle));
end;

procedure TDXFOut.Trailer;
begin
  StringList.Add('0');
  StringList.Add('ENDSEC');
  StringList.Add('0');
  StringList.Add('EOF');
end;

{ TInsertBlock }

constructor TInsertBlock.Create(const AName: String);
begin
  inherited;
  FTranslate := TPoint2dObj.Create;
  FZoom      := TPoint2dObj.Create;
end;

destructor TInsertBlock.Destroy;
begin
  FTranslate.Free;
  FZoom.Free;
  inherited;
end;

{ TBlockList }

procedure TBlockList.AddBlock(Value: TBlock);
begin
//   block :=
end;

function TBlockList.GetBlock(idx: integer): TBlock;
begin
  Result := TBlock(Items[idx]);
end;

function TBlockList.GetBlockByName(Name: string): TBlock;
var i: integer;
begin
  Result := nil;
  for I := 0 to Pred(Count) do
      if Items[i].BlockName = Name then
         Result := Items[i];
end;

function TBlockList.LoadBlockFromStream(Stm: TStream; Item: Integer): boolean;
var block: TBlock;
begin
  Result:=False;
  Block:=TBlock.Create('');
  Result := Block.LoadBlockFromStream(Stm);
  Add(Block);
end;

function TBlockList.SaveBlockToStream(Stm: TStream; Item: Integer): boolean;
var block: TBlock;
begin
Try
  Result:=False;
  if not InRange(Item,0,Pred(Count)) or not Assigned(Stm) then Exit;
  Block:=TBlock.Create('');
  Block:=Items[Item];
  Result := FBlock.SaveBlockToStream(Stm);
Except
  Result := False;
End;
end;

procedure TBlockList.SetBlock(idx: integer; const Value: TBlock);
begin
  Items[idx] := Value;
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

  // Oldal felezõ pontok mozgatása
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

// Orto mode: az oldalfelezõ pontok a téglalap oldalait ||-an tolja el;
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

{ TCADSource }

procedure TCADSource.Clear;
begin
  FCurveList.ClearList;
  FBlockList.Clear;
  FLayerList.Clear;
end;

constructor TCADSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurveList   := TCurveList.Create(AOwner);
  FBlockList   := TBlockList.Create;
  FLayerList   := TLayerList.Create;
  EnableRecall := True;
  FAppend      := False;
end;

destructor TCADSource.Destroy;
begin
  fEnableRecall:=False;
  inherited Destroy;
end;

procedure TCADSource.SetEnableRecall(AValue: boolean);
begin
  if fEnableRecall=AValue then Exit;
  fEnableRecall:=AValue;
  Recall;
end;

function TCADSource.GetBlock(idx: integer): TBlock;
begin
  Result := FBlockList.Block[idx];
end;

function TCADSource.GetBlockBoundsRect(cuv: TCurve): TRect2d;
var block: TBlock;
begin
  if cuv.Shape=dmInsert then
  begin
    block := FBlockList.GetBlockByName(cuv.BlockParams.BlockName);
    Result := GetBlockBoundsRect(block,cuv);
  end;
end;

procedure TCADSource.SetLayer(idx: integer; const Value: TLayer);
begin
  Layers[idx]:=Value;
end;

function TCADSource.GetCurve(idx: integer): TCurve;
begin
  Result := FCurveList.Curves[idx];
end;

function TCADSource.GetBlockBoundsRect(BR: TBlock;cuv: TCurve): TRect2d;
VAR II: integer;
    jj: integer;
    p: TPoint2d;
    cu: TCurve;
begin
Try
  Result := Rect2d(10e+10,10e+10,-10e+10,-10e+10);
  for ii := 0 to Pred(BR.FCurveList.Count) do
  begin
    cu := BR.FCurveList.Items[ii];
    cu.FBlockParams.BlockName := BR.FBlockName;
    for jj := 0 to Pred(cu.Count) do
    begin
        p := cu.Points[jj];
        Rotate2D(p,cuv.BlockParams.fAngle);
        p.X := p.X*cuv.BlockParams.Magnify+cuv.Points[0].x;
        p.Y := p.Y*cuv.BlockParams.Magnify+cuv.Points[0].y;
        if p.x<Result.x1 then Result.x1:=p.x;
        if p.x>Result.x2 then Result.x2:=p.x;
        if p.y<Result.y1 then Result.y1:=p.y;
        if p.y>Result.y2 then Result.y2:=p.y;
    end;
  end;
Finally
End;
end;

function TCADSource.GetDrawExtension: TRect2d;
var R       : TRect2d;
    i       : integer;
    block   : TBlock;
    cent    : TPoint2d;
    x,y     : TFloat;

begin
  Result := FCurveList.GetDrawExtension;
  if FCurveList.ShapeCount(dmInsert)>0 then
  for i:=0 to Pred(FCurveList.Count) do begin
      if Curves[i].Shape=dmInsert then
      begin
        block := FBlockList.GetBlockByName(Curves[i].BlockParams.BlockName);
        if block<>nil then
        begin
           R := GetBlockBoundsRect(block,Curves[i]);
           Curves[i].BoundsRect := R;
           if R.x1<Result.x1 then Result.x1:=R.x1;
           if R.x2>Result.x2 then Result.x2:=R.x2;
           if R.y2>Result.y2 then Result.y2:=R.y2;
           if R.y1<Result.y1 then Result.y1:=R.y1;
        end;
      end;
  end;
end;

function TCADSource.GetLayer(idx: integer): TLayer;
begin
  Result := TLayer(FLayerList[idx]);
end;

function TCADSource.LoadFromDXF(const FileName: string): Boolean;
var dxfObj : TALCustomDXF;
begin
Try
   dxfObj := TALCustomDXF.Create(FileName);
   dxfObj.CadSource := Self;
   dxfObj.AutoPoligonize := dxfObj.CadSource.AutoPoligonize;
   dxfObj.GetFullDraw(FileName,Self);
finally
   dxfObj.FreeInstance;
end;
end;

function TCADSource.LoadFromFile(fn: TFileName): boolean;
var ext: string;
begin
  Result  := False;
  if not FileExists(fn) then Exit;
  try
    Result := True;
    if not Append then
       Clear;
    ext := UpperCase(ExtractFileExt(fn));
    if ext = '.SBN' then
       Result := LoadFromSBN(fn);
    if ext = '.PLT' then
       LoadFromPLT(fn);
    if ext = '.DXF' then
       LoadFromDXF(fn);
    if ext = '.DAT' then
       LoadFromSVG(fn);
    if ext = '.SVG' then
       LoadFromDAT(fn);
    if Pos(ext,'.EIA.NC.TXT')>0 then
       LoadFromGKOD(fn);
  finally
    Loading := False;
    Recall;
  end;
end;

function TCADSource.LoadFromSBN(fn: TFileName): boolean;
var
  FileStream: TFileStream;
  GraphData : TNewGraphData;
  BlockData : TNewBlockData;
  FCurve    : TCurve;
  FBlock    : TBlock;
  N,I: Integer;
begin
    FileStream:=TFileStream.Create(fn,fmOpenRead);
    try
      Try
          FileStream.Position:=0;
          FileStream.ReadBuffer(GraphData,SizeOf(TNewGraphData));
          // Load Curves
          for N:=0 to Pred(GraphData.Curves) do
          begin
              FCurve := TCurve.Create;
              if FCurve.LoadCurveFromStream(FileStream) then
                 FCurvelist.AddCurve(FCurve)
              else
              begin
                 FileStream.Free;
                 exit;
              end;
          end;
          // Load blocks
          if FileStream.Position<FileStream.Size then
          begin
          for N:=0 to Pred(GraphData.Blocks) do
//              FBlockList.LoadBlockFromStream(FileStream,N);
          begin
              FileStream.ReadBuffer(BlockData,SizeOf(TNewBlockData));
              FBlock := TBlock.Create(BlockData.BlockName);
              FBlock.FBasePoint.x := BlockData.BasePoint.X;
              FBlock.FBasePoint.y := BlockData.BasePoint.Y;
              FBlock.FLayer       := BlockData.Layer;
              for I := 0 to Pred(BlockData.CurveCount) do
              begin
                  FCurve := TCurve.Create;
                  if FCurve.LoadCurveFromStream(FileStream) then
                     FBlock.FCurvelist.AddCurve(FCurve);
              end;
              FBlockList.Add(FBlock);
          end;
          end;

      finally
             FileStream.Free;
             Result:=True;
      end;
    except
      if FileStream<>nil then FileStream.Free;
      Result:=False;
    end;
end;

function TCADSource.SaveToSBN(fn: TFileName): boolean;
var
  FileStream: TFileStream;
  NewGraphData: TNewGraphData;
  N: Integer;
begin
  Result:=False;
  try
    FileStream:=TFileStream.Create(fn,fmCreate);
    try
         FileStream.Position:=0;
         NewGraphData.Copyright := 'StellaFactory Obelisk Sablon Ver 2';
         NewGraphData.Version   := 2;
         NewGraphData.GraphTitle:=fn;
         NewGraphData.Curves:=FCurveList.Count;
         NewGraphData.Blocks:=FBlockList.Count;
         FileStream.Write(NewGraphData,SizeOf(NewGraphData));

         for N:=0 to Pred(FCurveList.Count) do
             FCurveList.SaveCurveToStream(FileStream,N);
         for N:=0 to Pred(FBlockList.Count) do
             FBlockList.SaveBlockToStream(FileStream,N);

         Result:=True;
    except
      Result:=False;
    end;
  finally
    FileStream.Free;
    Changed := False;
  end;
end;

function TCADSource.SaveToStream(stm: TStream): boolean;
var N: integer;
    NewGraphData: TNewGraphData;
begin
  Result:=False;
  if stm<>nil then
  try
         stm.Seek(0,0);
         NewGraphData.Copyright := 'StellaFactory Obelisc Sablon Ver 2';
         NewGraphData.Version   := 2;
         NewGraphData.GraphTitle:='';
         NewGraphData.Curves:=FCurveList.Count;
         NewGraphData.Blocks:=FBlockList.Count;
         stm.Write(NewGraphData,SizeOf(NewGraphData));
         for N:=0 to Pred(FCurveList.Count) do
             FCurveList.SaveCurveToStream(stm,N);
         for N:=0 to Pred(FBlockList.Count) do
             FBlockList.SaveBlockToStream(stm,N);
  finally
    Result := True;
  end;
end;

function TCADSource.LoadFromStream(stm: TStream): boolean;
var N: integer;
  GraphData : TNewGraphData;
  BlockData : TNewBlockData;
begin
  Result:=False;
  if stm<>nil then
  try
         stm.Seek(0,0);
         if not Append then
            Clear;
         stm.ReadBuffer(GraphData,SizeOf(TNewGraphData));
         for N:=0 to Pred(GraphData.Curves) do
             FCurveList.LoadCurveFromStream(stm,N);
         for N:=0 to Pred(GraphData.Blocks) do
             FBlockList.LoadBlockFromStream(stm,N);
  finally
    Result := True;
  end;
end;

procedure TCADSource.SetBlock(idx: integer; const Value: TBlock);
begin
  FBlockList.Block[idx] := Value;
end;

procedure TCADSource.SetBlockBoundsRect(cuv: TCurve);
begin
  cuv.BoundsRect := GetBlockBoundsRect(cuv);
end;

procedure TCADSource.SetCurve(idx: integer; AValue: TCurve);
begin
  FCurveList.Curves[idx] := AValue;
end;

procedure TCADSource.Recall;
begin
  if fEnableRecall then begin
     if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

{ TCadPens }

procedure TCadPens.SetpClosed(AValue: TPen);
begin
  fpClosed:=AValue;
  Change;
end;

procedure TCadPens.SetpContour(AValue: TPen);
begin
  fpContour:=AValue;
  Change;
end;

procedure TCadPens.SetbFillBrush(const Value: TBrush);
begin
  fbFillBrush := Value;
  Change;
end;

procedure TCadPens.SetpBasePen(AValue: TPen);
begin
  fpBasePen:=AValue;
  Change;
end;

procedure TCadPens.SetpCrossed(AValue: TPen);
begin
  fpCrossed:=AValue;
  Change;
end;

procedure TCadPens.SetpOpened(AValue: TPen);
begin
  fpOpened:=AValue;
  Change;
end;

procedure TCadPens.SetpSelected(AValue: TPen);
begin
  fpSelected:=AValue;
  Change;
end;

procedure TCadPens.SetpSigned(AValue: TPen);
begin
  fpSigned:=AValue;
  Change;
end;

procedure TCadPens.SetpSorted(AValue: TPen);
begin
  fpSorted:=AValue;
  Change;
end;

constructor TCadPens.Create;
begin
  inherited Create;
  fpBasePen  := TPen.Create;
  fpClosed   := TPen.Create;
  fpOpened   := TPen.Create;
  fpSelected := TPen.Create;
  fpSigned   := TPen.Create;
  fpCrossed  := TPen.Create;
  fpSorted   := TPen.Create;
  fpContour  := TPen.Create;
  fbFillBrush:= TBrush.Create;
  with fpBasePen do begin
       Color := clBlack;
       Width := 1;
       Style := psSolid;
  end;
  with fpClosed do begin
       Color := clBlack;
       Width := 2;
       Style := psSolid;
  end;
  with fpOpened do begin
       Color := clGray;
       Width := 1;
       Style := psDot;
  end;
  with fpSelected do begin
       Color := clBlue;
       Width := 3;
       Style := psSolid;
  end;
  with fpSigned do begin
       Color := clRed;
       Width := 4;
       Style := psSolid;
  end;
  with fpContour do begin
       Color := clRed;
       Width := 1;
       Style := psSolid;
  end;
  with fpCrossed do begin
       Color := clRed;
       Width := 4;
       Style := psSolid;
  end;
  with fbFillBrush do begin
       Color := clSilver;
       Style := bsSolid;
  end;
end;

destructor TCadPens.Destroy;
begin
  pBasePen.Free;
  pClosed.Free;
  pOpened.Free;
  pSelected.Free;
  pSigned.Free;
  pCrossed.Free;
  pSorted.Free;
  pContour.Free;
  inherited Destroy;
end;

procedure TCadPens.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


{ TLayerList }

function TLayerList.GetLayer(idx: integer): TLayer;
begin
  Result := TLayer(Items[idx]);
end;

function TLayerList.GetLayerByName(Name: string): TLayer;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
      if Layer[i].Name=Name then
      begin
         Result := Layer[i];
         Exit;
      end;
end;

function TLayerList.GetLayerIndex(Name: string): byte;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
      if Layer[i].Name=Name then
      begin
         Result := i;
         Exit;
      end;
end;

procedure TLayerList.SetLayer(idx: integer; const Value: TLayer);
begin
  Items[idx]:=Value;
end;

{ TBlockParams }

constructor TBlockParams.Create;
begin
  inherited Create;
  FBlockname  := '';
  FBasePoint  := TPoint2dObj.Create;
  FAngle      := 0;
  FMagnify    := 1;
end;

destructor TBlockParams.Destroy;
begin
  FBasePoint.Free;
  inherited;
end;

function TBlockParams.LoadFromStream(FileStream: TStream): Boolean;
begin

end;

function TBlockParams.SaveToStream(FileStream: TStream): Boolean;
begin

end;

procedure TBlockParams.SetAngle(const Value: TFloat);
begin
  fAngle := Value;
end;

procedure TBlockParams.SetBlockName(const Value: Str32);
begin
  FBlockName := Value;

end;

//===================== utils ============================

// CAD Standard colors conversion to valid RGB colors
function AUTOCADStandardColor(X:TgaColor):TColor;
begin
  { Az AUTOCAD standard szineivel tér vissza}
  case x of
    gaByBlock     : Result:=clBlack;
    gaByLayer     : Result:=clBlack;
    gaRed         : Result:=clRed;
    gaYellow      : Result:=clYellow;
    gaGreen       : Result:=clGreen;
    gaCyan        : Result:=clTeal;
    gaBlue        : Result:=clBlue;
    gaMagenta     : Result:=clPurple;
    gaWhite       : Result:=clBlack; //clWhite;
    //0  : Result:=RGB(0,0,0);
    //1  : Result:=RGB(255,0,0);
    //2  : Result:=RGB(255,255,0);
    //3  : Result:=RGB(0,255,0);
    //4  : Result:=RGB(0,255,255);
    //5  : Result:=RGB(0,0,255);
    //6  : Result:=RGB(255,0,255);
    //7  : Result:=RGB(255,255,255);
    8  : Result:=RGB(128,128,128);
    9  : Result:=RGB(192,192,192);
    10  : Result:=RGB(255,0,0);
    11  : Result:=RGB(255,127,127);
    12  : Result:=RGB(204,0,0);
    13  : Result:=RGB(204,102,102);
    14  : Result:=RGB(153,0,0);
    15  : Result:=RGB(153,76,76);
    16  : Result:=RGB(127,0,0);
    17  : Result:=RGB(127,63,63);
    18  : Result:=RGB(76,0,0);
    19  : Result:=RGB(76,38,38);
    20  : Result:=RGB(255,63,0);
    21  : Result:=RGB(255,159,127);
    22  : Result:=RGB(204,51,0);
    23  : Result:=RGB(204,127,102);
    24  : Result:=RGB(153,38,0);
    25  : Result:=RGB(153,95,76);
    26  : Result:=RGB(127,31,0);
    27  : Result:=RGB(127,79,63);
    28  : Result:=RGB(76,19,0);
    29  : Result:=RGB(76,47,38);
    30  : Result:=RGB(255,127,0);
    31  : Result:=RGB(255,191,127);
    32  : Result:=RGB(204,102,0);
    33  : Result:=RGB(204,153,102);
    34  : Result:=RGB(153,76,0);
    35  : Result:=RGB(153,114,76);
    36  : Result:=RGB(127,63,0);
    37  : Result:=RGB(127,95,63);
    38  : Result:=RGB(76,38,0);
    39  : Result:=RGB(76,57,38);
    40  : Result:=RGB(255,191,0);
    41  : Result:=RGB(255,223,127);
    42  : Result:=RGB(204,153,0);
    43  : Result:=RGB(204,178,102);
    44  : Result:=RGB(153,114,0);
    45  : Result:=RGB(153,133,176);
    46  : Result:=RGB(127,95,0);
    47  : Result:=RGB(127,111,63);
    48  : Result:=RGB(76,57,0);
    49  : Result:=RGB(76,66,38);
    50  : Result:=RGB(255,255,0);
    51  : Result:=RGB(255,255,127);
    52  : Result:=RGB(204,204,0);
    53  : Result:=RGB(204,204,102);
    54  : Result:=RGB(153,153,0);
    55  : Result:=RGB(153,153,76);
    56  : Result:=RGB(127,127,0);
    57  : Result:=RGB(127,127,63);
    58  : Result:=RGB(76,76,0);
    59  : Result:=RGB(76,76,38);
    60  : Result:=RGB(191,255,0);
    61  : Result:=RGB(223,255,127);
    62  : Result:=RGB(153,204,0);
    63  : Result:=RGB(178,204,102);
    64  : Result:=RGB(114,153,0);
    65  : Result:=RGB(133,153,76);
    66  : Result:=RGB(95,127,0);
    67  : Result:=RGB(111,127,63);
    68  : Result:=RGB(57,76,0);
    69  : Result:=RGB(66,76,38);
    70  : Result:=RGB(127,255,0);
    71  : Result:=RGB(191,255,127);
    72  : Result:=RGB(102,204,0);
    73  : Result:=RGB(153,204,102);
    74  : Result:=RGB(76,153,0);
    75  : Result:=RGB(114,153,76);
    76  : Result:=RGB(63,127,0);
    77  : Result:=RGB(95,127,63);
    78  : Result:=RGB(38,76,0);
    79  : Result:=RGB(57,76,38);
    80  : Result:=RGB(63,255,0);
    81  : Result:=RGB(159,255,127);
    82  : Result:=RGB(51,204,0);
    83  : Result:=RGB(127,204,102);
    84  : Result:=RGB(38,153,0);
    85  : Result:=RGB(95,153,76);
    86  : Result:=RGB(31,127,0);
    87  : Result:=RGB(79,127,63);
    88  : Result:=RGB(19,76,0);
    89  : Result:=RGB(47,76,38);
    90  : Result:=RGB(0,255,0);
    91  : Result:=RGB(127,255,127);
    92  : Result:=RGB(0,204,0);
    93  : Result:=RGB(102,204,102);
    94  : Result:=RGB(0,153,0);
    95  : Result:=RGB(76,153,76);
    96  : Result:=RGB(0,127,0);
    97  : Result:=RGB(63,127,63);
    98  : Result:=RGB(0,76,0);
    99  : Result:=RGB(38,76,38);
    //100  : Result:=RGB(0,255,63);
    101  : Result:=RGB(127,255,159);
    102  : Result:=RGB(0,204,51);
    103  : Result:=RGB(102,204,127);
    104  : Result:=RGB(0,153,38);
    105  : Result:=RGB(76,153,95);
    106  : Result:=RGB(0,127,31);
    107  : Result:=RGB(63,127,79);
    108  : Result:=RGB(0,76,19);
    109  : Result:=RGB(38,76,47);
    110  : Result:=RGB(0,255,127);
    111  : Result:=RGB(127,255,191);
    112  : Result:=RGB(0,204,102);
    113  : Result:=RGB(102,204,153);
    114  : Result:=RGB(0,153,76);
    115  : Result:=RGB(76,153,114);
    116  : Result:=RGB(0,127,63);
    117  : Result:=RGB(63,127,95);
    118  : Result:=RGB(0,76,38);
    119  : Result:=RGB(38,76,57);
    120  : Result:=RGB(0,255,191);
    121  : Result:=RGB(127,255,223);
    122  : Result:=RGB(0,204,153);
    123  : Result:=RGB(102,204,178);
    124  : Result:=RGB(0,153,114);
    125  : Result:=RGB(76,153,133);
    126  : Result:=RGB(0,127,95);
    127  : Result:=RGB(63,127,111);
    128  : Result:=RGB(0,76,57);
    129  : Result:=RGB(38,76,66);
    130  : Result:=RGB(0,255,255);
    131  : Result:=RGB(127,255,255);
    132  : Result:=RGB(0,204,204);
    133  : Result:=RGB(102,204,204);
    134  : Result:=RGB(0,153,153);
    135  : Result:=RGB(76,153,153);
    136  : Result:=RGB(0,127,127);
    137  : Result:=RGB(63,127,127);
    138  : Result:=RGB(0,76,76);
    139  : Result:=RGB(38,76,76);
    140  : Result:=RGB(0,191,255);
    141  : Result:=RGB(127,223,255);
    142  : Result:=RGB(0,153,204);
    143  : Result:=RGB(102,178,204);
    144  : Result:=RGB(0,114,153);
    145  : Result:=RGB(76,133,153);
    146  : Result:=RGB(0,95,127);
    147  : Result:=RGB(63,111,127);
    148  : Result:=RGB(0,57,76);
    149  : Result:=RGB(38,66,76);
    150  : Result:=RGB(0,127,255);
    151  : Result:=RGB(127,191,255);
    152  : Result:=RGB(0,102,204);
    153  : Result:=RGB(102,153,204);
    154  : Result:=RGB(0,76,153);
    155  : Result:=RGB(76,114,153);
    156  : Result:=RGB(0,63,127);
    157  : Result:=RGB(63,95,127);
    158  : Result:=RGB(0,38,76);
    159  : Result:=RGB(38,57,76);
    160  : Result:=RGB(0,63,255);
    161  : Result:=RGB(127,159,255);
    162  : Result:=RGB(0,51,204);
    163  : Result:=RGB(102,127,204);
    164  : Result:=RGB(0,38,153);
    165  : Result:=RGB(76,95,153);
    166  : Result:=RGB(0,31,127);
    167  : Result:=RGB(63,79,127);
    168  : Result:=RGB(0,19,76);
    169  : Result:=RGB(38,47,76);
    170  : Result:=RGB(0,0,255);
    171  : Result:=RGB(127,127,255);
    172  : Result:=RGB(0,0,204);
    173  : Result:=RGB(102,102,204);
    174  : Result:=RGB(0,0,153);
    175  : Result:=RGB(76,76,153);
    176  : Result:=RGB(0,0,127);
    177  : Result:=RGB(63,63,127);
    178  : Result:=RGB(0,0,76);
    179  : Result:=RGB(38,38,76);
    180  : Result:=RGB(63,0,255);
    181  : Result:=RGB(159,127,255);
    182  : Result:=RGB(51,0,204);
    183  : Result:=RGB(127,102,204);
    184  : Result:=RGB(38,0,153);
    185  : Result:=RGB(95,76,153);
    186  : Result:=RGB(31,0,127);
    187  : Result:=RGB(79,63,127);
    188  : Result:=RGB(19,0,76);
    189  : Result:=RGB(47,38,76);
    190  : Result:=RGB(127,0,255);
    191  : Result:=RGB(191,127,255);
    192  : Result:=RGB(102,0,204);
    193  : Result:=RGB(153,102,204);
    194  : Result:=RGB(76,0,153);
    195  : Result:=RGB(114,76,153);
    196  : Result:=RGB(63,0,127);
    197  : Result:=RGB(95,63,127);
    198  : Result:=RGB(38,0,76);
    199  : Result:=RGB(57,38,76);
    200  : Result:=RGB(191,0,255);
    201  : Result:=RGB(223,127,255);
    202  : Result:=RGB(153,0,204);
    203  : Result:=RGB(178,102,204);
    204  : Result:=RGB(114,0,153);
    205  : Result:=RGB(133,76,153);
    206  : Result:=RGB(95,0,127);
    207  : Result:=RGB(111,63,127);
    208  : Result:=RGB(57,0,76);
    209  : Result:=RGB(66,38,76);
    210  : Result:=RGB(255,0,255);
    211  : Result:=RGB(255,127,255);
    212  : Result:=RGB(204,0,204);
    213  : Result:=RGB(204,102,204);
    214  : Result:=RGB(153,0,153);
    215  : Result:=RGB(153,76,153);
    216  : Result:=RGB(127,0,127);
    217  : Result:=RGB(127,63,79);
    218  : Result:=RGB(76,0,76);
    219  : Result:=RGB(76,38,76);
    220  : Result:=RGB(255,0,191);
    221  : Result:=RGB(255,127,223);
    222  : Result:=RGB(204,0,153);
    223  : Result:=RGB(204,102,178);
    224  : Result:=RGB(153,0,114);
    225  : Result:=RGB(153,76,133);
    226  : Result:=RGB(127,0,95);
    227  : Result:=RGB(127,63,111);
    228  : Result:=RGB(76,0,57);
    229  : Result:=RGB(76,38,66);
    230  : Result:=RGB(255,0,127);
    231  : Result:=RGB(255,127,191);
    232  : Result:=RGB(204,0,102);
    233  : Result:=RGB(204,102,153);
    234  : Result:=RGB(153,0,76);
    235  : Result:=RGB(153,76,114);
    236  : Result:=RGB(127,0,63);
    237  : Result:=RGB(127,63,95);
    238  : Result:=RGB(76,0,38);
    239  : Result:=RGB(76,38,57);
    240  : Result:=RGB(255,0,63);
    241  : Result:=RGB(255,127,159);
    242  : Result:=RGB(204,0,51);
    243  : Result:=RGB(204,102,127);
    244  : Result:=RGB(153,0,38);
    245  : Result:=RGB(153,76,95);
    246  : Result:=RGB(127,0,31);
    247  : Result:=RGB(127,63,79);
    248  : Result:=RGB(76,0,19);
    249  : Result:=RGB(76,38,47);
    250  : Result:=RGB(51,51,51);
    251  : Result:=RGB(91,91,91);
    252  : Result:=RGB(132,132,132);
    253  : Result:=RGB(173,173,173);
    254  : Result:=RGB(214,214,214);
    255  : Result:=RGB(255,255,255);
  else begin
     Result       :=clBlack;
  end;
  end;
end;

end.



(*
    StellaSOFT objektumok gyüjteménye
    ---------------------------------
    TUndoRedo:      UndoRedo folyamatokat megvalósító objektum;

*)
unit AL_Objects;

interface

uses Windows, Classes, SysUtils, Graphics, Vcl.Forms, Vcl.Controls, StdCtrls,
     Math, NewGeom, Clipper, ClipBrd;


Type

  TFloat = Double;
  Str32 = string[32];
  TMetric = (meMM,meInch,meTime);
  TGridStyle  = (gsNone,gsLine,gsDot,gsCross);


  {Síkbeli pont objektum}
  TPoint2dObj = Class(TPersistent)
  private
    fx,fy : TFloat;
    FOnChange: TNotifyEvent;
    procedure Setx(Value:TFloat);
    procedure Sety(Value:TFloat);
    procedure Changed; dynamic;
  public
    constructor Create;
  published
    property x:TFloat read fx write Setx;
    property y:TFloat read fy write Sety;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  T2DPoint = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    Fx : extended;
    Fy : extended;
    FID: integer;
    procedure Setx(Value:extended);
    procedure Sety(Value:extended);
    procedure Changed; dynamic;
  public
    constructor Create(AOwner:TObject; px,py: extended);
  published
    property ID          : integer  read FID write FID;
    property x           : extended read Fx write Setx;
    property y           : extended read Fy write Sety;
    property OnChange    : TNotifyEvent read FOnChange write FOnChange;
  end;

  T3DPoint = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FID: integer;
    Fx : extended;
    Fy : extended;
    Fz : extended;
    procedure Changed; dynamic;
    procedure Setx(Value:extended);
    procedure Sety(Value:extended);
    procedure Setz(const Value: extended);
  public
    constructor Create(AOwner:TObject; px,py,pz: extended);
  published
    property ID          : integer  read FID write FID;
    property x           : extended read Fx write Setx;
    property y           : extended read Fy write Sety;
    property z           : extended read Fz write Setz;
    property OnChange    : TNotifyEvent read FOnChange write FOnChange;
  end;


  TUndoRedoChangeEvent = procedure(Sender:TObject; Undo,Redo:boolean) of object;
  TUndoSaveEvent = procedure(Sender:TObject; MemSt:TMemoryStream) of object;
  TUndoSaveProcedure = procedure(var MemSt:TMemoryStream) of object;
  TUndoRedoProcedure = procedure(MemSt:TMemoryStream) of object;

  {---- UndoRedo objektum -----}
  TUndoRedo = class
  private
    fEnable: boolean;
    fUndoLimit: integer;
    FUndoRedo:TUndoRedoChangeEvent;
    FUndoSave: TUndoSaveEvent;
    fUndoSaveProcedure: TUndoSaveProcedure;
    fUndoRedoProcedure: TUndoRedoProcedure;
    procedure SetUndoLimit(const Value: integer);
    procedure SetEnable(const Value: boolean);
  protected
    UndoSaveCount : integer;
    UndoCount     : integer;
    UndoStart     : integer;
    UndoPointer   : integer;
    UndoEnable,RedoEnable : boolean;
    function GetIndex(us:integer): integer;
  public
    UndoStreams   : array[0..999] of TMemoryStream;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure UndoInit;
    procedure UndoSave;
    procedure Undo;
    procedure Redo;
    property Enable : boolean read fEnable write SetEnable;
    property UndoLimit : integer read fUndoLimit write SetUndoLimit;
    property UndoSaveProcedure: TUndoSaveProcedure read fUndoSaveProcedure write fUndoSaveProcedure;
    property UndoRedoProcedure: TUndoRedoProcedure read fUndoRedoProcedure write fUndoRedoProcedure;
    property OnUndoRedo : TUndoRedoChangeEvent read FUndoRedo write FUndoRedo;
    property OnUndoSave : TUndoSaveEvent read FUndoSave write FUndoSave;
  end;

   THRTimer = Class(TObject)
     Constructor Create;
     Function StartTimer : Boolean;
     Function ReadTimer : Double;
   private
   public
     Exists    : Boolean;
     StartTime : Double;
     ClockRate : Double;
     PROCEDURE Delay(ms: double);
   End;

{ TGrid }

TGrid = Class(TPersistent)
private
  FAbove: boolean;
  fVisible: boolean;
  fGridStyle: TGridStyle;
  fSubGridColor: TColor;
  fMainGridColor: TColor;
  FOnChange: TNotifyEvent;
  fMetric: TMetric;
  fMargin: integer;
  fOnlyOnPaper: boolean;
  FAligne: boolean;
  FSubDistance: integer;
  procedure SetAbove(AValue: boolean);
  procedure SetMainGridColor(Value: TColor);
  procedure SetGridStyle(const Value: TGridStyle);
  procedure SetSubGridColor(Value: TColor);
  procedure SetVisible(const Value: boolean);
  procedure SetMetric(const Value: TMetric);
  procedure SetMargin(const Value: integer);
  procedure SetOnlyOnPaper(const Value: boolean);
  procedure SetAligne(const Value: boolean);
  procedure SetSubDistance(const Value: integer);
protected
public
  constructor Create;
  procedure Change;
published
  property Above      : boolean  read FAbove write SetAbove;
  property Aligne     : boolean  read FAligne write SetAligne;
  property MainGridColor: TColor read fMainGridColor write SetMainGridColor;
  property Margin: integer       read fMargin write SetMargin;
  property SubDistance: integer  read FSubDistance write SetSubDistance default 1;
  property SubGridColor: TColor  read fSubGridColor write SetSubGridColor;
  property Style: TGridStyle     read fGridStyle write SetGridStyle default gsNone;
  property Metric: TMetric       read fMetric write SetMetric default meMM;
  property Visible: boolean      read fVisible write SetVisible default True;
  property OnlyOnPaper: boolean  read fOnlyOnPaper write SetOnlyOnPaper default True;
  property OnChange: TNotifyEvent read FOnChange write FOnChange;
end;

  TALPaper = class(TPersistent)
  private
    FWidth: double;
    FColor: TColor;
    FBottom: double;
    fVisible: boolean;
    FOnChange: TNotifyEvent;
    FHeight: double;
    FLeft: double;
    fShadow: boolean;
    procedure Changed; dynamic;
    procedure SetBottom(const Value: double);
    procedure SetColor(const Value: TColor);
    procedure SetHeight(const Value: double);
    procedure SetLeft(const Value: double);
    procedure SetVisible(const Value: boolean);
    procedure SetWidth(const Value: double);
    procedure SetShadow(const Value: boolean);
  public
    constructor Create;
  published
    property Left        : double       read FLeft write SetLeft;
    property Bottom      : double       read FBottom write SetBottom;
    property Width       : double       read FWidth write SetWidth;
    property Height      : double       read FHeight write SetHeight;
    property Color       : TColor       read FColor write SetColor default clWhite;
    property Shadow      : boolean      read fShadow write SetShadow default True;
    property Visible     : boolean      read fVisible write SetVisible default True;
    property OnChange    : TNotifyEvent read FOnChange write FOnChange;
  end;

  TBMPObject = class(TPersistent)
  private
    fVisible: boolean;
    FOnChange: TNotifyEvent;
    FPosition: TPoint2dObj;
    FZoom: double;
    procedure Changed; dynamic;
    procedure SetVisible(const Value: boolean);
    function GetBoundsRect: TRect2d;
    procedure SetPosition(const Value: TPoint2dObj);
    procedure SetZoom(const Value: double);
  public
    origBMP  : TBitmap;
    BMP      : TBitmap;
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(fn: string): boolean;
    function PasteFromClipboard: boolean;
    procedure RestoreOriginal;
    procedure Clear;
    procedure Lightness( Amount: integer );
    property BoundsRect: TRect2d read GetBoundsRect;
  published
    property Position    : TPoint2dObj  read FPosition write SetPosition;       // Left & bottom
    property Visible     : boolean      read fVisible write SetVisible default True;
    property Zoom        : double       read FZoom write SetZoom;
    property OnChange    : TNotifyEvent read FOnChange write FOnChange;
  end;



  // Procedures ---------------------------------------------------------

  function InRange(Test,Min,Max: Integer): Boolean;
  function AL_InputQuery(const x,y : integer; ACaption, APrompt: string;
                             VisibleButtons: boolean;  var Value: string): Boolean;
  function KeyPressChange(var Key: Char; Numeric: boolean): Char;

implementation

{$R Cursors.RES}

function InRange(Test,Min,Max: Integer): Boolean;
begin
  Result:=(Test >= Min) and (Test <= Max);
end;

{------------------------------------------------------------------------------}


{ -----------  TUndoRedo --------- }

constructor TUndoRedo.Create;
var i: integer;
begin
  Inherited Create;
  UndoLimit := 1000;
  Enable    := True;
  UndoInit;
end;

destructor TUndoRedo.Destroy;
var i: integer;
begin
  for i:=0 to fUndoLimit-1 do
      if  UndoStreams[i]<>nil then UndoStreams[i].Destroy;
  Inherited Destroy;
end;

{Az Undo stream-eket alapra hozza}
procedure TUndoRedo.UndoInit;
var i: integer;
begin
  UndoSaveCount := 0;
  UndoCount     := 0;
  UndoStart     := 0;
  UndoPointer   := 0;
  UndoEnable    := False;
  RedoEnable    := False;
  for i:=0 to fUndoLimit-1 do
      if UndoStreams[i]=nil then UndoStreams[i].Create
      else UndoStreams[i].Clear;
  If Assigned(FUndoRedo) then FUndoRedo(Self,False,False);
end;

{Undo mentés az sbl stream tartalmát menti az UndoStreams n. streamjére;
 az undopointer és undoCount értékét 1-el növeli }
procedure TUndoRedo.UndoSave;
begin
If Enable then begin
  UndoStart := UndoPointer;
  UndoStreams[UndoPointer].Clear;
  If Assigned(fUndoSaveProcedure) then
     fUndoSaveProcedure(UndoStreams[UndoPointer]);
  Inc(UndoPointer);
  UndoPointer := UndoPointer mod UndoLimit;
  Inc(UndoSaveCount);
  UndoCount := 0;
  UndoEnable    := UndoSaveCount>0;
  RedoEnable    := False;
  If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable);
end;
end;

function TUndoRedo.GetIndex(us:integer): integer;
begin
  Result := us;
  If us>(Undolimit-1) then Result:=us mod Undolimit;
  if us<0 then Result:=UndoLimit-(Trunc(Abs(us)) mod Undolimit)
end;

procedure TUndoRedo.Undo;
var UC,IDX: integer;
begin
If Enable then begin
   UC := UndoPointer-1;
   If UndoSaveCount>=UndoLimit then UC:=UndoLimit-1;
   UndoEnable := UndoCount<UC;
   if UndoEnable then begin
        Dec(UndoStart);
        IDX := GetIndex(UndoStart);
        UndoStreams[IDX].Seek(0,0);
        If Assigned(fUndoRedoProcedure) then
           fUndoRedoProcedure(UndoStreams[IDX]);
        Inc(UndoCount);
        UndoEnable := UndoCount<UC;
        RedoEnable := UndoCount>0;
   end;
   If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable);
end;
end;

procedure TUndoRedo.Redo;
var UC,IDX: integer;
begin
If Enable then begin
   RedoEnable := UndoCount>0;
   if RedoEnable then begin
        Inc(UndoStart);
        IDX := GetIndex(UndoStart);
        UndoStreams[IDX].Seek(0,0);
        If Assigned(fUndoRedoProcedure) then
           fUndoRedoProcedure(UndoStreams[IDX]);
        Dec(UndoCount);
        RedoEnable := UndoCount>0;
        UndoEnable := True;
   end;
   If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable);
end;
end;

procedure TUndoRedo.SetUndoLimit(const Value: integer);
var i: integer;
begin
  If fUndoLimit <> Value then begin
     fUndoLimit := Value;
     If fUndoLimit>High(UndoStreams) then fUndoLimit:=High(UndoStreams);
     for i:=0 to fUndoLimit-1 do
       if UndoStreams[i]=nil then UndoStreams[i]:=TMemoryStream.Create;
     for i:=fUndoLimit to High(UndoStreams) do
       if UndoStreams[i]<>nil then UndoStreams[i].Destroy;
  end;
end;

procedure TUndoRedo.SetEnable(const Value: boolean);
begin
  fEnable := Value;
  if Value then begin
   If Assigned(FUndoRedo) then FUndoRedo(Self,UndoEnable,RedoEnable)
  end else
   If Assigned(FUndoRedo) then FUndoRedo(Self,False,False);
end;

{ -----------  TPoint2dObj --------- }

constructor TPoint2dObj.Create;
begin
  inherited;
  fx := 0;
  fy := 0;
end;

procedure TPoint2dObj.Changed;
begin if Assigned(FOnChange) then FOnChange(Self); end;

procedure TPoint2dObj.Setx(Value:TFloat);
begin
     Fx:=Value;
     Changed;
end;

procedure TPoint2dObj.Sety(Value:TFloat);
begin
     Fy:=Value;
     Changed;
end;

{ -----------  T2DPoint --------- }

procedure T2DPoint.Changed;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

constructor T2DPoint.Create(AOwner:TObject; px,py: extended);
begin
  inherited Create;
  x := px; y := py;
  ID := 0;
end;

procedure T2DPoint.Setx(Value:extended);
begin
  If Fx<>Value then begin
     Fx:=Value;
     Changed;
  end;
end;

procedure T2DPoint.Sety(Value:extended);
begin
  If Fy<>Value then begin
     Fy:=Value;
     Changed;
  end;
end;

//-----------THRTimer-----------------

Constructor THRTimer.Create;
Var  QW : _Large_Integer;
BEGIN
   Inherited Create;
   Exists := QueryPerformanceFrequency(TLargeInteger(QW));
   ClockRate := QW.QuadPart;
END;

Function THRTimer.StartTimer : Boolean;
Var
  QW : _Large_Integer;
BEGIN
   Result := QueryPerformanceCounter(TLargeInteger(QW));
   StartTime := QW.QuadPart;
END;

Function THRTimer.ReadTimer : Double;
Var
  ET : _Large_Integer;
BEGIN
   QueryPerformanceCounter(TLargeInteger(ET));
   Result := 1000.0*(ET.QuadPart - StartTime)/ClockRate;
END;

PROCEDURE THRTimer.Delay(ms: double);
Var
  QW,ET : _Large_Integer;
  Start_Time, dt : double;
BEGIN
   QueryPerformanceCounter(TLargeInteger(QW));
   Start_Time := QW.QuadPart;
   repeat
         QueryPerformanceCounter(TLargeInteger(ET));
         dt := 1000.0*(ET.QuadPart - Start_Time)/ClockRate;
   Until dt>=ms;
END;

{------------------------------------------------------------------------------}


{ T3DPoint }

procedure T3DPoint.Changed;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

constructor T3DPoint.Create(AOwner: TObject; px, py, pz: extended);
begin
  inherited Create;
  x := px; y := py; z := pz;
  ID := 0;
end;

procedure T3DPoint.Setx(Value: extended);
begin
  If Fx<>Value then begin
     Fx:=Value;
     Changed;
  end;
end;

procedure T3DPoint.Setz(const Value: extended);
begin
  If Fz<>Value then begin
     Fz:=Value;
     Changed;
  end;
end;

procedure T3DPoint.Sety(Value: extended);
begin
  If Fy<>Value then begin
     Fy:=Value;
     Changed;
  end;
end;

// ------------------------------------------------------------------------

{ TGrid }

constructor TGrid.Create;
begin
  inherited Create;
  fGridStyle     := gsLine;
  fMainGridColor := clGreen;
  fSubGridColor  := clSilver;
  FSubDistance   := 1;
  fOnlyOnPaper   := True;
  fMetric        := meMM;
  fMargin        := 0;
  fVisible       := True;
end;

procedure TGrid.SetMainGridColor(Value: TColor);
begin
  fMainGridColor:=Value;
  Change;
end;

procedure TGrid.SetAbove(AValue: boolean);
begin
  if FAbove=AValue then Exit;
  FAbove:=AValue;
  Change;
end;

procedure TGrid.SetGridStyle(const Value: TGridStyle);
begin
  fGridStyle := Value;
  Change;
end;

procedure TGrid.SetSubGridColor(Value: TColor);
begin
  fSubGridColor := Value;
  Change;
end;

procedure TGrid.SetVisible(const Value: boolean);
begin
  fVisible := Value;
  Change;
end;

procedure TGrid.Change;
begin if Assigned(FOnChange) then FOnChange(Self); end;


procedure TGrid.SetMetric(const Value: TMetric);
begin
  fMetric := Value;
  Change;
end;

procedure TGrid.SetMargin(const Value: integer);
begin
     fMargin := Value;
     Change;
end;

procedure TGrid.SetOnlyOnPaper(const Value: boolean);
begin
  fOnlyOnPaper := Value;
  Change;
end;

procedure TGrid.SetAligne(const Value: boolean);
begin
  FAligne := Value;
  Change;
end;

procedure TGrid.SetSubDistance(const Value: integer);
begin
  // Értlke meghatározza a grid finom beosztásainak távolságát.
  // Ez alap esetben 1 mm.
  // Ennek akkor van jelentõsége, ha a rácshoz igazítás funkció be van kapcsolva,
  //       azaz csak a rácspontokba lehet rajzolni.
  FSubDistance := Value;
  Change;
end;

{------------------------------------------------------------------------------}

{ TALPaper }

procedure TALPaper.Changed;
begin
  if Assigned(FOnChange) then
     FOnChange(Self);
end;

constructor TALPaper.Create;
begin
  FLeft    := 0;
  FBottom  := 0;
  fWidth   := 100;
  fHeight  := 100;
  FColor   := clWhite;
  FShadow  := true;
  FVisible := true;
end;

procedure TALPaper.SetBottom(const Value: double);
begin
  FBottom := Value;
  Changed;
end;

procedure TALPaper.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TALPaper.SetHeight(const Value: double);
begin
  FHeight := Value;
  Changed;
end;

procedure TALPaper.SetLeft(const Value: double);
begin
  FLeft := Value;
  Changed;
end;

procedure TALPaper.SetShadow(const Value: boolean);
begin
  fShadow := Value;
  Changed;
end;

procedure TALPaper.SetVisible(const Value: boolean);
begin
  fVisible := Value;
  Changed;
end;

procedure TALPaper.SetWidth(const Value: double);
begin
  FWidth := Value;
  Changed;
end;




function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function AL_InputQuery(const x,y : integer; ACaption, APrompt: string;
                             VisibleButtons: boolean;  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(128, DialogUnits.X, 4);
      if VisibleButtons then
         ClientHeight := MulDiv(63, DialogUnits.Y, 8)
      else
         ClientHeight := MulDiv(40, DialogUnits.Y, 8);
      Left := x;
      Top  := y;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(40, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        if VisibleButtons then
        SetBounds(MulDiv(10, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight)
        else
        SetBounds(0,0,0,0);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        if VisibleButtons then
        SetBounds(MulDiv(68, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight)
        else
        SetBounds(0,0,0,0);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

{ TBMPObject }

procedure TBMPObject.Changed;
begin if Assigned(FOnChange) then FOnChange(Self); end;

procedure TBMPObject.Clear;
begin
  origBMP.FreeImage;
  BMP.FreeImage;
  Changed;
end;

constructor TBMPObject.Create;
begin
  origBMP   := TBitmap.Create;
  BMP       := TBitmap.Create;
  FPosition := TPoint2DObj.Create;
  FZoom     := 1.0;
end;

destructor TBMPObject.Destroy;
begin
  BMP.Free;
  origBMP.Free;
  FPosition.Free;
  inherited;
end;

function TBMPObject.GetBoundsRect: TRect2d;
begin
  // befoglaló téglalap (Left,top,left+width, bottom)
  Result := Rect2d(FPosition.x, FPosition.y+FZoom*BMP.Height,
         FPosition.x+FZoom*BMP.Width, FPosition.y );
end;

function TBMPObject.LoadFromFile(fn: string): boolean;
var
  W: TWICImage;
begin
  Result := True;
  W:= TWicImage.Create;
  try
    Try
      W.LoadFromFile(fn);
    Except
      W.LoadFromFile(fn);
    End;
    if Bmp=nil then
    Bmp:= TBitmap.Create;
    Bmp.Assign(W);
    BMP.PixelFormat := pf24bit;
    origBmp.Assign(BMP);
  finally
    W.Free;
    Changed;
  end;
end;

function TBMPObject.PasteFromClipboard: boolean;
begin
  Result := false;
  if Clipboard.HasFormat(CF_PICTURE) then begin
    origBMP.Assign(Clipboard);
    origBMP.PixelFormat := pf24bit;
    BMP.Assign(origBMP);
    Result := true;
    Changed;
  end;
end;

procedure TBMPObject.RestoreOriginal;
begin
  BMP.Assign(OrigBMP);
end;

procedure TBMPObject.SetPosition(const Value: TPoint2dObj);
begin
  // befoglaló téglalap bal alsó sarka
  FPosition := Value;
  Changed;
end;

procedure TBMPObject.SetVisible(const Value: boolean);
begin
  fVisible := Value;
  Changed;
end;

procedure TBMPObject.SetZoom(const Value: double);
begin
  FZoom := Value;
  Changed;
end;

procedure TBMPObject.Lightness( Amount: integer );
var
Wsk:^Byte;
H,V: Integer;

     function IntToByte(i:Integer):Byte;
     begin
          if i > 255 then
             Result := 255
          else if i < 0 then
             Result := 0
          else
             Result := i;
     end;

begin
  RestoreOriginal;
  for V:=0 to BMP.Height-1 do begin
    WSK:=BMP.ScanLine[V];
    for H:=0 to BMP.Width*3-1 do
    begin
    if Amount>0 then
       Wsk^:=IntToByte(Wsk^+((255-Wsk^)*Amount)div 255)
    else
       Wsk^:=IntToByte(Wsk^+(Wsk^*Amount)div 255);
    inc(Wsk);
    end;
  end;
  Changed;
end;

 // Controll the KeyPress chars:
  // If Numeric then char in (0..9,'+','-'#13,#8,'.',',')
  // and change ,=>.
  function KeyPressChange(var Key: Char; Numeric: boolean): Char;
  begin
    Result := Key;
    if Numeric then
    begin
      if not ( Key in ['0'..'9','+','-',#13,#27,#8,'.',','] ) then
         Result := #0;
      if Key=',' then Result:='.';
    end;
    Key := Result;
  end;

end.

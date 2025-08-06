unit al_DXF;

interface

uses
  Classes, SysUtils, ComCtrls, Math,
  AL_CADTypes, Szoveg, NewGeom;

Const
  DXFObjectNames : shortstring = 'POINT LINE POLYLINE LWPOLYLINE CIRCLE ARC '
                  +'ELLIPSE SPLINE SOLID TEXT MTEXT INSERT ';

  DXFShapeStr : array[0..11] of Shortstring =
          ('POINT','LINE','POLYLINE','LWPOLYLINE','CIRCLE','ARC','ELLIPSE',
          'SPLINE','SOLID','TEXT','MTEXT','INSERT');

  DXFShapeType : array[0..11] of TDrawMode =
          (dmPoint,dmLine,dmPolyline,dmPolyline,dmCircle,dmArc,dmEllipse,
          dmSpline,dmSolid,dmText,dmText,dmInsert);

Type

  TDXFEntity = (POINT,LINE,POLYLINE,LWPOLYLINE,CIRCLE,ARC,ELLIPSE,SPLINE,
            SOLID,TEXT,MTEXT,INSERT);

  DXFEntitySet = Set of TDXFEntity;


  TDXFShapeArray = array[0..High(DXFShapeType)] of integer;

  intPtr = ^intVal;
  intVal = integer;

  TALCustomDXF = class
  private
    FDXFFileName: string;
    fAutoPoligonize: boolean;
    FCadSource: TCADSource;
    procedure SetDXFFileName(const Value: string);
    procedure GetCurveParameters(cuv: TCurve; var idx: integer; var closed: boolean);
    function DXFKeres(s: string): integer;
    function Loop(var j: integer; s: string): boolean;
    function GetVal(var j: integer; code: string; var Value: string): boolean;
    procedure Generate;
  protected
    ActualIdx  : integer;      // Actual index of DXFLines stringlist
    iPtr : intPtr;
    procedure RepairDXF;
  public
    DXFLines   : TStringList;  // For DXF lenes
    DXFIdx     : integer;      // Index of aktual line
    DXFEntity  : TDXFEntity;
    ShapeName  : string;
    idxHEADER  : integer;      // Index of Header ....
    idxCLASSES : integer;
    idxTABLES  : integer;
    idxLAYERS  : integer;
    idxLTYPES  : integer;
    idxBLOCKS  : integer;
    idxENTITIES: integer;
    idxOBJECTS : integer;

    BlockCount : integer;
    CurveCount : integer;

    DXFLayerList: array of string;
    DXFLineTypeList: array of string;

    Constructor Create (const File_Name: string); overload;
    Constructor Create (const Data_Source: TCADSource); overload;
    Destructor  Destroy; override;

    procedure First;
    procedure Last;
    procedure GoLine(lNumber: integer);

    procedure Init;           // Clear all datas
    procedure ClearIndexList;

    procedure GetStructure(Nodes: TTreeNodes);
    procedure GetSECTIONList(Nodes: TTreeNodes);
    procedure GetHeader(Nodes: TTreeNodes);
    procedure GetClasses(Nodes: TTreeNodes);
    procedure GetTables(Nodes: TTreeNodes);
    procedure GetBlocks(Nodes: TTreeNodes);
    procedure GetEntities(Nodes: TTreeNodes);
    procedure GetObjects(Nodes: TTreeNodes);

    procedure GetBlockList(BlockList: TBlockList);
    procedure GetEntitiesList(CurveList: TCurveList);
    procedure GetCurveList(CurveList: TCurveList);
//    procedure GetLineTypeList(LineTypeList: TLineTypeList);
    procedure GetLayerList(LayerList: TLayerList);
    function  GetLayerIndex(Name: string): integer;

    // Teljes adattömeg kinyerése
    procedure GetFullDraw(fn: TFileName; DS: TCADSource); overload;
    procedure Regenerate(lines: TStrings);

  published
    property CadSource: TCADSource read FCadSource write FCadSource;
    property DXFFileName: string read FDXFFileName write SetDXFFileName;
    property AutoPoligonize: boolean read fAutoPoligonize
             write fAutoPoligonize default True;
  end;



implementation

function GetNodeByText(ATree : TTreeNodes; AValue:String): TTreeNode;
var i: integer;
begin
     Result := nil;
     if ATree.Count = 0 then Exit;
     i:=0;
     while i<=Pred(Atree.Count) do begin
           if UpperCase(ATree.Item[i].Text) = UpperCase(AValue) then
              begin
                   Result := ATree.Item[i];
                   Break;
              end;
           Inc(i);
     end;
end;

{ TALCustomDXF }

procedure TALCustomDXF.ClearIndexList;
{ Törli a mutatókat }
begin
    idxHEADER  := -1;      // Index of Header
    idxCLASSES := -1;
    idxTABLES  := -1;
    idxLTYPES  := -1;
    idxLAYERS  := -1;
    idxBLOCKS  := -1;
    idxENTITIES:= -1;
    idxOBJECTS := -1;
end;

constructor TALCustomDXF.Create(const File_Name: string);
begin
  Inherited Create;
  FormatSettings.DecimalSeparator:='.';
  DXFLines := TStringList.Create;
  FAutoPoligonize := False;
  DXFFileName := File_Name;
  BlockCount  := 0;
end;

constructor TALCustomDXF.Create(const Data_Source: TCADSource);
begin
  Inherited Create;
  FormatSettings.DecimalSeparator:='.';
  FCADSource := Data_Source;
  DXFLines := TStringList.Create;
  FAutoPoligonize := False;
  DXFFileName := '';
  BlockCount  := 0;
end;

destructor TALCustomDXF.Destroy;
begin
  DXFLines.Clear;
  DXFLines.Free;
  SetLength(DXFLayerList,0);
  FCADSource := nil;
  inherited;
end;

procedure TALCustomDXF.First;
begin

end;

procedure TALCustomDXF.GetBlocks(Nodes: TTreeNodes);
var i: integer;
    node: TTreeNode;
    nodeIdx: integer;
    nodeBLK: TTreeNode;
begin
  I := idxBLOCKS;
  BlockCount := 0;
  node := GetNodeByText(Nodes,'BLOCKS');
  while I<Pred(DXFLines.Count) do
  begin
    if DXFLines.Strings[i]='ENDSEC' then Break;
    if DXFLines.Strings[i]='  0' then
    BEGIN
       while I<Pred(DXFLines.Count) do
       begin
          if DXFLines.Strings[i]='ENDBLK' then Break;
          if DXFLines.Strings[i]='  2' then begin
             New(iPtr);
             iPtr^ := i+1;
             nodeBLK := Nodes.AddChildObject(node,DXFLines.Strings[i+1],iPtr);
             Inc(BlockCount);
             Inc(i,2);
             While DXFLines.Strings[i+1]<>'ENDBLK' do begin
                   if DXFLines.Strings[i]='  0' then begin
                      New(iPtr);
                      iPtr^ := i+1;
                      nodes.AddChildObject(nodeBLK,DXFLines.Strings[i+1],iPtr);
                   end;
                   Inc(i);
             end;
          end;
          Inc(i);
          if DXFLines.Strings[i]='ENDSEC' then EXIT;
       end;
    END;
    Inc(i);
  end;
end;

procedure TALCustomDXF.GetClasses(Nodes: TTreeNodes);
begin

end;

procedure TALCustomDXF.GetEntities(Nodes: TTreeNodes);
var i,k,n: integer;
    node,nodCount,nodList: TTreeNode;
    DXFShapeArray: TDXFShapeArray;

    procedure DXFShapeArrayNull;
    var ii: integer;
    begin
      for ii := 0 to High(DXFShapeArray) do
          DXFShapeArray[ii]:=0;
    end;

begin
  I := idxENTITIES;
  DXFShapeArrayNull;
  node := GetNodeByText(Nodes,'ENTITIES');
  nodCount := Nodes.AddChild(node,'COUNT');
  nodList := Nodes.AddChild(node,'LIST');
  n:=0;
  while I<Pred(DXFLines.Count) do
  begin
    if DXFLines.Strings[i]='  0' then
    BEGIN
       if DXFLines.Strings[i+1]='ENDSEC' then Break;
       k := Pos(DXFLines.Strings[i+1],DXFObjectNames);
       if k>0 then
       begin
         k := DXFKeres(DXFLines.Strings[i+1]);
         Inc(DXFShapeArray[k]);
         New(iPtr);
         iPtr^ := i+1;
         Nodes.AddChildObject(nodList,DXFLines.Strings[i+1]+'_'+IntToStr(n),iPtr);
         Inc(n);
       end;
//       Nodes.AddChild(nodList,DXFLines.Strings[i+1]);
    END;
    Inc(i);
  end;

  for i := 0 to High(DXFShapeArray) do
  begin
      if DXFShapeArray[i]<>0 then
         Nodes.AddChild(nodCount,DXFShapeStr[i]+' ('+IntToStr(DXFShapeArray[i])+')');
  end;


end;

procedure TALCustomDXF.GetHeader(Nodes: TTreeNodes);
var i: integer;
    node: TTreeNode;
begin
  I := idxHEADER;
  node := GetNodeByText(Nodes,'HEADER');
  while I<Pred(DXFLines.Count) do
  begin
    if DXFLines.Strings[i]='  9' then
    begin
       New(iPtr);
       iPtr^ := i+1;
       Nodes.AddChildObject(node,DXFLines.Strings[i+1],iPtr);
       Inc(i);
    end;
    Inc(i);
    if DXFLines.Strings[i]='ENDSEC' then Break;
  end;
end;

procedure TALCustomDXF.GetObjects(Nodes: TTreeNodes);
begin

end;

procedure TALCustomDXF.GetSECTIONList(Nodes: TTreeNodes);
var i: integer;
    S: string;
begin
  ClearIndexList;
  if Nodes<>nil then
     Nodes.Clear;
  I := 0;
  while I<DXFLines.Count do
  begin
    if DXFLines.Strings[i]='SECTION' then
    begin
       S := DXFLines.Strings[i+2];
       if Nodes<>nil then begin
          New(iPtr);
          iPtr^ := i+2;
          Nodes.AddObject(nil,S,iPtr);
       end;
       if S='HEADER'   then idxHEADER   := i+2;
       if S='CLASSES'  then idxCLASSES  := i+2;
       if S='TABLES'   then idxTABLES   := i+2;
       if S='BLOCKS'   then idxBLOCKS   := i+2;
       if S='ENTITIES' then idxENTITIES := i+2;
       if S='OBJECTS'  then idxOBJECTS  := i+2;
       Inc(i);
    end;

    if (DXFLines.Strings[i]='LAYER')
    then
       if idxLAYERS=-1 then
          idxLAYERS   := i+1;
    if (DXFLines.Strings[i]='LTYPE')
    then
       if idxLTYPES=-1 then
          idxLTYPES   := i+1;

    Inc(i);
  end;
end;

procedure TALCustomDXF.GetStructure(Nodes: TTreeNodes);
begin
  GetSECTIONList(Nodes);
  if Length(DXFLines.Strings[idxENTITIES-1])<>3 then
     RepairDXF;
  if idxHEADER>-1 then
     GetHeader(Nodes);
  if idxTABLES>-1 then
     GetTables(Nodes);
  if idxBLOCKS>-1 then
     GetBlocks(Nodes);
  if idxENTITIES>-1 then
     GetEntities(Nodes);
end;

procedure TALCustomDXF.GetTables(Nodes: TTreeNodes);
var i: integer;
    nodeArr: array[0..2] of TTreeNode;
begin
  I := idxTABLES;
  nodeArr[0] := GetNodeByText(Nodes,'TABLES');

  while I<Pred(DXFLines.Count) do
  begin

    if DXFLines.Strings[i]='  0' then
    BEGIN

       while I<Pred(DXFLines.Count) do
       begin

          if DXFLines.Strings[i]='TABLE' then
          begin
            if DXFLines.Strings[i+1]='  2' then
            begin
                 New(iPtr);
                 iPtr^ := i+2;
                 nodeArr[1] := Nodes.AddChildObject(nodeArr[0],DXFLines.Strings[i+2],iPtr);
                 Inc(i,3);
            end;
            while I<Pred(DXFLines.Count) do
            begin
               if DXFLines.Strings[i+1]='ENDTAB' then Break;
               if DXFLines.Strings[i]='  2' then begin
                  New(iPtr);
                  iPtr^ := i+1;
                  Nodes.AddChildObject(nodeArr[1],DXFLines.Strings[i+1],iPtr);
               end;
               Inc(i,2);
            end;
          end;

          Inc(i);
          if DXFLines.Strings[i]='ENDSEC' then EXIT;

       end;

    END;
    Inc(i);
  end;
end;

procedure TALCustomDXF.GoLine(lNumber: integer);
begin

end;

procedure TALCustomDXF.Init;
{ Összes adat alaphelyzetbe állítása }
begin
  DXFLines.Clear;
  ClearIndexList;
  BlockCount := 0;
end;

procedure TALCustomDXF.Last;
begin

end;

procedure TALCustomDXF.Regenerate(lines: TStrings);
begin
  Init;
  DXFLines.Assign(lines);
  RepairDXF;
  if CadSource<>nil then
     Generate;
end;

// Ha a DXF parancsok nem 3 karakter hosszúak, akkor kiegészíti 3 karakterre
procedure TALCustomDXF.RepairDXF;
Var k,i: integer;
    zart, paros: boolean;
    s: string;
begin
  zart := True;
  For i:=0 to Pred(DXFLines.Count) do
    if DXFLines[i]='SECTION' then
    Begin
       zart := Length(DXFLines[i+1])<>3;
       k:=i+1;
       break;
    end;
  if zart then begin
     paros := (k mod 2)=0;
     For i:=0 to Pred(DXFLines.Count) do
     begin
         s := DXFLines[i];
         if paros and ((i mod 2)=0) then
                 s := PadR(Trim(DXFLines[i]),' ',3);
         if (not paros) and ((i mod 2)=1) then
                 s := PadR(Trim(DXFLines[i]),' ',3);
         DXFLines[i]:=s;
     end;
  end;
end;

procedure TALCustomDXF.SetDXFFileName(const Value: string);
var ext : string;
begin
 Init;
 if AnsiUpperCase(ExtractFileExt(Value))='.DXF' then
 begin
  if FileExists(Value) then
  begin
       FDXFFileName := Value;
       DXFLines.LoadFromFile(FDXFFileName);
       RepairDXF;
  end
  else
      FDXFFileName := '';
 end
 else
      FDXFFileName := '';
end;

procedure TALCustomDXF.GetEntitiesList(CurveList: TCurveList);
begin
  if idxENTITIES=-1 then
     getSECTIONList(nil);
  ActualIdx := idxENTITIES;
  if idxENTITIES>-1 then
     GetCurveList(CurveList);
end;

procedure TALCustomDXF.GetFullDraw(fn: TFileName; DS: TCADSource);
begin
if DS<>nil then
  DXFFileName := fn;
  getSECTIONList(nil);
//  if DS.FLineTypeList<>nil then
//     GetLineTypeList(DS.FLineTypeList);
  if DS.FLayerList<>nil then
     GetLayerList(DS.FLayerList);
  if DS.FBlockList<>nil then
     GetBlockList(DS.FBlockList);
  if DS.FCurveList<>nil then
  if idxENTITIES>-1 then begin
     DS.FCurveList.Clear;
     GetEntitiesList(DS.FCurveList);
  end;
end;

procedure TALCustomDXF.Generate;
begin
  getSECTIONList(nil);
  if CadSource<>nil then
  with CadSource do begin
  if FLayerList<>nil then
     GetLayerList(FLayerList);
  if FBlockList<>nil then
     GetBlockList(FBlockList);
  if FCurveList<>nil then
  if idxENTITIES>-1 then begin
     FCurveList.Clear;
     GetEntitiesList(FCurveList);
  end;
     Recall;
  end;
end;

function TALCustomDXF.GetLayerIndex(Name: string): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(DXFLayerList) do
  if DXFLayerList[i]=Name then
  begin
    Result := i;
    exit;
  end;
end;

procedure TALCustomDXF.GetLayerList(LayerList: TLayerList);
var i: integer;
    sor,sor1 : string;
    layer    : TLayer;
    inBlock  : boolean;
begin
  LayerList.Clear;
  if idxLayers>-1 then
  begin
    ActualIdx := idxLAYERS;
    inBlock := False;
    i := 0;
    while ActualIdx<Pred(DXFLines.Count) do
    begin
         sor := DXFLines.Strings[ActualIdx];
         sor1:= DXFLines.Strings[ActualIdx+1];

         if sor='  0' then
         BEGIN
              if sor1='LAYER' then
              begin
                 if layer<>nil then
                 if layer.Name<>'' then
                    LayerList.Add(layer);
                 layer := TLayer.Create(i);
                 Inc(i);
                 inBlock := True;
              end;

              if sor1='ENDSEC' then Break;
              if sor1='ENDTAB' then
              begin
                 if layer.Name<>'' then
                    LayerList.Add(layer);
                 break;
              end;
         END;

         if inBlock then
         BEGIN
              case StrToInt(sor) of
              2:   layer.Name := sor1;
              62:  layer.Pen.Color := AUTOCADStandardColor(StrToInt(sor1));
//              6:   layer.LineTypeName := sor1;
              end;
         END;

         Inc(ActualIdx,2);

    end;

    SetLength(DXFLayerList,LayerList.Count);
    for i := 0 to Pred(LayerList.Count) do
        DXFLayerList[i]:=LayerList.Layer[i].Name;

  end;
end;

(*
procedure TALCustomDXF.GetLineTypeList(LineTypeList: TLineTypeList);
var i: integer;
    sor,sor1 : string;
    linetype : TLineType;
    inBlock  : boolean;
    pNo      : integer;
begin
  LineTypeList.Clear;
  if idxLTYPES>-1 then
  begin
    ActualIdx := idxLTYPES;
    inBlock := False;
    i := 0;
    pNo := 0;
    while ActualIdx<Pred(DXFLines.Count) do
    begin
         sor := DXFLines.Strings[ActualIdx];
         sor1:= DXFLines.Strings[ActualIdx+1];

         if sor='  0' then
         BEGIN
              if sor1='LTYPE' then
              begin
                 if LineType<>nil then
                 if LineType.Name<>'' then
                    LineTypeList.Add(LineType);
                 LineType := TLineType.Create(i);
                 Inc(i);
                 inBlock := True;
              end;

              if sor1='ENDSEC' then Break;
              if sor1='ENDTAB' then
              begin
                 if LineType.Name<>'' then
                    LineTypeList.Add(LineType);
                 break;
              end;
         END;

         if inBlock then
         BEGIN
              case StrToInt(sor) of
              2:   LineType.Name := sor1;
              3:   LineType.TypeStr := sor1;
              73:  begin
                        pNo := StrToInt(sor1);
                        i := 0;
                        LineType.ElementNumber := pNo;
                        SetLength(LineType.FPattern,pNo);
                   end;
              40:  LineType.PatterLength := StrToFloat(sor1);
              49:  begin
                     LineType.FPattern[i] := StrToFloat(sor1);
                     Inc(i);
                   end;
              end;
         END;

         Inc(ActualIdx,2);

    end;

    SetLength(DXFLineTypeList,LineTypeList.Count);
    for i := 0 to Pred(LineTypeList.Count) do
        DXFLineTypeList[i]:=LineTypeList.LineType[i].Name;

  end;
end;
*)
// Kigyüjti a Block információkat
procedure TALCustomDXF.GetBlockList(BlockList: TBlockList);
var i: integer;
    sor,sor1 : string;
    x,y: double;
    block: TBlock;
    inBlock : boolean;
begin
  BlockList.Clear;
  if idxBLOCKS>-1 then
  begin
    ActualIdx := idxBLOCKS+1;
    inBlock := False;
    while ActualIdx<Pred(DXFLines.Count) do
    begin
         sor := DXFLines.Strings[ActualIdx];
         sor1:= DXFLines.Strings[ActualIdx+1];

         if inBlock then
         BEGIN
                 // Blokk name
                 if (sor='  2') or (sor='  3') then
                       block.BlockName := sor1;
                 // Layer
                 if sor='  8' then begin
                    if IsNum(trim(sor1)) then
                       block.Layer := StrToInt(sor1)
                    else
                       block.Layer := 0;
                    end;
                 // X
                 if (sor=' 10') or (sor=' 11') then begin
                    x := StrToFloat(sor1);
                 end;
                 // Y
                 if (sor=' 20') or (sor=' 21')  then
                 begin
                    y := StrToFloat(sor1);
                    block.BasePoint.x := x;
                    block.BasePoint.y := y;
                 end;
         END;

         if sor='  0' then
         BEGIN
              if sor1='BLOCK' then
              begin
                 block := TBlock.Create('');
                 inBlock := True;
              end;

              if sor1='ENDSEC' then Break;
              if sor1='ENDBLK' then
              begin
                 BlockList.Add(block);
                 inBlock := False;
              end;

              if DXFKeres(sor1)>-1 then
              begin
                 GetCurveList(block.FCurveList);
                 Inc(ActualIdx,-2);
              end;
         END;

         Inc(ActualIdx,2);

    end;
  end;
  BlockCount := BlockList.Count;
end;

  {Alakzat név indexének keresése}
  function TALCustomDXF.DXFKeres(s: string):integer;
  var j: integer;
  begin
    Result := -1;
    For j:=0 to High(DXFShapeStr) do
        If s=DXFShapeStr[j] then begin
            Result := j;
            Break;
        end;
  end;


// Feltölti adatokkal a CurveList-et az ENTITIES szekcióban
// definiált alakzatokkal
procedure TALCustomDXF.GetCurveList(CurveList: TCurveList);
var k        : integer;
    H        : integer;   // Object ID
    node     : TTreeNode;
    sor,sor1 : string;
    ShapeType: TDrawMode;
    ShClosed : boolean;
    cuv      : Tcurve;

begin
 if ActualIdx>-1 then
  while ActualIdx<Pred(DXFLines.Count) do
  begin
    sor := DXFLines.Strings[ActualIdx];
    sor1:= DXFLines.Strings[ActualIdx+1];
    if sor='  0' then
    BEGIN
       if (sor1='ENDSEC') or (sor1='ENDBLK') then
          Break;
       k := DXFKeres(sor1);
       if k>-1 then
       begin
            DXFEntity := TDXFEntity(k);
            ShapeName := DXFShapeStr[k];
            ShapeType := DXFShapeType[k];
            ShClosed  := ShapeClosed[Ord(TDrawMode(DXFShapeType[k]))];
            H := CurveList.MakeCurve(ShapeName,-1,ShapeType,True,True,ShClosed);
            Inc(ActualIdx,2);
            GetCurveParameters( CurveList.Curves[H], ActualIdx, ShClosed );
            if CurveList.Curves[H].Shape=dmInsert then
               cuv := CurveList.Curves[H];
            if AutoPoligonize then
               CurveList.Curves[H].Poligonize(0);
            CurveList.Curves[H].Closed := ShClosed;
            Inc(ActualIdx);
       end;

    END;
    Inc(ActualIdx);
  end; // end while
end;

// Ugrás az s szöveg soráig (j=aktuális sor indexe)
function TALCustomDXF.Loop(var j: integer; s: string): boolean;
var jj: integer;
begin
  result := False;
  jj := j;
  while jj<DXFLines.Count do
  begin
    if DXFLines.Strings[jj]='  0' then
       Break;
    if s=DXFLines.Strings[jj] then
    begin
      j:=jj;
      Result := True;
      Break;
    end;
    Inc(jj);
  end;
end;

  function TALCustomDXF.GetVal(var j: integer; code: string; var Value: string): boolean;
  var sj,scode: string;
  begin
    result := False;
    repeat
          sj := DXFLines[j];
          if sj=code then begin
             Inc(j);
             Value := DXFLines[j];
             Result := True;
             Exit;
          end;
          inc(j);
    until sj='  0';
  end;

procedure TALCustomDXF.GetCurveParameters(cuv: TCurve; var idx: integer;
          var closed: boolean);
var x,y,r: double;
    sor,sor1 : string;
    dxfCode  : integer;
    arcSAngle,arcEAngle: double;
    arcPPP : T3Point2d;
    bulge  : double;
    oldIdx : integer;
    existEntity: integer;
    VertexCount: integer;
    BeginSplene: boolean; // if 10,20,30

Label ENDLABEL;

Function HexToDec( hex_szam : string ):word;
var   i: integer;
Const hx : String = '0123456789ABCDEF';
begin
  Result:=0;
  For i:=Length(hex_szam) downto 1 do begin
      Result:=Result+(Pos(hex_szam[i],hx)-1)*Trunc(Power(16,Length(hex_szam)-i));
  end;
end;

begin

  if Pos(ShapeName,DXFObjectNames)=0 then
     goto ENDLABEL;

  bulge := 0;
  VertexCount := 0;
  BeginSplene := False;

  while idx<Pred(DXFLines.Count) do
  begin
    sor := DXFLines.Strings[idx];
    sor1:= Trim(DXFLines.Strings[idx+1]);
    dxfCode := StrToInt(sor);

    if (sor1='VERTEX') and (cuv.Count=1)then
       Inc(VertexCount);

    Case dxfCode of
    1: cuv.Text := sor1;
    2:
    begin
       if cuv.Shape = dmInsert then
          cuv.BlockParams.BlockName := sor1;
    end;
    5: cuv.ID :=  HexToDec(sor1);
    // Layer
    8:
    begin
       if IsNum(trim(sor1)) then begin
          cuv.Layer := StrToInt(sor1);
          if (cuv.Layer>Pred(High(DXFLayerList))) or (Cuv.Layer<0) then
             cuv.Layer := GetLayerIndex(sor1);
       end
       else
          cuv.Layer := GetLayerIndex(sor1);
    end;
    // X
    10,11,12,13:
    begin
         if dxfCode=10 then BeginSplene := True;
         x:= StrToFloat(sor1);
    end;
    // Y
    20,21,22,23:
    begin
       y:= StrToFloat(sor1);
       if cuv.Shape = dmInsert then
       begin
          cuv.BlockParams.TranslateX := X;
          cuv.BlockParams.TranslateY := Y;
       end;
       if VertexCount=1 then
          cuv.ClearPoints;
       if BeginSplene then
          cuv.AddPoint(x,y);
       if DXFEntity = SOLID then
       if cuv.Count=4 then
       begin
          cuv.InterchangePoints(2,3);
       end;
    end;
    // Radius
    40:
    begin
       r:= StrToFloat(sor1);
       Case cuv.Shape of
       dmCircle : cuv.AddPoint(x+r,y);
       dmText   : cuv.FontHeight := -StrToFloat(sor1);
       end;
    end;
    41:
       Case cuv.Shape of
       dmInsert : cuv.BlockParams.Magnify := StrToFloat(sor1);
       End;
    42:
    begin
       r := StrToFloat(sor1);
       if r<>0 then bulge := r;
    end;
    50:
       Case cuv.Shape of
       dmArc    : arcSAngle := StrToFloat(sor1);
       dmInsert,dmText : cuv.BlockParams.Angle := DegToRad(StrToFloat(sor1));
       End;
    51:
       if cuv.Shape = dmArc then begin
          arcEAngle := StrToFloat(sor1);
          {Átszámítás 3 kerületi pontra}
          arcPPP := KorivbolHarompont(x,y,r,DegToRad(arcSAngle),DegToRad(arcEAngle));
          cuv.ClearPoints;
          cuv.AddPoint(arcPPP.p1);
          cuv.AddPoint(arcPPP.p2);
          cuv.AddPoint(arcPPP.p3);
       end;
    70:
    begin
          Closed := (StrToInt(sor1) and 1)=1;
          if cuv.Shape = dmPolyline then
          begin
             oldIdx := idx;
             if Loop(idx,'VERTEX') then
                Inc(idx)
             else
                idx := oldIdx;
          end;
    end;
    71:
    begin
       if cuv.Shape = dmSpline then
       case StrToInt(sor1) of
       1,3: // Nem megy át a támpontokon
            cuv.Shape := dmBSpline;
       2: // átmegy a támpontokon
            cuv.Shape := dmSpline;
       end;
    end;
    end;

    if ((sor='  0') and (sor1<>'VERTEX')) or (sor1='ENDBLK') then begin  // Exit
       Inc(idx,-2);
       break;
    end;

    Inc(idx,2);

  end;

ENDLABEL:
  if (cuv.Shape=dmPolyline) then begin
     cuv.Closed := (cuv.Points[0].x=cuv.LastPoint.x) and
                   (cuv.Points[0].y=cuv.LastPoint.y);
     if bulge<>0 then
       if DXFEntity IN [POLYLINE,LWPOLYLINE] then // Bulge (= dudor)
       if cuv.Count=2 then begin
          cuv.Shape := dmArc;
          arcPPP.p1 := cuv.Points[0];
          arcPPP.p3 := cuv.Points[1];
          // 3th point
          r := -arctan(r);
          arcPPP.p2 := Elometszes(arcPPP.p1,arcPPP.p3,r,r);
          cuv.ClearPoints;
          cuv.AddPoint(arcPPP.p1);
          cuv.AddPoint(arcPPP.p2);
          cuv.AddPoint(arcPPP.p3);
       end;
  end;
end;

end.

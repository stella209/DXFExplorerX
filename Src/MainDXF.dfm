object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 661
  ClientWidth = 1034
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1034
    Height = 31
    Align = alTop
    TabOrder = 0
    object lbCoord: TLabel
      Left = 336
      Top = 8
      Width = 135
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = '0 : 0'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object ALTimerSpeedButton1: TALTimerSpeedButton
      Left = 471
      Top = 5
      Width = 23
      Height = 22
      Hint = '+ Zoom'
      Caption = '+'
      Interval = 1
      TimerEneble = True
      OnTimerEvent = ALTimerSpeedButton1TimerEvent
    end
    object ALTimerSpeedButton2: TALTimerSpeedButton
      Left = 315
      Top = 5
      Width = 23
      Height = 22
      Hint = '- Zoom'
      Caption = '-'
      Interval = 1
      TimerEneble = True
      OnTimerEvent = ALTimerSpeedButton2TimerEvent
    end
    object SpeedButton1: TSpeedButton
      Left = 154
      Top = 3
      Width = 24
      Height = 24
      Hint = 'Zoom Paper'
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C006000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFF0000
        00FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0
        FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000FFFFFF000000FFFFFFFFFFFFFFFFFFC0C0C000
        0000000000C0C0C0FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF000000FFFFFFFFFF
        FFFFFFFFC0C0C0000000000000C0C0C0FFFFFF000000FFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
        FFFFFF000000FFFFFFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFF0000
        00FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFC0C0C0000000000000C0C0C0
        FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFC0C0C000
        0000000000C0C0C0FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
        00FFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000
        000000000000000000000000C0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0
        FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000
        00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C000
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0
        FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C000
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0FFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000000000C0C0C0
        FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000000000000000000000000000
        00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C000
        0000000000C0C0C0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFC0C0C0000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 175
      Top = 3
      Width = 24
      Height = 24
      Hint = 'Zoom Drawing'
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        0400000000002001000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000FFFFFFF
        000FFFFF0FFFFFFF0FFFFF00FFF00FFF0FFFFFFF0FFFFF0FFFFF0FFF0FFFFFFF
        0FFFF0FFFFFFF0FF0FFFFFFF0FFFF0FFFFFFF0FF0FFFFFFF0FFFF0FFFFFFF0FF
        0FFFFFFF0FFFFF0FFFFF0FFF0FFFFFFF0FFFFF00FFF00FFF000000000FFFFFFF
        000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFF
        FFFFFFFFF0FFFFFFFF0FFFFFFFFFFFFFF0FFFFFFF0FFFFFFFFFFFFFFF0FFFFFF
        0FFFFFFFFFFFFFFFF0FFFFF0FFFFFFFFFFFFFFFFF0FFFF0FFFFFFFFFFFFFFFFF
        F0FFF0FFFFFFFFFFFFFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFF0F0FFFFFFFFFFFF
        FFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = SpeedButton2Click
    end
    object SpeedButton3: TSpeedButton
      Left = 5
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Full Screen'
      Caption = '<>'
      OnClick = SpeedButton3Click
    end
    object RotLabel: TLabel
      Left = 641
      Top = 5
      Width = 40
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '0'
    end
    object CheckBox1: TCheckBox
      Left = 46
      Top = 8
      Width = 58
      Height = 17
      Caption = 'OpenGL'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object RotTrackBar: TTrackBar
      Left = 500
      Top = 3
      Width = 135
      Height = 22
      Hint = 'Rotate'
      Max = 359
      TabOrder = 1
      OnChange = RotTrackBarChange
      OnEnter = RotTrackBarEnter
      OnExit = RotTrackBarExit
    end
    object CheckBox2: TCheckBox
      Left = 205
      Top = 8
      Width = 50
      Height = 17
      Caption = 'Points'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object Poligonize: TButton
      Left = 714
      Top = 0
      Width = 69
      Height = 25
      Caption = 'Polygonize'
      TabOrder = 3
      Visible = False
    end
    object Button2: TButton
      Left = 785
      Top = 0
      Width = 62
      Height = 25
      Caption = 'Vectorize'
      TabOrder = 4
      Visible = False
    end
    object CheckBox4: TCheckBox
      Left = 104
      Top = 8
      Width = 55
      Height = 17
      Caption = 'Paper'
      TabOrder = 5
      OnClick = CheckBox4Click
    end
    object CheckBox3: TCheckBox
      Left = 258
      Top = 8
      Width = 51
      Height = 17
      Hint = 'Layer Color'
      Caption = 'Color'
      TabOrder = 6
      OnClick = CheckBox3Click
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 642
    Width = 1034
    Height = 19
    Panels = <
      item
        Width = 237
      end
      item
        Width = 50
      end>
    SimpleText = 'fdgdfgd'
  end
  object PageControl1: TPageControl
    Left = 262
    Top = 31
    Width = 507
    Height = 611
    ActivePage = TabSheet6
    Align = alClient
    TabOrder = 2
    object TabSheet6: TTabSheet
      Caption = 'View'
      object Sablon: TalCADViewGL
        Left = 0
        Top = 0
        Width = 499
        Height = 583
        OpenGLPaint = True
        BackColor = 15066597
        BackImage.Width = 0
        BackImage.Height = 0
        BackImage.Zoom = 1.000000000000000000
        BackImage.Visible = False
        CentralCross = False
        CentralisZoom = False
        CursorCross = False
        DigitAction = daSync
        EnableActions = True
        FillColor = clBlack
        Grid.Above = True
        Grid.Aligne = False
        Grid.MainGridColor = clGreen
        Grid.Margin = 0
        Grid.SubGridColor = clGray
        Grid.Style = gsLine
        Grid.Visible = False
        Grid.OnlyOnPaper = False
        Paper.Width = 210.000000000000000000
        Paper.Height = 297.000000000000000000
        Paper.Shadow = False
        Paper.Visible = False
        ShadeModel = smSmooth
        Zoom = 1.000000000000000000
        OnChangeWindow = SablonChangeWindow
        OnMouseLeave = SablonMouseLeave
        Align = alClient
        TabStop = True
        OnMouseMove = SablonMouseMove
        Append = False
        CADSource = alCADSource1
        LayerPaint = False
        CadPens.pClosed.Width = 2
        CadPens.pOpened.Color = clGray
        CadPens.pOpened.Style = psDot
        CadPens.pSelected.Color = clBlue
        CadPens.pSelected.Width = 3
        CadPens.pSigned.Color = clRed
        CadPens.pSigned.Width = 4
        CadPens.pCrossed.Color = clRed
        CadPens.pCrossed.Width = 4
        CadPens.pContour.Color = clRed
        CadPens.bFillBrush.Color = clSilver
        EnablePaint = False
        EnableRecall = False
        Filled = False
        PointWidth = 0
        ShowPoints = False
        SensitiveRadius = 4
        Hinted = True
        CoordHint = False
        VisibleBeginPoint = False
        OnChangeSelected = SablonChangeSelected
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Datagrid'
      ImageIndex = 1
      object Label2: TLabel
        Left = 49
        Top = 10
        Width = 101
        Height = 13
        Caption = 'Under Developement'
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Help'
      ImageIndex = 2
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 499
        Height = 583
        Align = alClient
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'RichEdit1')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object PageControl2: TPageControl
    Left = 0
    Top = 31
    Width = 262
    Height = 611
    ActivePage = TabSheet8
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    object TabSheet8: TTabSheet
      Caption = 'Open'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 254
        Height = 582
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        BevelInner = bvRaised
        BevelWidth = 2
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 4
          Top = 261
          Width = 246
          Height = 4
          Cursor = crVSplit
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          ExplicitLeft = 3
          ExplicitTop = 150
          ExplicitWidth = 182
        end
        object Panel3: TPanel
          Left = 4
          Top = 549
          Width = 246
          Height = 29
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 0
          object FilterComboBox1: TFilterComboBox
            Left = 1
            Top = 1
            Width = 244
            Height = 22
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            AutoDropDown = True
            FileList = FileListBox1
            Filter = 
              'All Drawing Files|*.sbn;*.dxf;*.plt|DRAWING NEW|*.sbn|DXF Files|' +
              '*.dxf|HPGL Plotter File|*.plt'
            TabOrder = 0
          end
        end
        object Panel4: TPanel
          Left = 4
          Top = 31
          Width = 246
          Height = 230
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          TabOrder = 1
          object DirectoryListBox1: TDirectoryListBox
            Left = 1
            Top = 1
            Width = 244
            Height = 228
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            FileList = FileListBox1
            TabOrder = 0
          end
        end
        object Panel5: TPanel
          Left = 4
          Top = 265
          Width = 246
          Height = 257
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alClient
          TabOrder = 2
          object FileListBox1: TFileListBox
            Left = 1
            Top = 1
            Width = 244
            Height = 255
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            FileEdit = Edit1
            Mask = '*.sbn;*.dxf;*.plt'
            ShowGlyphs = True
            TabOrder = 0
            OnClick = FileListBox1Click
          end
        end
        object Panel6: TPanel
          Left = 4
          Top = 522
          Width = 246
          Height = 27
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alBottom
          TabOrder = 3
          object DriveComboBox1: TDriveComboBox
            Left = 1
            Top = 1
            Width = 244
            Height = 20
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            AutoDropDown = True
            DirList = DirectoryListBox1
            TabOrder = 0
          end
        end
        object Panel7: TPanel
          Left = 4
          Top = 4
          Width = 246
          Height = 27
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 2
          Margins.Bottom = 2
          Align = alTop
          Caption = 'Panel7'
          TabOrder = 4
          object Edit1: TEdit
            Left = 1
            Top = 1
            Width = 244
            Height = 25
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            TabOrder = 0
            Text = '*.sbn;*.dxf;*.plt'
            ExplicitHeight = 22
          end
        end
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Structure'
      ImageIndex = 1
      object StructTreeView: TTreeView
        Left = 0
        Top = 0
        Width = 254
        Height = 582
        Align = alClient
        AutoExpand = True
        DragMode = dmAutomatic
        HotTrack = True
        Indent = 19
        MultiSelectStyle = [msControlSelect, msShiftSelect, msVisibleOnly, msSiblingOnly]
        PopupMenu = pmStruct
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        OnChange = StructTreeViewChange
      end
    end
  end
  object PageControl3: TPageControl
    Left = 769
    Top = 31
    Width = 265
    Height = 611
    ActivePage = TabSheet10
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'DXF'
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 257
        Height = 41
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 13
          Width = 29
          Height = 14
          Caption = 'Find:'
        end
        object SpeedButton6: TSpeedButton
          Left = 172
          Top = 9
          Width = 23
          Height = 22
          Caption = '<'
        end
        object SpeedButton7: TSpeedButton
          Left = 197
          Top = 9
          Width = 23
          Height = 22
          Caption = '>'
          OnClick = SpeedButton7Click
        end
        object SRCEdit: TEdit
          Left = 43
          Top = 9
          Width = 123
          Height = 22
          CharCase = ecUpperCase
          TabOrder = 0
          OnChange = SRCEditChange
        end
      end
      object Panel11: TPanel
        Left = 0
        Top = 517
        Width = 257
        Height = 65
        Align = alBottom
        TabOrder = 1
        object Button3: TButton
          Left = 18
          Top = 22
          Width = 75
          Height = 25
          Caption = 'Generate'
          TabOrder = 0
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 112
          Top = 22
          Width = 126
          Height = 25
          Caption = 'Save To DXF'
          TabOrder = 1
          OnClick = Button4Click
        end
      end
      object DXFMemo: TMemo
        Left = 0
        Top = 41
        Width = 257
        Height = 476
        Align = alClient
        CharCase = ecUpperCase
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        Lines.Strings = (
          '')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Object'
      ImageIndex = 1
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 257
        Height = 25
        Align = alTop
        TabOrder = 0
      end
      object ComponentInspector: TComponentInspector
        Left = 0
        Top = 25
        Width = 257
        Height = 220
        Align = alTop
        Color = clBtnFace
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        Splitter = 130
      end
      object Panel10: TPanel
        Left = 0
        Top = 541
        Width = 257
        Height = 41
        Align = alBottom
        Caption = 'Drawing Surface'
        TabOrder = 2
        object Button1: TButton
          Left = 28
          Top = 10
          Width = 205
          Height = 25
          Caption = 'Drawing Surface'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object TabSheet10: TTabSheet
      Caption = 'ObjList'
      ImageIndex = 2
      object ObjectList: TCheckListBox
        Left = 0
        Top = 0
        Width = 257
        Height = 544
        Hint = 'Popup menu = Right Click;  Dragging = Ctrl+Up/Down Arrow'
        OnClickCheck = ObjectListClickCheck
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BevelKind = bkFlat
        Color = clInfoBk
        DragMode = dmAutomatic
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clNavy
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        PopupMenu = pupObjList
        ShowHint = True
        TabOrder = 0
        StyleElements = [seFont, seBorder]
        OnClick = ObjectListClick
      end
      object Panel12: TPanel
        Left = 0
        Top = 544
        Width = 257
        Height = 38
        Align = alBottom
        Alignment = taLeftJustify
        Caption = '     Shape '
        TabOrder = 1
        object cbShape: TComboBox
          Left = 84
          Top = 6
          Width = 145
          Height = 22
          DropDownCount = 16
          TabOrder = 0
          OnChange = cbShapeChange
        end
      end
    end
    object TabSheet11: TTabSheet
      Caption = 'Layers'
      ImageIndex = 3
      object LayerGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 257
        Height = 582
        Align = alClient
        Ctl3D = False
        DefaultColWidth = 32
        DefaultRowHeight = 20
        DrawingStyle = gdsClassic
        ParentCtl3D = False
        ScrollBars = ssVertical
        TabOrder = 0
        OnDrawCell = LayerGridDrawCell
        OnSelectCell = LayerGridSelectCell
        ColWidths = (
          32
          95
          22
          26
          87)
      end
    end
    object TabSheet12: TTabSheet
      Caption = 'Blocks'
      ImageIndex = 4
      object Panel13: TPanel
        Left = 0
        Top = 0
        Width = 257
        Height = 145
        Align = alTop
        TabOrder = 0
        object BlkView: TalCADView
          Left = 54
          Top = 16
          Width = 141
          Height = 113
          Append = False
          BackColor = clWhite
          CadPens.pClosed.Width = 2
          CadPens.pOpened.Color = clGray
          CadPens.pOpened.Style = psDot
          CadPens.pSelected.Color = clBlue
          CadPens.pSelected.Width = 3
          CadPens.pSigned.Color = clRed
          CadPens.pSigned.Width = 4
          CadPens.pCrossed.Color = clRed
          CadPens.pCrossed.Width = 4
          CadPens.pContour.Color = clRed
          CadPens.bFillBrush.Color = clSilver
          CADSource = alCADSource2
          Hinted = False
          CoordHint = False
          CentralCross = False
          Filled = False
          FillBrush.Color = clYellow
          Grid.Above = False
          Grid.Aligne = False
          Grid.MainGridColor = clGreen
          Grid.Margin = 0
          Grid.SubGridColor = clSilver
          Grid.Style = gsLine
          Grid.Visible = False
          Paper.x = 210.000000000000000000
          Paper.y = 297.000000000000000000
          PaperColor = clWhite
          PaperVisible = False
          PointWidth = 3
          ShowPoints = False
          Zoom = 0.630914826498422700
          CentralisZoom = True
          VisibleBeginPoint = False
          Caption = 'BlkView'
          TabOrder = 0
          TabStop = True
        end
      end
      object BlkList: TListBox
        Left = 0
        Top = 145
        Width = 257
        Height = 437
        Align = alClient
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = BlkListClick
        OnDblClick = BlkListDblClick
      end
    end
  end
  object alCADSource1: TalCADSource
    Append = False
    Changed = False
    Left = 298
    Top = 130
  end
  object SaveDialog1: TSaveDialog
    Filter = 'DXF|*.dxf|Sablon|*.sbn'
    Left = 296
    Top = 184
  end
  object FindDialog1: TFindDialog
    Left = 706
    Top = 162
  end
  object ImageList1: TImageList
    Left = 362
    Top = 80
    Bitmap = {
      494C010103000800640110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000993300009933
      0000993300009933000099330000993300009933000099330000993300009933
      00009933000099330000993300000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003399CC00006699000066
      9900006699000066990000669900006699000066990000669900006699000066
      990066CCCC000000000000000000000000000000000099330000CC660000CC66
      000099330000E5E5E500CC66000099330000E5E5E500E5E5E500E5E5E5009933
      0000CC660000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC003399CC0099FFFF0066CC
      FF0066CCFF0066CCFF0066CCFF0066CCFF0066CCFF0066CCFF0066CCFF003399
      CC00006699000000000000000000000000000000000099330000CC660000CC66
      000099330000E5E5E500CC66000099330000E5E5E500E5E5E500E5E5E5009933
      0000CC660000CC660000993300000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC003399CC0066CCFF0099FF
      FF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0066CC
      FF00006699003399CC0000000000000000000000000099330000CC660000CC66
      000099330000E5E5E500CC66000099330000E5E5E500E5E5E500E5E5E5009933
      0000CC660000CC660000993300000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC003399CC0066CCFF0099FF
      FF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0066CC
      FF0066CCCC000066990000000000000000000000000099330000CC660000CC66
      000099330000E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E5009933
      0000CC660000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC0066CCFF003399CC0099FF
      FF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0066CC
      FF0099FFFF00006699003399CC00000000000000000099330000CC660000CC66
      0000CC660000993300009933000099330000993300009933000099330000CC66
      0000CC660000CC660000993300000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC0066CCFF0066CCCC0066CC
      CC0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0099FFFF0066CC
      FF0099FFFF0066CCCC0000669900000000000000000099330000CC660000CC66
      0000CC660000CC660000CC660000CC660000CC660000CC660000CC660000CC66
      0000CC660000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC0099FFFF0066CCFF003399
      CC00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF0099FF
      FF00CCFFFF00CCFFFF0000669900000000000000000099330000CC660000CC66
      0000993300009933000099330000993300009933000099330000993300009933
      0000CC660000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC0099FFFF0099FFFF0066CC
      FF003399CC003399CC003399CC003399CC003399CC003399CC003399CC003399
      CC003399CC003399CC0066CCFF00000000000000000099330000CC6600009933
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0099330000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003399CC00CCFFFF0099FFFF0099FF
      FF0099FFFF0099FFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF00CCFFFF000066
      9900000000000000000000000000000000000000000099330000CC6600009933
      0000FFFFFF00993300009933000099330000993300009933000099330000FFFF
      FF0099330000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003399CC00CCFFFF00CCFF
      FF00CCFFFF00CCFFFF003399CC003399CC003399CC003399CC003399CC000000
      0000000000000000000000000000000000000000000099330000CC6600009933
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0099330000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003399CC003399
      CC003399CC003399CC0000000000000000000000000000000000000000000000
      0000000000009933000099330000993300000000000099330000E5E5E5009933
      0000FFFFFF00993300009933000099330000993300009933000099330000FFFF
      FF009933000099330000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099330000993300000000000099330000CC6600009933
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0099330000CC660000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099330000000000000000
      0000000000009933000000000000993300000000000099330000993300009933
      0000993300009933000099330000993300009933000099330000993300009933
      00009933000099330000993300000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000993300009933
      0000993300000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFC00198110000
      800780018411000000078001A209000000038001A22100000003800184450000
      00018001984500000001800180010000000180018FC100000001800188410000
      000F800188410000801F800188410000C3F8800188410000FFFC80018FC10000
      FFBA800180010000FFC7FFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Left = 296
    Top = 86
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Reopen1: TMenuItem
        Caption = 'Reopen'
        ShortCut = 16469
        OnClick = Reopen1Click
      end
      object Append1: TMenuItem
        Caption = 'Append'
        OnClick = Append1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object SaveAsDXF1: TMenuItem
        Caption = 'Save As DXF'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      object Cut1: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
        Visible = False
      end
      object Copy1: TMenuItem
        Caption = 'Copy Draw'
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Pate1: TMenuItem
        Caption = 'Pate'
        ShortCut = 16470
        Visible = False
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Scale1: TMenuItem
        Caption = 'Scale...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object CentralCross1: TMenuItem
        Caption = 'Central Cross'
        ShortCut = 16459
        OnClick = CentralCross1Click
      end
      object CursorCross1: TMenuItem
        Caption = 'Cursor Cross'
        OnClick = CursorCross1Click
      end
      object Grid1: TMenuItem
        Caption = 'Grid'
        ShortCut = 16455
        OnClick = Grid1Click
      end
      object Pointsnodes1: TMenuItem
        Caption = 'Points/nodes'
        ShortCut = 115
        OnClick = Pointsnodes1Click
      end
      object Hinted1: TMenuItem
        Caption = 'Hinted'
        Checked = True
        OnClick = Hinted1Click
      end
      object LayerColor1: TMenuItem
        Caption = 'Layer Color'
        OnClick = LayerColor1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object HelpContext1: TMenuItem
        Caption = 'Help'
        ShortCut = 112
        OnClick = HelpContext1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Language1: TMenuItem
        Caption = 'Language'
        object English1: TMenuItem
          AutoCheck = True
          Caption = 'English'
          Checked = True
          RadioItem = True
          OnClick = English1Click
        end
        object Hungarian1: TMenuItem
          Caption = 'Hungarian'
          RadioItem = True
          OnClick = Hungarian1Click
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object ColorDialog: TColorDialog
    Left = 706
    Top = 211
  end
  object alCADSource2: TalCADSource
    Append = False
    Changed = False
    Left = 710
    Top = 80
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'DXF'
    Filter = 'DXF file|*.DXF'
    Left = 656
    Top = 368
  end
  object pupObjList: TPopupMenu
    Left = 836
    Top = 388
    object RefreshList1: TMenuItem
      Caption = 'Refresh List'
      OnClick = RefreshList1Click
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object UnselectAll1: TMenuItem
      Caption = 'Unselect All'
      OnClick = UnselectAll1Click
    end
    object InversSelection1: TMenuItem
      Caption = 'Invers Selection'
      OnClick = InversSelection1Click
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object DeleteSelected1: TMenuItem
      Caption = 'Delete Selected'
      OnClick = DeleteSelected1Click
    end
  end
  object pmStruct: TPopupMenu
    Left = 101
    Top = 174
    object Expand1: TMenuItem
      Caption = 'Expand'
      OnClick = Expand1Click
    end
    object Collapse1: TMenuItem
      Caption = 'Collapse'
      OnClick = Collapse1Click
    end
  end
  object janLanguage1: TjanLanguage
    SaveAble = False
    Left = 435
    Top = 80
  end
end

object Form1: TForm1
  Left = 94
  Top = 131
  Width = 701
  Height = 617
  Caption = 'mPascal ACDI String Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 693
    Height = 590
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'ACDI'
      DesignSize = (
        685
        562)
      object Label1: TLabel
        Left = 16
        Top = 48
        Width = 63
        Height = 13
        Caption = 'Manufacturer'
      end
      object Label2: TLabel
        Left = 16
        Top = 80
        Width = 57
        Height = 13
        Caption = 'Node Name'
      end
      object Label3: TLabel
        Left = 16
        Top = 112
        Width = 84
        Height = 13
        Caption = 'Hardware Version'
      end
      object Label4: TLabel
        Left = 16
        Top = 144
        Width = 80
        Height = 13
        Caption = 'Software Version'
      end
      object Label5: TLabel
        Left = 16
        Top = 225
        Width = 93
        Height = 13
        Caption = 'User Defined Name'
      end
      object Label6: TLabel
        Left = 16
        Top = 257
        Width = 118
        Height = 13
        Caption = 'User Defined Description'
      end
      object LabelMfgInfoVer: TLabel
        Left = 16
        Top = 16
        Width = 122
        Height = 13
        Caption = 'Manufacturer Info Version'
      end
      object LabelUserInfoVersion: TLabel
        Left = 32
        Top = 193
        Width = 101
        Height = 13
        Caption = 'LabelUserInfoVersion'
      end
      object EditMfg: TEdit
        Left = 160
        Top = 40
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = EditMfgChange
      end
      object EditName: TEdit
        Left = 160
        Top = 72
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = EditNameChange
      end
      object EditHWVersion: TEdit
        Left = 160
        Top = 104
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = EditHWVersionChange
      end
      object EditSWVersion: TEdit
        Left = 160
        Top = 136
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = EditSWVersionChange
      end
      object EditUserDefinedName: TEdit
        Left = 160
        Top = 217
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = EditUserDefinedNameChange
      end
      object EditUserDefinedDesc: TEdit
        Left = 160
        Top = 249
        Width = 299
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = EditUserDefinedDescChange
      end
      object RichEdit1: TRichEdit
        Left = 16
        Top = 344
        Width = 657
        Height = 201
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 6
      end
      object ButtonGenerateCode: TButton
        Left = 16
        Top = 297
        Width = 137
        Height = 25
        Caption = 'Generate Code'
        TabOrder = 7
        OnClick = ButtonGenerateCodeClick
      end
      object ButtonDecompile: TButton
        Left = 168
        Top = 297
        Width = 137
        Height = 25
        Caption = 'Decompile'
        TabOrder = 8
        OnClick = ButtonDecompileClick
      end
      object EditMfgInfoVer: TEdit
        Left = 160
        Top = 8
        Width = 48
        Height = 21
        TabOrder = 9
        Text = '1'
        OnExit = EditMfgInfoVerExit
        OnKeyPress = EditMfgInfoVerKeyPress
      end
      object EditUserInfoVer: TEdit
        Left = 160
        Top = 185
        Width = 48
        Height = 21
        TabOrder = 10
        Text = '1'
      end
      object GroupBox1: TGroupBox
        Left = 468
        Top = 30
        Width = 214
        Height = 289
        Anchors = [akTop, akRight]
        Caption = 'Counts (Including nulls)'
        TabOrder = 11
        object Bevel1: TBevel
          Left = 5
          Top = 128
          Width = 204
          Height = 49
        end
        object Bevel2: TBevel
          Left = 5
          Top = 184
          Width = 204
          Height = 65
        end
        object LabelMfgLen: TLabel
          Left = 15
          Top = 17
          Width = 6
          Height = 13
          Caption = '0'
        end
        object LabelNodeNameLen: TLabel
          Left = 15
          Top = 49
          Width = 6
          Height = 13
          Caption = '0'
        end
        object LabelHardwareLen: TLabel
          Left = 15
          Top = 80
          Width = 6
          Height = 13
          Caption = '0'
        end
        object LabelSoftwareLen: TLabel
          Left = 15
          Top = 112
          Width = 6
          Height = 13
          Caption = '0'
        end
        object LabelUserName: TLabel
          Left = 15
          Top = 193
          Width = 6
          Height = 13
          Caption = '0'
        end
        object LabelUserDesc: TLabel
          Left = 15
          Top = 225
          Width = 6
          Height = 13
          Caption = '0'
        end
        object Label13: TLabel
          Left = 6
          Top = 257
          Width = 55
          Height = 13
          Caption = 'Total Count'
        end
        object LabelTotalCount: TLabel
          Left = 70
          Top = 257
          Width = 6
          Height = 13
          Caption = '0'
        end
        object Label15: TLabel
          Left = 111
          Top = 136
          Width = 90
          Height = 13
          Caption = 'OpenLCB Limits'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelMaxDesc: TLabel
          Left = 181
          Top = 225
          Width = 15
          Height = 13
          Caption = '40'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelUserMaxChar: TLabel
          Left = 181
          Top = 193
          Width = 15
          Height = 13
          Caption = '20'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelMaxMfg: TLabel
          Left = 181
          Top = 153
          Width = 15
          Height = 13
          Caption = '64'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label19: TLabel
          Left = 77
          Top = 153
          Width = 92
          Height = 13
          Caption = '(Recommended)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelMfgSubTotal: TLabel
          Left = 15
          Top = 153
          Width = 6
          Height = 13
          Caption = '0'
        end
        object Label21: TLabel
          Left = 15
          Top = 137
          Width = 43
          Height = 13
          Caption = 'SubTotal'
        end
      end
      object CheckBoxACDI: TCheckBox
        Left = 328
        Top = 304
        Width = 97
        Height = 17
        Caption = 'For Virtual Node ACDI'
        TabOrder = 12
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'CDI'
      ImageIndex = 1
      DesignSize = (
        685
        562)
      object Label7: TLabel
        Left = 8
        Top = 40
        Width = 41
        Height = 13
        Caption = 'XML File'
      end
      object Label8: TLabel
        Left = 8
        Top = 72
        Width = 88
        Height = 13
        Caption = 'CDI Array Identifier'
      end
      object Label9: TLabel
        Left = 8
        Top = 96
        Width = 85
        Height = 13
        Caption = 'CDI Map Identifier'
      end
      object Label10: TLabel
        Left = 8
        Top = 120
        Width = 111
        Height = 13
        Caption = 'Max CDI Array Identifier'
      end
      object Label11: TLabel
        Left = 16
        Top = 528
        Width = 280
        Height = 13
        Caption = 'Manufacturer Total Strings Length (64 Max Recommended)'
      end
      object LabelMaxMfgStrLen: TLabel
        Left = 312
        Top = 528
        Width = 6
        Height = 13
        Caption = '0'
      end
      object EditXMLFile: TEdit
        Left = 64
        Top = 32
        Width = 505
        Height = 21
        TabOrder = 0
      end
      object Button1: TButton
        Left = 576
        Top = 32
        Width = 75
        Height = 25
        Caption = 'Open...'
        TabOrder = 1
        OnClick = Button1Click
      end
      object MemoCDI: TMemo
        Left = 8
        Top = 184
        Width = 668
        Height = 329
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object Button2: TButton
        Left = 8
        Top = 144
        Width = 129
        Height = 25
        Caption = 'Generate Code'
        TabOrder = 3
        OnClick = Button2Click
      end
      object EditCDIArray: TEdit
        Left = 128
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 4
        Text = 'CDI_ARRAY'
      end
      object EditCDIMap: TEdit
        Left = 128
        Top = 88
        Width = 121
        Height = 21
        TabOrder = 5
        Text = 'CDI_MAP'
      end
      object EditMaxCDIArray: TEdit
        Left = 128
        Top = 112
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'MAX_CDI_ARRAY'
      end
      object CheckBoxVirtualNodeCDI: TCheckBox
        Left = 168
        Top = 152
        Width = 97
        Height = 17
        Caption = 'For Virtual Node'
        TabOrder = 7
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'XML Files|*.XML'
    Left = 612
    Top = 32
  end
end
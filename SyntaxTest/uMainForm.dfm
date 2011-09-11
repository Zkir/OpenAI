object Form1: TForm1
  Left = 230
  Top = 25
  Caption = 'Form1'
  ClientHeight = 506
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 152
    Width = 105
    Height = 13
    Caption = 'Label1'
  end
  object Memo1: TMemo
    Left = 16
    Top = 16
    Width = 601
    Height = 105
    Lines.Strings = (
      #1052#1072#1084#1072' '#1084#1099#1083#1072' '#1088#1072#1084#1091)
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 16
    Top = 192
    Width = 601
    Height = 297
    Lines.Strings = (
      #1056#1077#1079#1091#1083#1100#1090#1072#1090)
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button1: TButton
    Left = 16
    Top = 144
    Width = 75
    Height = 25
    Caption = #1056#1072#1079#1073#1086#1088'!'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 144
    Width = 75
    Height = 25
    Caption = #1080#1079' '#1092#1072#1081#1083#1072
    TabOrder = 3
    OnClick = Button2Click
  end
  object XMLDocument1: TXMLDocument
    Left = 448
    Top = 144
    DOMVendorDesc = 'MSXML'
  end
end

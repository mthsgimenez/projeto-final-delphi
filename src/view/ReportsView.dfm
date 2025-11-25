object formReports: TformReports
  Left = 0
  Top = 0
  Caption = 'formReports'
  ClientHeight = 561
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pcontrolReports: TPageControl
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tabStock
    Align = alClient
    TabOrder = 0
    object tabSpent: TTabSheet
      Caption = 'Gastos'
      object labelStartDate: TLabel
        Left = 264
        Top = 147
        Width = 58
        Height = 15
        Caption = 'A partir de:'
      end
      object labelEndDate: TLabel
        Left = 264
        Top = 211
        Width = 21
        Height = 15
        Caption = 'At'#233':'
      end
      object buttonShowSpent: TButton
        Left = 264
        Top = 288
        Width = 153
        Height = 33
        Caption = 'Emitir relat'#243'rio'
        TabOrder = 0
        OnClick = buttonShowSpentClick
      end
      object editSpentStartDate: TEdit
        Left = 264
        Top = 168
        Width = 121
        Height = 23
        TabOrder = 1
        TextHint = '30/12/2000'
      end
      object editSpentEndDate: TEdit
        Left = 264
        Top = 232
        Width = 121
        Height = 23
        TabOrder = 2
        TextHint = '30/12/2001'
      end
    end
    object tabUsage: TTabSheet
      Caption = 'Uso'
      ImageIndex = 1
      object Label1: TLabel
        Left = 272
        Top = 155
        Width = 58
        Height = 15
        Caption = 'A partir de:'
      end
      object Label2: TLabel
        Left = 272
        Top = 219
        Width = 21
        Height = 15
        Caption = 'At'#233':'
      end
      object editUsageStartDate: TEdit
        Left = 272
        Top = 176
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = '30/12/2000'
      end
      object editUsageEndDate: TEdit
        Left = 272
        Top = 240
        Width = 121
        Height = 23
        TabOrder = 1
        TextHint = '30/12/2001'
      end
      object buttonShowUsage: TButton
        Left = 272
        Top = 296
        Width = 153
        Height = 33
        Caption = 'Emitir relat'#243'rio'
        TabOrder = 2
        OnClick = buttonShowUsageClick
      end
    end
    object tabStock: TTabSheet
      Caption = 'Estoque'
      ImageIndex = 2
      object Label3: TLabel
        Left = 240
        Top = 219
        Width = 99
        Height = 15
        Caption = 'Estoque abaixo de:'
      end
      object editStockLimit: TEdit
        Left = 345
        Top = 216
        Width = 121
        Height = 23
        NumbersOnly = True
        TabOrder = 0
        TextHint = '5'
      end
      object buttonShowStock: TButton
        Left = 272
        Top = 272
        Width = 153
        Height = 41
        Caption = 'Emitir relat'#243'rio'
        TabOrder = 1
        OnClick = buttonShowStockClick
      end
    end
  end
end

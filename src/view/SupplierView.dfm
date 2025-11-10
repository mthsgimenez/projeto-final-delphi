object formSupplier: TformSupplier
  Left = 0
  Top = 0
  Caption = 'formSupplier'
  ClientHeight = 561
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pcontrolSupplier: TPageControl
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tabCreate
    Align = alClient
    TabOrder = 0
    object tabView: TTabSheet
      Caption = 'tabList'
      TabVisible = False
      object gridSuppliers: TStringGrid
        Left = 32
        Top = 24
        Width = 657
        Height = 441
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        OnDrawCell = gridSuppliersDrawCell
      end
      object buttonCreate: TButton
        Left = 32
        Top = 495
        Width = 145
        Height = 45
        Caption = 'Cadastrar fornecedor'
        TabOrder = 1
      end
      object buttonDelete: TButton
        Left = 544
        Top = 495
        Width = 145
        Height = 45
        Caption = 'Excluir fornecedor'
        TabOrder = 2
      end
      object buttonEdit: TButton
        Left = 288
        Top = 495
        Width = 145
        Height = 45
        Caption = 'Editar dados fornecedor'
        TabOrder = 3
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object panelCNPJ: TPanel
        Left = 136
        Top = 152
        Width = 145
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelCNPJ'
        ShowCaption = False
        TabOrder = 0
        object Label1: TLabel
          Left = 9
          Top = 3
          Width = 30
          Height = 15
          Caption = 'CNPJ:'
        end
        object imgSearch: TImage
          Left = 111
          Top = 22
          Width = 24
          Height = 24
          Picture.Data = {
            0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
            2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
            7078222076696577426F783D2230202D39363020393630203936302220776964
            74683D2232347078222066696C6C3D2223363636363636223E3C706174682064
            3D224D3738342D313230203533322D333732712D33302032342D363920333874
            2D3833203134712D31303920302D3138342E352D37352E35543132302D353830
            71302D3130392037352E352D3138342E35543338302D38343071313039203020
            3138342E352037352E35543634302D35383071302034342D3134203833742D33
            382036396C323532203235322D35362035365A4D3338302D3430307137352030
            203132372E352D35322E35543536302D35383071302D37352D35322E352D3132
            372E35543338302D373630712D373520302D3132372E352035322E3554323030
            2D35383071302037352035322E35203132372E35543338302D3430305A222F3E
            3C2F7376673E}
        end
        object MaskEdit1: TMaskEdit
          Left = 8
          Top = 22
          Width = 97
          Height = 23
          EditMask = '99 999 999\/9999\-99;0;_'
          MaxLength = 18
          TabOrder = 0
          Text = ''
          TextHint = '00.000.000/0000-00'
        end
      end
      object panelTradeName: TPanel
        Left = 376
        Top = 200
        Width = 185
        Height = 41
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 1
      end
    end
  end
end

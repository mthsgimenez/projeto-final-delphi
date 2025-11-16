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
  OnCreate = FormCreate
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
    ActivePage = tabView
    Align = alClient
    TabOrder = 0
    object tabView: TTabSheet
      Caption = 'tabList'
      TabVisible = False
      OnShow = tabViewShow
      object gridSuppliers: TStringGrid
        Left = 32
        Top = 24
        Width = 657
        Height = 425
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ColCount = 6
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridSuppliersDrawCell
        OnSelectCell = gridSuppliersSelectCell
      end
      object buttonCreate: TButton
        Left = 32
        Top = 487
        Width = 145
        Height = 45
        Caption = 'Cadastrar fornecedor'
        TabOrder = 1
        Visible = False
        OnClick = buttonCreateClick
      end
      object buttonDelete: TButton
        Left = 544
        Top = 487
        Width = 145
        Height = 45
        Caption = 'Excluir fornecedor'
        TabOrder = 2
        Visible = False
        OnClick = buttonDeleteClick
      end
      object buttonEdit: TButton
        Left = 288
        Top = 487
        Width = 145
        Height = 45
        Caption = 'Editar dados fornecedor'
        TabOrder = 3
        Visible = False
        OnClick = buttonEditClick
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object panelCNPJ: TPanel
        Left = 16
        Top = 176
        Width = 165
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
          Left = 135
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
          OnClick = imgSearchClick
        end
        object editCNPJ: TMaskEdit
          Left = 8
          Top = 22
          Width = 121
          Height = 23
          EditMask = '99 999 999\/9999\-99;0;_'
          MaxLength = 18
          TabOrder = 0
          Text = ''
          TextHint = '00.000.000/0000-00'
        end
      end
      object panelTradeName: TPanel
        Left = 197
        Top = 176
        Width = 193
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 1
        object labelTradeName: TLabel
          Left = 9
          Top = 3
          Width = 80
          Height = 15
          Caption = 'Nome fantasia:'
        end
        object editTradeName: TEdit
          Left = 8
          Top = 22
          Width = 177
          Height = 23
          TabOrder = 0
        end
      end
      object panelLegalName: TPanel
        Left = 408
        Top = 176
        Width = 193
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 2
        object labelLegalName: TLabel
          Left = 9
          Top = 3
          Width = 67
          Height = 15
          Caption = 'Raz'#227'o social:'
        end
        object editLegalName: TEdit
          Left = 8
          Top = 22
          Width = 177
          Height = 23
          TabOrder = 0
        end
      end
      object panelEmail: TPanel
        Left = 197
        Top = 254
        Width = 193
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 3
        object labelEmail: TLabel
          Left = 9
          Top = 3
          Width = 32
          Height = 15
          Caption = 'Email:'
        end
        object editEmail: TEdit
          Left = 8
          Top = 22
          Width = 177
          Height = 23
          TabOrder = 0
        end
      end
      object panelCEP: TPanel
        Left = 621
        Top = 176
        Width = 92
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 4
        object labelCEP: TLabel
          Left = 9
          Top = 3
          Width = 24
          Height = 15
          Caption = 'CEP:'
        end
        object editCEP: TMaskEdit
          Left = 8
          Top = 22
          Width = 73
          Height = 23
          TabOrder = 0
          Text = ''
        end
      end
      object panelPhone: TPanel
        Left = 408
        Top = 254
        Width = 129
        Height = 49
        BevelOuter = bvNone
        Caption = 'panelTradeName'
        ShowCaption = False
        TabOrder = 5
        object labelPhone: TLabel
          Left = 9
          Top = 3
          Width = 29
          Height = 15
          Caption = 'Fone:'
        end
        object editPhone: TMaskEdit
          Left = 9
          Top = 22
          Width = 112
          Height = 23
          TabOrder = 0
          Text = ''
        end
      end
      object buttonSave: TButton
        Left = 376
        Top = 392
        Width = 145
        Height = 45
        Caption = 'Salvar'
        TabOrder = 6
        OnClick = buttonSaveClick
      end
      object buttonBack: TButton
        Left = 205
        Top = 392
        Width = 145
        Height = 45
        Caption = 'Cancelar'
        TabOrder = 7
        OnClick = buttonBackClick
      end
    end
  end
end

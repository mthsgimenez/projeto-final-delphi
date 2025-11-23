object formOrder: TformOrder
  Left = 0
  Top = 0
  Caption = 'formOrder'
  ClientHeight = 561
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pcontrolOrders: TPageControl
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tabList
    Align = alClient
    TabOrder = 0
    object tabList: TTabSheet
      Caption = 'tabList'
      TabVisible = False
      OnShow = tabListShow
      object labelType: TLabel
        Left = 136
        Top = 13
        Width = 26
        Height = 15
        Caption = 'Tipo:'
      end
      object labelStatus: TLabel
        Left = 344
        Top = 13
        Width = 35
        Height = 15
        Caption = 'Status:'
      end
      object gridOrders: TStringGrid
        Left = 15
        Top = 50
        Width = 700
        Height = 449
        FixedCols = 0
        TabOrder = 0
        OnSelectCell = gridOrdersSelectCell
      end
      object comboType: TComboBox
        Left = 172
        Top = 10
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          'Ordem de compra'
          'Ordem de servi'#231'o')
      end
      object comboStatus: TComboBox
        Left = 386
        Top = 10
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 2
        Items.Strings = (
          'EM ABERTO'
          'FECHADO'
          'CANCELADO')
      end
      object buttonFilter: TButton
        Left = 640
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Filtrar'
        TabOrder = 3
        OnClick = buttonFilterClick
      end
      object buttonClear: TButton
        Left = 15
        Top = 10
        Width = 90
        Height = 25
        Caption = 'Limpar filtros'
        TabOrder = 4
        OnClick = buttonClearClick
      end
      object buttonCreatePurchase: TButton
        Left = 392
        Top = 505
        Width = 147
        Height = 43
        Caption = 'Emitir ordem de compra'
        TabOrder = 5
        OnClick = buttonCreatePurchaseClick
      end
      object buttonCreateService: TButton
        Left = 568
        Top = 505
        Width = 147
        Height = 43
        Caption = 'Emitir ordem de servi'#231'o'
        TabOrder = 6
        OnClick = buttonCreateServiceClick
      end
      object buttonCancel: TButton
        Left = 15
        Top = 505
        Width = 147
        Height = 43
        Caption = 'Cancelar ordem'
        TabOrder = 7
        OnClick = buttonCancelClick
      end
    end
    object tabCreatePurchase: TTabSheet
      Caption = 'tabCreatePurchase'
      ImageIndex = 1
      TabVisible = False
      OnHide = tabCreatePurchaseHide
      OnShow = tabCreatePurchaseShow
      object labelPrice: TLabel
        Left = 32
        Top = 240
        Width = 28
        Height = 15
        Caption = 'Total:'
      end
      object buttonBack: TButton
        Left = 56
        Top = 480
        Width = 161
        Height = 41
        Caption = 'Cancelar'
        TabOrder = 0
        OnClick = buttonBackClick
      end
      object buttonSavePurchase: TButton
        Left = 520
        Top = 480
        Width = 161
        Height = 41
        Caption = 'Emitir ordem de compra'
        TabOrder = 1
        OnClick = buttonSavePurchaseClick
      end
      object listPreview: TListBox
        Left = 32
        Top = 79
        Width = 273
        Height = 145
        ItemHeight = 15
        TabOrder = 2
      end
      object editSupplier: TEdit
        Left = 32
        Top = 32
        Width = 161
        Height = 23
        Enabled = False
        TabOrder = 3
        Text = 'Fornecedor'
      end
      object buttonPickSupplier: TButton
        Left = 216
        Top = 31
        Width = 137
        Height = 25
        Caption = 'Escolher fornecedor'
        TabOrder = 4
        OnClick = buttonPickSupplierClick
      end
      object editToolType: TEdit
        Left = 328
        Top = 80
        Width = 161
        Height = 23
        Enabled = False
        TabOrder = 5
        Text = 'Modelo'
      end
      object buttonPickToolType: TButton
        Left = 504
        Top = 79
        Width = 137
        Height = 25
        Caption = 'Escolher ferramenta'
        TabOrder = 6
        OnClick = buttonPickToolTypeClick
      end
      object editQuantity: TEdit
        Left = 328
        Top = 120
        Width = 121
        Height = 23
        NumbersOnly = True
        TabOrder = 7
        TextHint = 'Quantidade'
      end
      object buttonAddToolType: TButton
        Left = 328
        Top = 159
        Width = 75
        Height = 25
        Caption = 'Adicionar'
        TabOrder = 8
        OnClick = buttonAddToolTypeClick
      end
    end
    object tabCreateService: TTabSheet
      Caption = 'tabCreateService'
      ImageIndex = 2
      TabVisible = False
      OnHide = tabCreateServiceHide
      OnShow = tabCreateServiceShow
      object labelSupplier2: TLabel
        Left = 51
        Top = 43
        Width = 63
        Height = 15
        Caption = 'Fornecedor:'
      end
      object labelTool: TLabel
        Left = 323
        Top = 104
        Width = 63
        Height = 15
        Caption = 'Ferramenta:'
      end
      object labelPrice2: TLabel
        Left = 353
        Top = 147
        Width = 33
        Height = 15
        Caption = 'Pre'#231'o:'
      end
      object labelDisplayPrice: TLabel
        Left = 51
        Top = 296
        Width = 93
        Height = 15
        Caption = 'Valor total: R$0,00'
      end
      object editSupplier2: TEdit
        Left = 120
        Top = 40
        Width = 121
        Height = 23
        Enabled = False
        TabOrder = 0
      end
      object buttonPickSupplier2: TButton
        Left = 264
        Top = 39
        Width = 129
        Height = 25
        Caption = 'Escolher fornecedor'
        TabOrder = 1
        OnClick = buttonPickSupplier2Click
      end
      object listPreviewService: TListBox
        Left = 51
        Top = 88
        Width = 249
        Height = 193
        ItemHeight = 15
        TabOrder = 2
      end
      object editTool: TEdit
        Left = 400
        Top = 101
        Width = 121
        Height = 23
        Enabled = False
        TabOrder = 3
      end
      object buttonPickTool: TButton
        Left = 536
        Top = 100
        Width = 75
        Height = 25
        Caption = 'Escolher'
        TabOrder = 4
        OnClick = buttonPickToolClick
      end
      object editPrice: TEdit
        Left = 400
        Top = 144
        Width = 121
        Height = 23
        TabOrder = 5
        OnExit = editPriceExit
      end
      object buttonAddTool: TButton
        Left = 400
        Top = 192
        Width = 121
        Height = 33
        Caption = 'Adicionar'
        TabOrder = 6
        OnClick = buttonAddToolClick
      end
      object buttonBack2: TButton
        Left = 51
        Top = 464
        Width = 169
        Height = 49
        Caption = 'Voltar'
        TabOrder = 7
        OnClick = buttonBack2Click
      end
      object buttonSaveService: TButton
        Left = 491
        Top = 464
        Width = 169
        Height = 49
        Caption = 'Emitir ordem de servi'#231'o'
        TabOrder = 8
        OnClick = buttonSaveServiceClick
      end
    end
  end
  object panelPicker: TPanel
    Left = 32
    Top = 31
    Width = 665
    Height = 409
    BevelOuter = bvNone
    BorderWidth = 1
    BorderStyle = bsSingle
    Caption = 'panelPicker'
    Color = clSnow
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    Visible = False
    object gridPick: TStringGrid
      Left = 20
      Top = 24
      Width = 620
      Height = 329
      FixedCols = 0
      TabOrder = 0
      OnSelectCell = gridPickSelectCell
    end
    object buttonCancelPick: TButton
      Left = 20
      Top = 368
      Width = 105
      Height = 25
      Caption = 'Voltar'
      TabOrder = 1
      OnClick = buttonCancelPickClick
    end
    object buttonPick: TButton
      Left = 536
      Top = 367
      Width = 105
      Height = 25
      Caption = 'Escolher'
      TabOrder = 2
      OnClick = buttonPickClick
    end
  end
end

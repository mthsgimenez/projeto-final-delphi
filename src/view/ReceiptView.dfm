object formReceipt: TformReceipt
  Left = 0
  Top = 0
  Caption = 'formReceipt'
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
  object panelReceipt: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    Caption = 'panelReceipt'
    ShowCaption = False
    TabOrder = 0
    object labelType: TLabel
      Left = 183
      Top = 13
      Width = 26
      Height = 15
      Caption = 'Tipo:'
    end
    object gridOrders: TStringGrid
      Left = 16
      Top = 39
      Width = 700
      Height = 451
      BevelOuter = bvNone
      BorderStyle = bsNone
      FixedCols = 0
      TabOrder = 0
      OnDrawCell = gridOrdersDrawCell
      OnSelectCell = gridOrdersSelectCell
    end
    object buttonClearFilter: TButton
      Left = 16
      Top = 9
      Width = 97
      Height = 25
      Caption = 'Limpar filtro'
      TabOrder = 1
      OnClick = buttonClearFilterClick
    end
    object comboType: TComboBox
      Left = 215
      Top = 10
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Ordens de compra'
        'Ordens de servi'#231'o')
    end
    object buttonFilter: TButton
      Left = 624
      Top = 9
      Width = 92
      Height = 25
      Caption = 'Filtrar'
      TabOrder = 3
      OnClick = buttonFilterClick
    end
    object buttonCloseOrder: TButton
      Left = 569
      Top = 504
      Width = 147
      Height = 41
      Caption = 'Baixar ordem'
      TabOrder = 4
      OnClick = buttonCloseOrderClick
    end
    object buttonDetails: TButton
      Left = 16
      Top = 504
      Width = 137
      Height = 37
      Caption = 'Visualizar detalhes'
      TabOrder = 5
    end
    object panelClosePurchaseOrder: TPanel
      Left = 96
      Top = 88
      Width = 489
      Height = 289
      BorderWidth = 1
      BorderStyle = bsSingle
      Caption = 'panelClosePurchaseOrder'
      Color = clSnow
      ParentBackground = False
      ShowCaption = False
      TabOrder = 6
      Visible = False
      object Label1: TLabel
        Left = 20
        Top = 11
        Width = 107
        Height = 15
        Caption = 'Destinar ao estoque:'
      end
      object buttonClosePurchaseOrder: TButton
        Left = 324
        Top = 237
        Width = 145
        Height = 28
        Caption = 'Baixar ordem de compra'
        TabOrder = 0
        OnClick = buttonClosePurchaseOrderClick
      end
      object buttonCancel: TButton
        Left = 20
        Top = 237
        Width = 122
        Height = 28
        Caption = 'Cancelar'
        TabOrder = 1
        OnClick = buttonCancelClick
      end
      object listStorages: TListBox
        Left = 21
        Top = 32
        Width = 444
        Height = 199
        ItemHeight = 15
        TabOrder = 2
      end
    end
  end
end

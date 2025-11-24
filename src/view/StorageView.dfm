object formStorage: TformStorage
  Left = 0
  Top = 0
  Caption = 'formStorage'
  ClientHeight = 561
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pcontrolStorage: TPageControl
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    ActivePage = tabTools
    Align = alClient
    TabOrder = 0
    object tabList: TTabSheet
      Caption = 'tabList'
      TabVisible = False
      OnShow = tabListShow
      object gridStorages: TStringGrid
        Left = 13
        Top = 16
        Width = 700
        Height = 473
        ColCount = 4
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridStoragesDrawCell
        OnSelectCell = gridStoragesSelectCell
      end
      object buttonCreate: TButton
        Left = 16
        Top = 504
        Width = 145
        Height = 33
        Caption = 'Criar estoque'
        TabOrder = 1
        OnClick = buttonCreateClick
      end
      object buttonEdit: TButton
        Left = 167
        Top = 504
        Width = 145
        Height = 33
        Caption = 'Editar estoque'
        TabOrder = 2
        OnClick = buttonEditClick
      end
      object buttonDelete: TButton
        Left = 318
        Top = 504
        Width = 145
        Height = 33
        Caption = 'Remover estoque'
        TabOrder = 3
        OnClick = buttonDeleteClick
      end
      object buttonView: TButton
        Left = 568
        Top = 504
        Width = 145
        Height = 33
        Caption = 'Visualizar estoque'
        TabOrder = 4
        OnClick = buttonViewClick
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object editName: TEdit
        Left = 296
        Top = 200
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = 'Nome'
      end
      object buttonCancel: TButton
        Left = 200
        Top = 280
        Width = 161
        Height = 41
        Caption = 'Cancelar'
        TabOrder = 1
        OnClick = buttonCancelClick
      end
      object buttonSave: TButton
        Left = 376
        Top = 280
        Width = 161
        Height = 41
        Caption = 'Salvar'
        TabOrder = 2
        OnClick = buttonSaveClick
      end
    end
    object tabToolTypes: TTabSheet
      Caption = 'tabToolTypes'
      ImageIndex = 2
      TabVisible = False
      OnShow = tabToolTypesShow
      object gridToolTypes: TStringGrid
        Left = 11
        Top = 16
        Width = 700
        Height = 465
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridToolTypesDrawCell
        OnSelectCell = gridToolTypesSelectCell
      end
      object buttonToolTypesBack: TButton
        Left = 11
        Top = 496
        Width = 134
        Height = 41
        Caption = 'Voltar'
        TabOrder = 1
        OnClick = buttonToolTypesBackClick
      end
      object buttonTools: TButton
        Left = 577
        Top = 496
        Width = 134
        Height = 41
        Caption = 'Visualizar unidades'
        TabOrder = 2
        OnClick = buttonToolsClick
      end
    end
    object tabTools: TTabSheet
      Caption = 'tabTools'
      ImageIndex = 3
      TabVisible = False
      OnShow = tabToolsShow
      object gridTools: TStringGrid
        Left = 12
        Top = 13
        Width = 700
        Height = 468
        ColCount = 4
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridStoragesDrawCell
        OnSelectCell = gridToolsSelectCell
      end
      object buttonToolsBack: TButton
        Left = 12
        Top = 496
        Width = 134
        Height = 41
        Caption = 'Voltar'
        TabOrder = 1
        OnClick = buttonToolsBackClick
      end
      object buttonDiscard: TButton
        Left = 259
        Top = 496
        Width = 134
        Height = 41
        Caption = 'Descartar unidade'
        TabOrder = 2
        OnClick = buttonDiscardClick
      end
      object buttonStatus: TButton
        Left = 412
        Top = 497
        Width = 141
        Height = 40
        Caption = 'Alterar disponibilidade'
        TabOrder = 3
        OnClick = buttonStatusClick
      end
      object buttonMove: TButton
        Left = 571
        Top = 497
        Width = 141
        Height = 40
        Caption = 'Mover unidade'
        TabOrder = 4
        OnClick = buttonMoveClick
      end
      object panelMoveTool: TPanel
        Left = 112
        Top = 96
        Width = 489
        Height = 289
        BorderWidth = 1
        BorderStyle = bsSingle
        Caption = 'panelMoveTool'
        Color = clSnow
        ParentBackground = False
        ShowCaption = False
        TabOrder = 5
        Visible = False
        object labelMove: TLabel
          Left = 20
          Top = 11
          Width = 107
          Height = 15
          Caption = 'Destinar ao estoque:'
        end
        object buttonMoveTool: TButton
          Left = 352
          Top = 245
          Width = 113
          Height = 28
          Caption = 'Mover unidade'
          TabOrder = 0
          OnClick = buttonMoveToolClick
        end
        object buttonCancelMove: TButton
          Left = 20
          Top = 245
          Width = 122
          Height = 28
          Caption = 'Cancelar'
          TabOrder = 1
          OnClick = buttonCancelMoveClick
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
end

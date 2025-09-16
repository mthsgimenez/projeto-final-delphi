object formDBConfig: TformDBConfig
  Left = 0
  Top = 0
  Caption = 'formDBConfig'
  ClientHeight = 429
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object gridPanelDBConfig: TGridPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 409
    Height = 429
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    Caption = 'gridPanelDBConfig'
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = panelServer
        Row = 1
      end
      item
        Column = 0
        Control = panelPort
        Row = 2
      end
      item
        Column = 0
        Control = panelDatabase
        Row = 3
      end
      item
        Column = 0
        Control = panelUser
        Row = 4
      end
      item
        Column = 0
        Control = panelPassword
        Row = 5
      end
      item
        Column = 0
        Control = labelTitle
        Row = 0
      end
      item
        Column = 0
        Control = buttonSave
        Row = 6
      end>
    RowCollection = <
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714290000
      end
      item
        Value = 14.285714285714270000
      end>
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      409
      429)
    object panelServer: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 62
      Width = 407
      Height = 61
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'panelServer'
      ShowCaption = False
      TabOrder = 0
      object labelServer: TLabel
        Left = 136
        Top = 16
        Width = 114
        Height = 15
        Caption = 'Endere'#231'o do servidor:'
      end
      object editServer: TEdit
        Left = 136
        Top = 37
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = '127.0.0.1'
      end
    end
    object panelPort: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 123
      Width = 407
      Height = 61
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'panelServer'
      ShowCaption = False
      TabOrder = 1
      object labelPort: TLabel
        Left = 136
        Top = 16
        Width = 93
        Height = 15
        Caption = 'Porta do servidor:'
      end
      object editPort: TEdit
        Left = 136
        Top = 37
        Width = 121
        Height = 23
        NumbersOnly = True
        TabOrder = 0
        TextHint = '5432'
      end
    end
    object panelDatabase: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 184
      Width = 407
      Height = 61
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'panelServer'
      ShowCaption = False
      TabOrder = 2
      object labelDatabase: TLabel
        Left = 136
        Top = 16
        Width = 137
        Height = 15
        Caption = 'Nome do banco de dados'
      end
      object editDatabase: TEdit
        Left = 136
        Top = 37
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = 'mttools'
      end
    end
    object panelUser: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 245
      Width = 407
      Height = 61
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'panelServer'
      ShowCaption = False
      TabOrder = 3
      object labelUser: TLabel
        Left = 136
        Top = 16
        Width = 43
        Height = 15
        Caption = 'Usu'#225'rio:'
      end
      object editUser: TEdit
        Left = 136
        Top = 37
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = 'postgres'
      end
    end
    object panelPassword: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 306
      Width = 407
      Height = 61
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      Caption = 'panelServer'
      ShowCaption = False
      TabOrder = 4
      object labelPassword: TLabel
        Left = 136
        Top = 16
        Width = 35
        Height = 15
        Caption = 'Senha:'
      end
      object editPassword: TEdit
        Left = 136
        Top = 37
        Width = 121
        Height = 23
        PasswordChar = '*'
        TabOrder = 0
        TextHint = '****'
      end
    end
    object labelTitle: TLabel
      Left = 116
      Top = 24
      Width = 176
      Height = 15
      Anchors = []
      Caption = 'Configura'#231#227'o do banco de dados'
      ExplicitLeft = 119
      ExplicitTop = 29
    end
    object buttonSave: TButton
      Left = 167
      Top = 385
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Salvar'
      TabOrder = 5
      OnClick = buttonSaveClick
    end
  end
end

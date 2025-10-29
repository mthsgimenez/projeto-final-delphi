object formUser: TformUser
  Left = 0
  Top = 0
  Caption = 'formUser'
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
  object pcontrolUser: TPageControl
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
      object gridUsers: TStringGrid
        Left = 24
        Top = 48
        Width = 665
        Height = 345
        ColCount = 3
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridUsersDrawCell
        OnSelectCell = gridUsersSelectCell
      end
      object buttonCreate: TButton
        Left = 24
        Top = 444
        Width = 145
        Height = 45
        Caption = 'Cadastrar usu'#225'rio'
        TabOrder = 1
        Visible = False
        OnClick = buttonCreateClick
      end
      object buttonDelete: TButton
        Left = 536
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Remover usu'#225'rio'
        TabOrder = 2
        Visible = False
        OnClick = buttonDeleteClick
      end
      object buttonEdit: TButton
        Left = 272
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Editar dados'
        TabOrder = 3
        Visible = False
        OnClick = buttonEditClick
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object labelName: TLabel
        Left = 272
        Top = 171
        Width = 95
        Height = 15
        Caption = 'Nome do usu'#225'rio:'
      end
      object labelLogin: TLabel
        Left = 272
        Top = 235
        Width = 92
        Height = 15
        Caption = 'Login do usu'#225'rio:'
      end
      object labelPassword: TLabel
        Left = 272
        Top = 299
        Width = 35
        Height = 15
        Caption = 'Senha:'
      end
      object editName: TEdit
        Left = 272
        Top = 192
        Width = 145
        Height = 23
        TabOrder = 0
        TextHint = 'Name'
      end
      object editLogin: TEdit
        Left = 272
        Top = 256
        Width = 145
        Height = 23
        TabOrder = 1
        TextHint = 'Login'
      end
      object editPassword: TEdit
        Left = 272
        Top = 320
        Width = 145
        Height = 23
        PasswordChar = '*'
        TabOrder = 2
        TextHint = '********'
      end
      object buttonSave: TButton
        Left = 360
        Top = 384
        Width = 145
        Height = 33
        Caption = 'Salvar'
        TabOrder = 3
        OnClick = buttonSaveClick
      end
      object buttonCancel: TButton
        Left = 184
        Top = 384
        Width = 145
        Height = 33
        Caption = 'Cancelar'
        TabOrder = 4
        OnClick = buttonCancelClick
      end
    end
  end
end

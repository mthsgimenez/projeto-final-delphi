object formPermissions: TformPermissions
  Left = 0
  Top = 0
  Caption = 'formPermissions'
  ClientHeight = 561
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pcontrolPermissions: TPageControl
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
    ExplicitLeft = 88
    ExplicitTop = 8
    ExplicitWidth = 289
    ExplicitHeight = 193
    object tabList: TTabSheet
      Caption = 'tabList'
      TabVisible = False
      OnShow = tabListShow
      object listGroups: TListBox
        Left = 20
        Top = 20
        Width = 333
        Height = 445
        ItemHeight = 15
        TabOrder = 0
        OnClick = listGroupsClick
      end
      object listPermissionsView: TCheckListBox
        Left = 373
        Top = 20
        Width = 333
        Height = 445
        Enabled = False
        ItemHeight = 17
        Items.Strings = (
          'USERS_CREATE'
          'USERS_UPDATE'
          'USERS_DELETE'
          'USERS_PERMISSIONS')
        TabOrder = 1
      end
      object buttonCreate: TButton
        Left = 20
        Top = 491
        Width = 156
        Height = 41
        Caption = 'Adicionar grupo'
        TabOrder = 2
        OnClick = buttonCreateClick
      end
      object buttonEdit: TButton
        Left = 197
        Top = 491
        Width = 156
        Height = 41
        Caption = 'Editar grupo'
        TabOrder = 3
        OnClick = buttonEditClick
      end
      object buttonDelete: TButton
        Left = 373
        Top = 491
        Width = 156
        Height = 41
        Caption = 'Remover grupo'
        TabOrder = 4
        OnClick = buttonDeleteClick
      end
      object buttonUsers: TButton
        Left = 550
        Top = 491
        Width = 156
        Height = 41
        Caption = 'Atribuir/Remover usu'#225'rios'
        TabOrder = 5
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object labelName: TLabel
        Left = 192
        Top = 144
        Width = 88
        Height = 15
        Caption = 'Nome do grupo:'
      end
      object labelPermissions: TLabel
        Left = 192
        Top = 211
        Width = 114
        Height = 15
        Caption = 'Permiss'#245'es do grupo:'
      end
      object editName: TEdit
        Left = 192
        Top = 165
        Width = 121
        Height = 23
        TabOrder = 0
        TextHint = 'Almoxarifado'
      end
      object listPermissions: TCheckListBox
        Left = 192
        Top = 232
        Width = 305
        Height = 89
        ItemHeight = 17
        Items.Strings = (
          'USERS_CREATE'
          'USERS_UPDATE'
          'USERS_DELETE'
          'USERS_PERMISSIONS')
        TabOrder = 1
      end
      object buttonSave: TButton
        Left = 256
        Top = 344
        Width = 185
        Height = 41
        Caption = 'Salvar'
        TabOrder = 2
        OnClick = buttonSaveClick
      end
    end
  end
end

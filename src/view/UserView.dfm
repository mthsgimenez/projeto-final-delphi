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
        TabOrder = 0
      end
      object buttonCreate: TButton
        Left = 24
        Top = 444
        Width = 145
        Height = 45
        Caption = 'Cadastrar usu'#225'rio'
        TabOrder = 1
        OnClick = buttonCreateClick
      end
      object buttonDelete: TButton
        Left = 536
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Remover usu'#225'rio'
        TabOrder = 2
        OnClick = buttonDeleteClick
      end
      object buttonEdit: TButton
        Left = 192
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Editar dados'
        TabOrder = 3
        OnClick = buttonEditClick
      end
      object buttonPermissions: TButton
        Left = 361
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Permiss'#245'es'
        TabOrder = 4
        OnClick = buttonPermissionsClick
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
      TabVisible = False
      object editName: TEdit
        Left = 272
        Top = 152
        Width = 145
        Height = 23
        TabOrder = 0
        TextHint = 'Name'
      end
      object editLogin: TEdit
        Left = 272
        Top = 208
        Width = 145
        Height = 23
        TabOrder = 1
        TextHint = 'Login'
      end
      object editPassword: TEdit
        Left = 272
        Top = 272
        Width = 145
        Height = 23
        PasswordChar = '*'
        TabOrder = 2
        TextHint = '********'
      end
      object buttonSave: TButton
        Left = 360
        Top = 336
        Width = 145
        Height = 33
        Caption = 'Salvar'
        TabOrder = 3
        OnClick = buttonSaveClick
      end
      object buttonCancel: TButton
        Left = 184
        Top = 336
        Width = 145
        Height = 33
        Caption = 'Cancelar'
        TabOrder = 4
        OnClick = buttonCancelClick
      end
    end
    object tabPermissions: TTabSheet
      Caption = 'tabPermissions'
      ImageIndex = 2
      TabVisible = False
      object buttonBack: TButton
        Left = 24
        Top = 488
        Width = 129
        Height = 41
        Caption = 'Voltar'
        TabOrder = 0
        OnClick = buttonBackClick
      end
      object ListView1: TListView
        Left = 280
        Top = 232
        Width = 185
        Height = 73
        Checkboxes = True
        Columns = <>
        Items.ItemData = {
          05A00000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
          000E750073006500720073005F00720065006700690073007400650072000000
          0000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000000A750073006500720073
          005F00650064006900740000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
          00000011750073006500720073005F007000650072006D006900730073006900
          6F006E007300}
        TabOrder = 1
        ViewStyle = vsSmallIcon
      end
      object buttonSavePermissions: TButton
        Left = 272
        Top = 336
        Width = 193
        Height = 33
        Caption = 'Salvar'
        TabOrder = 2
      end
    end
  end
end

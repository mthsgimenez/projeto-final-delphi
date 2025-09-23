object formUser: TformUser
  Left = 0
  Top = 0
  Caption = 'formUser'
  ClientHeight = 561
  ClientWidth = 784
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
    Width = 784
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
      OnShow = tabListShow
      object gridUsers: TStringGrid
        Left = 48
        Top = 48
        Width = 665
        Height = 345
        TabOrder = 0
      end
      object buttonCreate: TButton
        Left = 48
        Top = 444
        Width = 145
        Height = 45
        Caption = 'Cadastrar usu'#225'rio'
        TabOrder = 1
        OnClick = buttonCreateClick
      end
      object buttonDelete: TButton
        Left = 560
        Top = 442
        Width = 153
        Height = 49
        Caption = 'Remover usu'#225'rio'
        TabOrder = 2
      end
    end
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      ImageIndex = 1
    end
  end
end

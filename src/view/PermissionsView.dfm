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
  OnShow = FormShow
  TextHeight = 15
  object panelContainer: TPanel
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
    BevelOuter = bvNone
    Caption = 'panelContainer'
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 24
    ExplicitTop = -5
    object listGroups: TListBox
      Left = 24
      Top = 32
      Width = 329
      Height = 249
      ItemHeight = 15
      TabOrder = 0
      OnClick = listGroupsClick
    end
    object listPermissions: TCheckListBox
      Left = 392
      Top = 32
      Width = 313
      Height = 249
      Enabled = False
      ItemHeight = 17
      Items.Strings = (
        'USERS_CREATE'
        'USERS_UPDATE'
        'USERS_DELETE'
        'USERS_PERMISSIONS')
      TabOrder = 1
    end
  end
end

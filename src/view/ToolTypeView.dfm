object formToolType: TformToolType
  Left = 0
  Top = 0
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
  object pcontrolToolType: TPageControl
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
    object tabCreate: TTabSheet
      Caption = 'tabCreate'
      TabVisible = False
      OnHide = tabCreateHide
      object labelSupplier: TLabel
        Left = 227
        Top = 222
        Width = 63
        Height = 15
        Caption = 'Fornecedor:'
      end
      object labelUsage: TLabel
        Left = 195
        Top = 260
        Width = 95
        Height = 15
        Caption = 'Tipo de consumo:'
      end
      object labelFamily: TLabel
        Left = 249
        Top = 149
        Width = 41
        Height = 15
        Caption = 'Fam'#237'lia:'
      end
      object labelPrice: TLabel
        Left = 257
        Top = 186
        Width = 33
        Height = 15
        Caption = 'Pre'#231'o:'
      end
      object labelImage: TLabel
        Left = 243
        Top = 296
        Width = 47
        Height = 15
        Caption = 'Imagem:'
      end
      object labelDescription: TLabel
        Left = 236
        Top = 111
        Width = 54
        Height = 15
        Caption = 'Descri'#231#227'o:'
      end
      object labelCode: TLabel
        Left = 248
        Top = 74
        Width = 42
        Height = 15
        Caption = 'C'#243'digo:'
      end
      object editCode: TEdit
        Left = 296
        Top = 71
        Width = 145
        Height = 23
        MaxLength = 20
        TabOrder = 0
        TextHint = 'C'#243'digo'
      end
      object editDescription: TEdit
        Left = 296
        Top = 108
        Width = 145
        Height = 23
        MaxLength = 100
        TabOrder = 1
        TextHint = 'Descri'#231#227'o'
      end
      object comboSuppliers: TComboBox
        Left = 296
        Top = 219
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 2
      end
      object buttonSelectImage: TButton
        Left = 456
        Top = 293
        Width = 75
        Height = 23
        Caption = 'Selecionar'
        TabOrder = 3
        OnClick = buttonSelectImageClick
      end
      object editImage: TEdit
        Left = 296
        Top = 293
        Width = 145
        Height = 23
        TabOrder = 4
        TextHint = 'Imagem'
      end
      object editPrice: TEdit
        Left = 296
        Top = 183
        Width = 145
        Height = 23
        TabOrder = 5
        TextHint = 'R$0,00'
        OnExit = editPriceExit
      end
      object comboUsage: TComboBox
        Left = 296
        Top = 257
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 6
        Items.Strings = (
          'Consum'#237'vel'
          'Afi'#225'vel')
      end
      object buttonSave: TButton
        Left = 386
        Top = 360
        Width = 145
        Height = 41
        Caption = 'Salvar'
        TabOrder = 7
        OnClick = buttonSaveClick
      end
      object buttonCancel: TButton
        Left = 209
        Top = 360
        Width = 145
        Height = 41
        Caption = 'Cancelar'
        TabOrder = 8
        OnClick = buttonCancelClick
      end
      object comboFamily: TComboBox
        Left = 296
        Top = 146
        Width = 145
        Height = 23
        Style = csDropDownList
        TabOrder = 9
        Items.Strings = (
          'Brocas'
          'Cabecotes'
          'Fresas'
          'Insertos')
      end
    end
    object tabList: TTabSheet
      Caption = 'tabList'
      ImageIndex = 1
      TabVisible = False
      object imageBrokenIcon: TImage
        Left = 600
        Top = 443
        Width = 105
        Height = 105
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2232347078222066696C6C3D2223363636363636223E3C706174682064
          3D224D3230302D313230712D333320302D35362E352D32332E35543132302D32
          3030762D35363071302D33332032332E352D35362E35543230302D3834306835
          363071333320302035362E352032332E35543834302D37363076353630713020
          33332D32332E352035362E35543736302D313230483230305A6D34302D333337
          203136302D3136302031363020313630203136302D313630203430203430762D
          31383348323030763236336C34302034305A6D2D34302032353768353630762D
          3236346C2D34302D34302D313630203136302D3136302D3136302D3136302031
          36302D34302D3430763138345A6D302030762D3236342038302D333736203536
          305A222F3E3C2F7376673E}
        Stretch = True
      end
      object labelTitle: TLabel
        Left = 12
        Top = 7
        Width = 145
        Height = 37
        Caption = 'Ferramentas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -27
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object imageDeleteIcon: TImage
        Left = 472
        Top = 352
        Width = 105
        Height = 105
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2232347078222066696C6C3D2223363636363636223E3C706174682064
          3D224D3238302D313230712D333320302D35362E352D32332E35543230302D32
          3030762D353230682D3430762D383068323030762D3430683234307634306832
          3030763830682D34307635323071302033332D32332E352035362E3554363830
          2D313230483238305A6D3430302D363030483238307635323068343030762D35
          32305A4D3336302D323830683830762D333630682D3830763336305A6D313630
          2030683830762D333630682D3830763336305A4D3238302D373230763532302D
          3532305A222F3E3C2F7376673E}
      end
      object imageEditIcon: TImage
        Left = 392
        Top = 448
        Width = 105
        Height = 105
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2232347078222066696C6C3D2223363636363636223E3C706174682064
          3D224D3230302D3230306835376C3339312D3339312D35372D35372D33393120
          3339317635375A6D2D3830203830762D3137306C3532382D3532377131322D31
          312032362E352D31377433302E352D3671313620302033312036743236203138
          6C35352035367131322031312031372E3520323674352E352033307130203136
          2D352E352033302E35543831372D3634374C3239302D313230483132305A6D36
          34302D3538342D35362D35362035362035365A6D2D3134312038352D32382D32
          392035372035372D32392D32385A222F3E3C2F7376673E}
      end
      object gridToolTypes: TStringGrid
        AlignWithMargins = True
        Left = 0
        Top = 60
        Width = 726
        Height = 481
        Margins.Left = 0
        Margins.Top = 60
        Margins.Right = 0
        Margins.Bottom = 10
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        DefaultDrawing = False
        FixedCols = 0
        TabOrder = 0
        OnDrawCell = gridToolTypesDrawCell
        OnMouseDown = gridToolTypesMouseDown
      end
      object panelButtonCreate: TPanel
        Left = 538
        Top = 8
        Width = 185
        Height = 41
        BevelOuter = bvNone
        Caption = 'panelButtonCreate'
        ShowCaption = False
        TabOrder = 1
        object shapeButtonCreate: TShape
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 185
          Height = 41
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Brush.Color = clRoyalblue
          Pen.Style = psClear
          Shape = stRoundRect
          ExplicitLeft = 8
        end
        object buttonCreate: TSpeedButton
          Left = 0
          Top = 0
          Width = 185
          Height = 41
          Align = alClient
          Caption = 'Adicionar'
          Flat = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -16
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          OnClick = buttonCreateClick
          ExplicitLeft = 56
          ExplicitTop = 16
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
      end
    end
  end
  object opdialogImage: TOpenDialog
    Filter = 'Images|*.bmp;*.jpg;*.jpeg;*.png;'
    Left = 612
    Top = 270
  end
end

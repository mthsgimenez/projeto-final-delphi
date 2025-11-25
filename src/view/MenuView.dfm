object formMenu: TformMenu
  Left = 0
  Top = 0
  Caption = 'formMenu'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object panelContainer: TPanel
    AlignWithMargins = True
    Left = 50
    Top = 0
    Width = 734
    Height = 561
    Margins.Left = 50
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Caption = 'panelContainer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 0
  end
  object panelMenu: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 250
    Height = 561
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Caption = 'panelMenu'
    Color = clRoyalblue
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    object imgMenu: TImage
      AlignWithMargins = True
      Left = 7
      Top = 7
      Width = 243
      Height = 36
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 0
      Margins.Bottom = 10
      Align = alTop
      Anchors = [akTop]
      Picture.Data = {
        0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
        2F7777772E77332E6F72672F323030302F73766722206865696768743D223430
        7078222076696577426F783D2230202D39363020393630203936302220776964
        74683D2234307078222066696C6C3D2223464646464646223E3C706174682064
        3D224D3132302D323430762D36362E363768373230562D323430483132305A6D
        302D3230362E3637762D36362E3636683732307636362E3636483132305A6D30
        2D3230362E3636562D373230683732307636362E3637483132305A222F3E3C2F
        7376673E}
      Proportional = True
      Stretch = True
      OnClick = imgMenuClick
      ExplicitWidth = 36
    end
    object shapeSeparator: TShape
      Left = 0
      Top = 50
      Width = 250
      Height = 3
      Pen.Style = psClear
    end
    object labelUsername: TLabel
      Left = 60
      Top = 518
      Width = 7
      Height = 32
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSnow
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object imgLogout: TImage
      Left = 7
      Top = 518
      Width = 36
      Height = 36
      Center = True
      Picture.Data = {
        0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
        2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
        7078222076696577426F783D2230202D39363020393630203936302220776964
        74683D2232347078222066696C6C3D2223464646464646223E3C706174682064
        3D224D3230302D313230712D333320302D35362E352D32332E35543132302D32
        3030762D35363071302D33332032332E352D35362E35543230302D3834306832
        3830763830483230307635363068323830763830483230305A6D3434302D3136
        302D35352D3538203130322D31303248333630762D3830683332374C3538352D
        3632326C35352D353820323030203230302D323030203230305A222F3E3C2F73
        76673E}
      Stretch = True
      OnClick = imgLogoutClick
    end
    object panelUser: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 57
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelUser'
      ShowCaption = False
      TabOrder = 0
      object imgUser: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Center = True
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223430
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2234307078222066696C6C3D2223464646464646223E3C706174682064
          3D224D3438302D3438302E3637712D363620302D3130392E36372D34332E3636
          513332362E36372D353638203332362E36372D3633347434332E36362D313039
          2E3637513431342D3738372E3333203438302D3738372E3333743130392E3637
          2034332E3636513633332E33332D373030203633332E33332D363334742D3433
          2E3636203130392E3637513534362D3438302E3637203438302D3438302E3637
          5A4D3136302D313630762D31303071302D33362E36372031382E352D36342E31
          37543232362E36372D3336367136352E33332D33302E3333203132372E36362D
          34352E352036322E33342D31352E3137203132352E36372D31352E3137743132
          352E33332031352E357136322031352E35203132372E32382034352E33203330
          2E35342031342E34322034382E39362034312E3831513830302D3239362E3637
          203830302D32363076313030483136305A6D36362E36372D36362E3637683530
          362E3636562D32363071302D31342E33332D382E31362D32372D382E31372D31
          322E36372D32302E352D31392D36302E36372D32392E36372D3131342E33342D
          34312E3833513533362E36372D333630203438302D333630742D313131203132
          2E3137513331342E36372D3333352E3637203235342E36372D333036712D3132
          2E333420362E33332D32302E31372031392D372E38332031322E36372D372E38
          332032377633332E33335A4D3438302D3534372E333371333720302036312E38
          332D32342E3834513536362E36372D353937203536362E36372D363334742D32
          342E38342D36312E3833513531372D3732302E3637203438302D3732302E3637
          742D36312E38332032342E3834513339332E33332D363731203339332E33332D
          3633347432342E38342036312E3833513434332D3534372E3333203438302D35
          34372E33335A6D302D38362E36375A6D30203430372E33335A222F3E3C2F7376
          673E}
        Proportional = True
        Stretch = True
      end
      object labelUser: TLabel
        Left = 60
        Top = 7
        Width = 90
        Height = 32
        Caption = 'Usu'#225'rios'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object buttonUserMenu: TSpeedButton
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Flat = True
        OnClick = buttonUserMenuClick
        ExplicitTop = 1
      end
    end
    object panelPermissions: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 111
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelUser'
      ShowCaption = False
      TabOrder = 1
      object imgPermissions: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Center = True
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223430
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2234307078222066696C6C3D2223464646464646223E3C706174682064
          3D224D3530392D3232362E36375A4D3533322E33332D31363048313630762D31
          303071302D33362E36372031382E352D36342E3137543232362E36372D333636
          7136352E33332D33302E3333203132372E36362D34352E352036322E33342D31
          352E3137203132352E36372D31352E31376831302E363771352E333320302031
          302E36362E36377636362E3637712D352E333320302D31302E36362D2E33342D
          352E33342D2E33332D31302E36372D2E33332D35362E363720302D3131302E36
          372031322E35543235342E36372D333036712D31322E333420362E33332D3230
          2E31372031392D372E38332031322E36372D372E38332032377633332E333348
          35303971332E36392031372E353620392E36382033342E323820352E39392031
          362E37322031332E36352033322E33395A4D3732342D3830712D37312E36372D
          31382D3131332E38332D37382E3736513536382D3231392E3532203536382D32
          3934762D39392E33386C3135362D37372E3935203135362037372E3935562D32
          393471302037342E34382D34322E3137203133352E3234513739352E36372D39
          38203732342D38305A6D302D36387134322D32302036352E36372D35392E3637
          2032332E36362D33392E36362032332E36362D38362E3437762D35372E39354C
          3732342D3339362E36376C2D38392E33332034342E35387635372E3935713020
          34362E38312032332E36362038362E3437513638322D313638203732342D3134
          385A4D3438302D3438302E3637712D363420302D3130382E36372D34342E3636
          513332362E36372D353730203332362E36372D3633347434342E36362D313038
          2E3637513431362D3738372E3333203438302D3738372E3333743130382E3637
          2034342E3636513633332E33332D363938203633332E33332D363334742D3434
          2E3636203130382E3637513534342D3438302E3637203438302D3438302E3637
          5A6D302D36362E36367133362E333320302036312E352D32352E31377432352E
          31372D36312E3571302D33362E33332D32352E31372D36312E35543438302D37
          32302E3637712D33362E333320302D36312E352032352E3137543339332E3333
          2D36333471302033362E33332032352E31372036312E357436312E352032352E
          31375A6D302D38362E36375A6D323434203336312E36375A222F3E3C2F737667
          3E}
        Proportional = True
        Stretch = True
      end
      object labelPermissions: TLabel
        Left = 60
        Top = 7
        Width = 117
        Height = 32
        Caption = 'Permiss'#245'es'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object buttonPermissions: TSpeedButton
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Flat = True
        OnClick = buttonPermissionsClick
        ExplicitTop = -4
      end
    end
    object panelSuppliers: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 165
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelUser'
      ShowCaption = False
      TabOrder = 2
      object labelSuppliers: TLabel
        Left = 60
        Top = 7
        Width = 144
        Height = 32
        Caption = 'Fornecedores'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object imgSuppliers: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Center = True
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2232347078222066696C6C3D2223464646464646223E3C706174682064
          3D224D3234302D313630712D353020302D38352D3335742D33352D3835483430
          762D34343071302D33332032332E352D35362E35543132302D38303068353630
          76313630683132306C3132302031363076323030682D383071302035302D3335
          203835742D3835203335712D353020302D38352D3335742D33352D3835483336
          3071302035302D3335203835742D38352033355A6D302D383071313720302032
          382E352D31312E35543238302D32383071302D31372D31312E352D32382E3554
          3234302D333230712D313720302D32382E352031312E35543230302D32383071
          302031372031312E352032382E35543234302D3234305A4D3132302D33363068
          33327131372D31382033392D32397434392D3131713237203020343920313174
          333920323968323732762D33363048313230763336305A6D3630302031323071
          313720302032382E352D31312E35543736302D32383071302D31372D31312E35
          2D32382E35543732302D333230712D313720302D32382E352031312E35543638
          302D32383071302031372031312E352032382E35543732302D3234305A6D2D34
          302D323030683137306C2D39302D313230682D3830763132305A4D3336302D35
          34305A222F3E3C2F7376673E}
        Stretch = True
      end
      object buttonSuppliers: TSpeedButton
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Align = alClient
        Flat = True
        OnClick = buttonSuppliersClick
        ExplicitLeft = 208
        ExplicitTop = 16
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
    end
    object panelToolTypes: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 219
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelUser'
      ShowCaption = False
      TabOrder = 3
      object labelToolTypes: TLabel
        Left = 60
        Top = 7
        Width = 131
        Height = 32
        Caption = 'Ferramentas'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object imageToolTypes: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Center = True
        Picture.Data = {
          0D54536B537667477261706869633C73766720786D6C6E733D22687474703A2F
          2F7777772E77332E6F72672F323030302F73766722206865696768743D223234
          7078222076696577426F783D2230202D39363020393630203936302220776964
          74683D2232347078222066696C6C3D2223464646464646223E3C706174682064
          3D226D3233342D3438302D31322D3630712D31322D352D32322E352D31302E35
          543137382D3536346C2D35382031382D34302D36382034362D3430712D322D31
          332D322D323674322D32366C2D34362D34302034302D36382035382031387131
          312D382032312E352D31332E35543232322D3832306C31322D36306838306C31
          3220363071313220352032322E352031302E35543337302D3739366C35382D31
          382034302036382D343620343071322031332032203236742D322032366C3436
          2034302D34302036382D35382D3138712D313120382D32312E352031332E3554
          3332362D3534306C2D3132203630682D38305A6D34302D313230713333203020
          35362E352D32332E35543335342D36383071302D33332D32332E352D35362E35
          543237342D373630712D333320302D35362E352032332E35543139342D363830
          71302033332032332E352035362E35543237342D3630305A4D3539322D34306C
          2D31382D3834712D31372D362D33312E352D31342E35543531342D3135386C2D
          38302032362D35362D39362036342D3536712D322D31382D322D333674322D33
          366C2D36342D35362035362D39362038302032367131342D31312032382E352D
          31392E35543537342D3531366C31382D3834683131326C313820383471313720
          362033312E352031342E35543738322D3438326C38302D32362035362039362D
          363420353671322031382032203336742D322033366C36342035362D35362039
          362D38302D3236712D31342031312D32382E352031392E35543732322D313234
          6C2D3138203834483539325A6D35362D31363071353020302038352D33357433
          352D383571302D35302D33352D3835742D38352D3335712D353020302D383520
          3335742D333520383571302035302033352038357438352033355A222F3E3C2F
          7376673E}
        Stretch = True
      end
      object buttonToolTypes: TSpeedButton
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Align = alClient
        Flat = True
        OnClick = buttonToolTypesClick
        ExplicitLeft = 192
        ExplicitTop = 24
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
    end
    object panelStorage: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 273
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelStorage'
      ShowCaption = False
      TabOrder = 4
      object labelStorage: TLabel
        Left = 60
        Top = 7
        Width = 95
        Height = 32
        Caption = 'Estoques'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object imageStorage: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000704944415478DA6364A03160A49B05FFFFFF
          B70052C7A964AE052323E349740BFE53D5E54080D5029804B900DD9C616801B5
          00360BD481D40D2A99AF0E34FF168A05D408266CFA478805B812002E7583CF82
          A11F07A316D0DF024A013E0B2C81D4310ACDB7049A7F02AB05B40034B700003F
          AD93197AE2C70F0000000049454E44AE426082}
        Stretch = True
      end
      object buttonStorage: TSpeedButton
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Align = alClient
        Flat = True
        OnClick = buttonStorageClick
        ExplicitLeft = 176
        ExplicitTop = 24
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
    end
    object panelOrders: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 327
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelStorage'
      ShowCaption = False
      TabOrder = 5
      object labelOrders: TLabel
        Left = 60
        Top = 7
        Width = 83
        Height = 32
        Caption = 'Pedidos'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object buttonOrders: TSpeedButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Flat = True
        OnClick = buttonOrdersClick
        ExplicitLeft = 184
        ExplicitTop = 24
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
      object imgOrders: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000240000
          00240806000000E1009898000001144944415478DAEDD7AD0AC25018C6F19DE2
          4D584C063FA2414C82419CC1A0D562F51E9CD760B5990441043FB0190C621541
          3098BC00AF60FE6186E350A670B633F1BC7078E01963BF6D6C3A61C56C846E80
          012903B9AE5B214A211D772784587F0C02E3467235984010961AB18802C4D898
          9641A01EE1BC3B031523DD018743F40D283410DB9254575F97211201860BFBDD
          9482E8B796F71A3852E7A4FE42A402400DF699A9060D890E6B42DDD20E523906
          F49F20FA29D1606DA8CB52AFED295B1155CBFBA52E6A07A91C03FA4F10FD80E8
          B246D46DA9D7F694ED8902EB4C9DD60E7A6CCB531D7C9D9EBF1F2AC78054836C
          621E06E4C5D4013D7D72C5FF43514235896C48961396F14B641457E29B31A09F
          03DD01177C3034028130CF0000000049454E44AE426082}
      end
    end
    object panelReports: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 435
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelStorage'
      ShowCaption = False
      TabOrder = 6
      object labelReports: TLabel
        Left = 60
        Top = 7
        Width = 104
        Height = 32
        Caption = 'Relat'#243'rios'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object buttonReports: TSpeedButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Flat = True
        OnClick = buttonReportsClick
        ExplicitLeft = 31
        ExplicitTop = 24
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
      object imgReports: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000240000
          00240806000000E10098980000017B4944415478DAEDD74F4A03311887E10657
          5E402D5AB550FF2D148FE0195CB854DCB9EE05D40B74EB562FE0195CB8120475
          214511475B503C812BC777204A089999A493B415F341F88D992FC94318908ADA
          9895183520822A83D2347D2366029EF9218498B6020D01F353EFA0EA36A0543E
          5EC8E1BBB6E4A801327E2E79A023FA8F7D6BD8FE30DB3B82FE052878FD79D08D
          1C2141FB2EA020DF9015348222C8B138B2CB59AB6301E2B88458607C72DEE448
          410A26AB84F39A5E412CDD24AE593761D1FB42CCEB186F20966D10B7F2CFAF22
          14BDAF44C384F10262C93A71A74D1B51F4F688B93C4C6510EDD9E63D65EA9CB1
          FDBBB9F2FF4AC3F479D530ED393088D635E25E996AB22661FE84E703F5A698EB
          F33C5B74339540B4AD105D1DA3BC57516A15620602D1B2443CE4610A50A51867
          10AF5BC46319C680B2C238810C1F708B9EA7B20358D7A1AF6D8331817689331D
          C4FC22F16C7B3355CAF4537A8A58E6C04B656E8F3875B9196FA0BC02B5435C85
          BA1967D0B02A82CAEA1B7D961534699601660000000049454E44AE426082}
      end
    end
    object panelReceipt: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 381
      Width = 250
      Height = 50
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Caption = 'panelStorage'
      ShowCaption = False
      TabOrder = 7
      object labelReceipt: TLabel
        Left = 60
        Top = 7
        Width = 140
        Height = 32
        Caption = 'Recebimento'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSnow
        Font.Height = -24
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object buttonReceipt: TSpeedButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 250
        Height = 50
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        Flat = True
        OnClick = buttonReceiptClick
        ExplicitLeft = 31
        ExplicitTop = 24
        ExplicitWidth = 23
        ExplicitHeight = 22
      end
      object imgReceipt: TImage
        Left = 7
        Top = 7
        Width = 36
        Height = 36
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000240000
          00240806000000E1009898000001AA4944415478DA636418648071A01D30BC1C
          F4FFFFFF1820B5988AF6BF6264641427CB4140C70800A9F73408941740474992
          E3A01220D50DE51610A96D0294DE01C5C8C0038A19800E82BB831407D503A906
          740308E8F90F653600B5341263DEA883461D34EAA051078D3A68483908A8880F
          487D24D691D402F81CF49F74E328062A40F7DC25E4A00E20EE4492426E760812
          61093710DF00621E3C6AF4810EB984115A381C8412E7C8214742FAB103520709
          2873051AB7875E0EFA03A49891849A80F82294D6461297021AF99C660E022A15
          02526F9184505A84503520B3EBB09949918390B32E0ED007545E8C66C72E5054
          21098903D5BCA287836C804A8FA299FF14144550EE6F20E602AAF983AC865207
          7100290E2C8EF9826C11501D1390FAC580485318BD0DAA38881800D42A02A45E
          23099D001A61894B3D4D1D04D46606A44E22094D066ACFC3A78796D97E1A90CA
          44124A076A9D45481FC90E22136800CDBB498C425C0E42EFD84D20C6302C0054
          518BA0E724521C0432800F8F7A5055709E8099A0EC7C16E88895E4F800233D00
          1DF515487161517B11688901992145BE83061A8C3A88100000C37964342727A0
          DE0000000049454E44AE426082}
      end
    end
  end
end

object Connection: TConnection
  Height = 480
  Width = 640
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=postgres'
      'DriverID=PG')
    Left = 264
    Top = 176
  end
  object FDPhysPgDriverLink: TFDPhysPgDriverLink
    VendorLib = '..\..\lib\libpq.dll'
    Left = 384
    Top = 344
  end
end

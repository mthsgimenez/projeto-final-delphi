object Connection: TConnection
  Height = 480
  Width = 640
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=mttools'
      'User_Name=postgres'
      'Password=root'
      'DriverID=PG')
    Left = 264
    Top = 176
  end
  object FDPhysPgDriverLink: TFDPhysPgDriverLink
    VendorLib = 
      'C:\Users\Matheus de Camargo\Desktop\projeto-final-delphi\lib\lib' +
      'pq.dll'
    Left = 384
    Top = 344
  end
end

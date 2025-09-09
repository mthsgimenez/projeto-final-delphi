unit DBConfigRepositoryInterface;

interface
  uses DBConfigModel, DBConfigDTO;

  type IDBConfigRepository = interface
    function ReadFromFile: TDBConfigModel;
    procedure SaveToFile(aDBConfigModel: TDBConfigModel);
  end;
implementation

end.

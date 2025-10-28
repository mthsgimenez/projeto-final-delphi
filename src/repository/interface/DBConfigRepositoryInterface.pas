unit DBConfigRepositoryInterface;

interface
  uses DBConfigModel, DBConfigDTO;

  type IDBConfigRepository = interface
    function Get: TDBConfigModel;
    procedure Save(aDBConfigModel: TDBConfigModel);
  end;
implementation

end.

unit StorageRepository;

interface

uses StorageModel, StorageDAOInterface, DBHelper,
  CrudRepositoryInterface, System.Generics.Collections;

type TStorageRepository = class(TInterfacedObject, ICrudRepository<TStorage>)
  private
    storageDAO: IStorageDAO;
    helper: TDBHelper;
  public
    function Save(aStorage: TStorage): TStorage;
    function FindById(aStorageId: Integer): TStorage;
    function FindAll(): TObjectList<TStorage>;
    function DeleteById(aStorageId: Integer): Boolean;
    function ExistsById(aStorageId: Integer): Boolean;
    constructor Create(aStorageDAO: IStorageDAO);
    destructor Destroy; override;
end;

implementation

{ TStorageRepository }

constructor TStorageRepository.Create(aStorageDAO: IStorageDAO);
begin
  inherited Create;
  Self.storageDAO := aStorageDAO;
  Self.helper := TDBHelper.Create;
end;

function TStorageRepository.DeleteById(aStorageId: Integer): Boolean;
begin
  Result := Self.storageDAO.DeleteById(aStorageId);
end;

destructor TStorageRepository.Destroy;
begin
  Self.helper.Free;
  inherited;
end;

function TStorageRepository.ExistsById(aStorageId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('storages', 'id', aStorageId);
end;

function TStorageRepository.FindAll: TObjectList<TStorage>;
begin
  Result := Self.storageDAO.SelectAll;
end;

function TStorageRepository.FindById(aStorageId: Integer): TStorage;
begin
  Result := Self.storageDAO.SelectById(aStorageId);
end;

function TStorageRepository.Save(aStorage: TStorage): TStorage;
begin
  if aStorage.id = 0 then begin
    Result := Self.storageDAO.Insert(aStorage);
  end else begin
    Result := Self.storageDAO.Update(aStorage);
  end;
end;

end.

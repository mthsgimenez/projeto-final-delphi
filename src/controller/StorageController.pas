unit StorageController;

interface

uses StorageModel, StorageDTO, StorageRepository;

type TStorageController = class
  private
    storageRepository: TStorageRepository;
  public
    constructor Create(aStorageRepository: TStorageRepository);
end;

implementation

{ TStorageController }

constructor TStorageController.Create(aStorageRepository: TStorageRepository);
begin
  Self.storageRepository := aStorageRepository;
end;

end.

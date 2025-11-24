unit ToolController;

interface

uses System.Generics.Collections, System.SysUtils, Session, Logging,
  ToolModel, ToolRepositoryInterface, StorageRepositoryInterface, StorageModel;

type TToolController = class
  private
    toolRepository: IToolRepository;
    storageRepository: IStorageRepository;
    logger: TLogger;
  public
    function GetTools: TObjectList<TTool>;
    function GetAvailableTools: TObjectList<TTool>;
    function DiscardTool(aToolId: Integer): Boolean;
    function ToggleAvailability(aToolId: Integer): TTool;
    function MoveTool(aToolId: Integer; aTargetStorageId: Integer): TTool;
    constructor Create(aToolRepository: IToolRepository;
      aStorageRepository: IStorageRepository);
end;

implementation

{ TToolController }

constructor TToolController.Create(aToolRepository: IToolRepository;
  aStorageRepository: IStorageRepository);
begin
  Self.toolRepository := aToolRepository;
  Self.storageRepository := aStorageRepository;
  Self.logger := TLogger.GetLogger;
end;

function TToolController.DiscardTool(aToolId: Integer): Boolean;
begin
  Result := Self.toolRepository.DeleteById(aToolId);

  if Result then begin
    Self.logger.Info(Format(
      'Unidade alterada: Usuário (ID: %d) descartou a ferramenta (ID: %d)',
      [TSession.GetUser.id, aToolId]
    ));
  end;
end;

function TToolController.GetAvailableTools: TObjectList<TTool>;
begin
  Result := Self.toolRepository.FindAvailable;
end;

function TToolController.GetTools: TObjectList<TTool>;
begin
  Result := Self.toolRepository.FindAll;
end;

function TToolController.MoveTool(aToolId, aTargetStorageId: Integer): TTool;
var
  tool: TTool;
  storage: TStorage;
begin
  tool := Self.toolRepository.FindById(aToolId);
  if not Assigned(tool) then
    raise Exception.Create(Format('Ferramenta (ID: %d) não encontrada', [aToolId]));
  storage := Self.storageRepository.FindById(aTargetStorageId);
  if not Assigned(storage) then
    raise Exception.Create(Format('Estoque (ID: %d) não encontrado', [aTargetStorageId]));

  tool.storage := storage;
  Result := Self.toolRepository.Update(tool);

  tool.Free;
end;

function TToolController.ToggleAvailability(aToolId: Integer): TTool;
var
  tool: TTool;
begin
  tool := Self.toolRepository.FindById(aToolId);

  if not Assigned(tool) then
    raise Exception.Create(Format('Ferramenta (ID: %d) não encontrada', [aToolId]));

  if tool.status = HONING then
    raise Exception.Create('Não é possível alterar a disponibilidade de uma ferramenta em afiação');

  case tool.status of
    AVAILABLE: tool.status := IN_USE;
    IN_USE: tool.status := AVAILABLE;
  end;

  Result := Self.toolRepository.Update(tool);

  if Assigned(Result) then begin
    Self.logger.Info(Format(
      'Unidade alterada: Usuário (ID: %d) alterou a disponibilidade da ferramenta (ID: %d) para %s',
      [TSession.GetUser.id, aToolId, StatusToString(tool.status)]
    ));
  end;

  tool.Free;
end;

end.

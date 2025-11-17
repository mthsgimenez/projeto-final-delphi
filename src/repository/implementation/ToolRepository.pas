unit ToolRepository;

interface

uses CrudRepositoryInterface, ToolModel, DBHelper,
  ToolTypeDAOInterface, ToolDAOInterface, StorageDAOInterface,
  System.Generics.Collections;

type TToolRepository = class(TInterfacedObject, ICrudRepository<TTool>)
  private
    storageDAO: IStorageDAO;
    toolDAO: IToolDAO;
    toolTypeDAO: IToolTypeDAO;
    helper: TDBHelper;
  public
    function Save(aTool: TTool): TTool;
    function FindById(aToolId: Integer): TTool;
    function FindAll(): TObjectList<TTool>;
    function DeleteById(aToolId: Integer): Boolean;
    function ExistsById(aToolId: Integer): Boolean;

    constructor Create(aToolDAO: IToolDAO;
      aToolTypeDAO: IToolTypeDAO; aStorageDAO: IStorageDAO);
    destructor Destroy; override;
end;

implementation

{ TToolRepository }

constructor TToolRepository.Create(aToolDAO: IToolDAO;
  aToolTypeDAO: IToolTypeDAO; aStorageDAO: IStorageDAO);
begin
  Self.toolDAO := aToolDAO;
  Self.toolTypeDAO := aToolTypeDAO;
  Self.storageDAO := aStorageDAO;
  Self.helper := TDBHelper.Create;
end;

function TToolRepository.DeleteById(aToolId: Integer): Boolean;
begin
  Result := Self.toolDAO.DeleteById(aToolId);
end;

destructor TToolRepository.Destroy;
begin
  Self.helper.Free;
  Inherited;
end;

function TToolRepository.ExistsById(aToolId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('tools', 'id', aToolId);
end;

function TToolRepository.FindAll: TObjectList<TTool>;
var
  toolList: TObjectList<TTool>;
  tool: TTool;
begin
  toolList := Self.toolDAO.SelectAll;
  for tool in toolList do begin
    tool.model := Self.toolTypeDAO.SelectByToolId(tool.id);
    tool.storage := Self.storageDAO.SelectByToolId(tool.id);
  end;

  Result := toolList;
end;

function TToolRepository.FindById(aToolId: Integer): TTool;
begin
  Result := Self.toolDAO.SelectById(aToolId);
  Result.model := Self.toolTypeDAO.SelectByToolId(aToolId);
  Result.storage := Self.storageDAO.SelectByToolId(aToolId);
end;

function TToolRepository.Save(aTool: TTool): TTool;
var
  tool: TTool;
begin
  if aTool.id = 0 then begin
    tool := Self.toolDAO.Insert(aTool);
    tool.model := Self.toolTypeDAO.SelectByToolId(tool.id);
    tool.storage := Self.storageDAO.SelectByToolId(tool.id);
  end else begin
    tool := Self.toolDAO.Update(aTool);
    tool.model := Self.toolTypeDAO.SelectByToolId(tool.id);
    tool.storage := Self.storageDAO.SelectByToolId(tool.id);
  end;

  Result := tool;
end;

end.

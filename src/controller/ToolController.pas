unit ToolController;

interface

uses System.Generics.Collections, System.SysUtils,
  ToolModel, ToolRepositoryInterface;

type TToolController = class
  private
    toolRepository: IToolRepository;
  public
    function GetTools: TObjectList<TTool>;
    function GetAvailableTools: TObjectList<TTool>;
    constructor Create(aToolRepository: IToolRepository);
end;

implementation

{ TToolController }

constructor TToolController.Create(aToolRepository: IToolRepository);
begin
  Self.toolRepository := aToolRepository;
end;

function TToolController.GetAvailableTools: TObjectList<TTool>;
begin
  Result := Self.toolRepository.FindAvailable;
end;

function TToolController.GetTools: TObjectList<TTool>;
begin
  Result := Self.toolRepository.FindAll;
end;

end.

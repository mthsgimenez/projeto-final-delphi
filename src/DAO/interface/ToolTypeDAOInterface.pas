unit ToolTypeDAOInterface;

interface

uses DAOInterface, ToolTypeModel;

type IToolTypeDAO = interface(IDAO<TToolType>)
  function SelectByToolId(aToolId: Integer): TToolType;
end;

implementation

end.

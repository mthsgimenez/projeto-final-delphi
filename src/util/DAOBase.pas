unit DAOBase;

interface

uses DBConnection, FireDAC.Comp.Client, FireDAC.DApt;

type TDAOBase = class(TInterfacedObject)
  protected
    Query: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;
end;

implementation

{ TRepositoryBase }

constructor TDAOBase.Create;
var
  Connection: TFDConnection;
begin
  inherited Create;
  Connection := DBConnection.Connection.FDConnection;
  Self.Query := TFDQuery.Create(Connection);
  Self.Query.Connection := Connection;
end;

destructor TDAOBase.Destroy;
begin
  Self.Query.Free;
  inherited Destroy;
end;

end.

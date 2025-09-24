unit RepositoryBase;

interface

uses DBConnection, FireDAC.Comp.Client, FireDAC.DApt;

type TRepositoryBase = class(TInterfacedObject)
  protected
    Query: TFDQuery;
  public
    constructor Create;
    destructor Destroy; override;
end;

implementation

{ TRepositoryBase }

constructor TRepositoryBase.Create;
var
  Connection: TFDConnection;
begin
  inherited Create;
  Connection := DBConnection.Connection.FDConnection;
  Self.Query := TFDQuery.Create(Connection);
  Self.Query.Connection := Connection;
end;

destructor TRepositoryBase.Destroy;
begin
  Self.Query.Free;
  inherited Destroy;
end;

end.

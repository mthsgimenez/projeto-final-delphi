unit RepositoryBase;

interface

uses DBConnection, FireDAC.Comp.Client, FireDAC.DApt;

type TRepositoryBase = class(TInterfacedObject)
  protected
    Query: TFDQuery;
    Connection: TFDConnection;
  public
    constructor Create;
    destructor Destroy; override;
end;

implementation

{ TRepositoryBase }

constructor TRepositoryBase.Create;
begin
  inherited Create;
  Self.Connection := DBConnection.Connection.FDConnection;
  Self.Query := TFDQuery.Create(Self.Connection);
  Self.Query.Connection := Self.Connection;
end;

destructor TRepositoryBase.Destroy;
begin
  Self.Query.Free;
  inherited Destroy;
end;

end.

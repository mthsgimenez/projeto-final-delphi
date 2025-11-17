unit ToolModel;

interface

uses ToolTypeModel, StorageModel;

type TState = (NEW, HONED);

type TTool = class
  public
    id: Integer;
    model: TToolType;
    state: String;
    batch: String;
    honing_num: Integer;
    storage: TStorage;
    inUse: Boolean;

    destructor Destroy; override;
end;

implementation

{ TTool }

destructor TTool.Destroy;
begin
  Self.storage.Free;
  Self.model.Free;
  inherited;
end;

end.

unit ToolModel;

interface

uses ToolTypeModel, StorageModel, System.SysUtils;

type TState = (NEW, HONED);
type TStatus = (
  AVAILABLE = 1,
  IN_USE = 2,
  HONING = 3
);

type TTool = class
  public
    id: Integer;
    code: String;
    model: TToolType;
    state: TState;
    honingNum: Integer;
    storage: TStorage;
    status: TStatus;

    destructor Destroy; override;
end;

function StringToStatus(aString: String): TStatus;
function StatusToString(aStatus: TStatus): String;
function StatusToStringDisplay(aStatus: TStatus): String;
function StringToState(aString: String): TState;
function StateToString(aState: TState): String;
function StateToStringDisplay(aState: TState): String;

implementation

{ TTool }

destructor TTool.Destroy;
begin
  Self.storage.Free;
  Self.model.Free;
  inherited;
end;

function StringToStatus(aString: String): TStatus;
begin
  aString := UpperCase(aString);
  if aString = 'AVAILABLE' then
    Result := AVAILABLE
  else if aString = 'IN_USE' then
    Result := IN_USE
  else if aString = 'HONING' then
    Result := HONING
  else
    raise Exception.Create(Format('String de status inválida: %s', [aString]));
end;

function StatusToString(aStatus: TStatus): String;
begin
  case aStatus of
    AVAILABLE: Result := 'AVAILABLE';
    IN_USE:    Result := 'IN_USE';
    HONING:    Result := 'HONING';
  else
    raise Exception.Create(Format('Valor de status inválido: %d', [Ord(aStatus)]));
  end;
end;

function StatusToStringDisplay(aStatus: TStatus): String;
begin
  case aStatus of
    AVAILABLE: Result := 'Disponível';
    IN_USE:    Result := 'Em uso';
    HONING:    Result := 'Em afiação';
  else
    raise Exception.Create(Format('Valor de status inválido: %d', [Ord(aStatus)]));
  end;
end;

function StringToState(aString: String): TState;
begin
  aString := UpperCase(aString);
  if aString = 'NEW' then
    Result := NEW
  else if aString = 'HONED' then
    Result := HONED
  else
    raise Exception.Create(Format('String de state inválida: %s', [aString]));
end;

function StateToString(aState: TState): String;
begin
  case aState of
    NEW: Result := 'NEW';
    HONED:    Result := 'HONED';
  else
    raise Exception.Create(Format('Valor de state inválido: %d', [Ord(aState)]));
  end;
end;

function StateToStringDisplay(aState: TState): String;
begin
  case aState of
    NEW: Result := 'Nova';
    HONED:    Result := 'Afiada';
  else
    raise Exception.Create(Format('Valor de state inválido: %d', [Ord(aState)]));
  end;
end;

end.

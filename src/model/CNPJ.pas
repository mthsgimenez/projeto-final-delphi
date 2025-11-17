unit CNPJ;

interface

uses System.SysUtils;

type TCNPJ = class
  private
    CNPJ: String;
    class function CharToInt(aChar: Char): Integer;
    class function CalculateDigit(aCNPJ: String; Weights: array of Integer): Integer;
  public
    constructor Create(aCNPJ: String);
    procedure setCNPJ(aCNPJ: String);
    function getCNPJ: String;
    class function IsCnpjValid(aCNPJ: String): Boolean;
end;

implementation

{ TCNPJ }

class function TCNPJ.CalculateDigit(aCNPJ: String;
  Weights: array of Integer): Integer;
var
  sum, i, digit: Integer;
begin
  sum := 0;
  for i := 1 to Length(Weights) do begin
    digit := CharToInt(aCNPJ[i]);
    // Strings são indexadas com 1 mas arrays são com 0
    sum := sum + (digit * Weights[i - 1]);
  end;

  digit := sum mod 11;
  if (digit <> 0) and (digit <> 1) then begin
    Result := 11 - digit;
    Exit;
  end;

  Result := 0;
end;

class function TCNPJ.CharToInt(aChar: Char): Integer;
begin
  // Transforma o char em int utilizando tabela ASCII
  // '0' = 48, '1' = 49 .. '9' = 57
  Result := Ord(aChar) - Ord('0');
end;

constructor TCNPJ.Create(aCNPJ: String);
begin
  Self.setCNPJ(aCNPJ);
end;

function TCNPJ.getCNPJ: String;
begin
  Result := Self.CNPJ;
end;

class function TCNPJ.IsCnpjValid(aCNPJ: String): Boolean;
const
  Multipliers1: array[0..11] of Integer = (5,4,3,2,9,8,7,6,5,4,3,2);
  Multipliers2: array[0..12] of Integer = (6,5,4,3,2,9,8,7,6,5,4,3,2);
var
  firstDigit, secondDigit: Integer;
begin
  Result := False;

  if Length(aCNPJ) <> 14 then Exit;

  firstDigit := CalculateDigit(aCNPJ, Multipliers1);
  // Strings no delphi são indexadas com 1
  if firstDigit <> CharToInt(aCNPJ[13]) then Exit;

  secondDigit := CalculateDigit(aCNPJ, Multipliers2);
  // Strings no delphi são indexadas com 1
  if secondDigit <> CharToInt(aCNPJ[14]) then Exit;

  Result := True;
end;

procedure TCNPJ.setCNPJ(aCNPJ: String);
begin
//  if not TCNPJ.IsCnpjValid(aCNPJ) then raise Exception.Create('CNPJ inválido');

  Self.CNPJ := aCNPJ;
end;

end.

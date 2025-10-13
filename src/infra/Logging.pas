unit Logging;

interface

uses System.IOUtils, System.SysUtils, System.Classes, System.TypInfo;

type TLogLevels = (DEBUG, INFO, WARNING, ERROR);

type TLogger = class
  private
    level: TLogLevels;
    logPath: String;
    writer: TStreamWriter;
    class var instance: TLogger;
    procedure Log(aLevel: TLogLevels; aMessage: String);
    constructor Create;
  public
    procedure Debug(aMessage: String);
    procedure Info(aMessage: String);
    procedure Warn(aMessage: String);
    procedure Error(aMessage: String);
    procedure SetLevel(aLevel: TLogLevels);
    class function GetLogger: TLogger;
end;

implementation

{ TLogger }

constructor TLogger.Create;
begin
  Self.logPath := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MTTools', 'log.txt');
  Self.writer := TStreamWriter.Create(Self.logPath, True);
end;

procedure TLogger.Debug(aMessage: String);
begin
  Self.Log(TLogLevels.DEBUG, aMessage);
end;

procedure TLogger.Error(aMessage: String);
begin
  Self.Log(TLogLevels.ERROR, aMessage);
end;

class function TLogger.GetLogger: TLogger;
begin
  if instance = nil then
    instance := TLogger.Create;
  Result := instance;
end;

procedure TLogger.Info(aMessage: String);
begin
  Self.Log(TLogLevels.INFO, aMessage);
end;

procedure TLogger.Log(aLevel: TLogLevels; aMessage: String);
var
  currentTime: TDateTime;
begin
  if Integer(aLevel) < Integer(Self.level) then Exit;

  currentTime := Now;

  Self.writer.WriteLine(Format(
    '[%s] %s: %s',
    [
      DateTimeToStr(currentTime),
      GetEnumName(TypeInfo(TLogLevels), Integer(aLevel)),
      aMessage
    ]
  ));
end;

procedure TLogger.SetLevel(aLevel: TLogLevels);
begin
  Self.level := aLevel;
end;

procedure TLogger.Warn(aMessage: String);
begin
  Self.Log(TLogLevels.WARNING, aMessage);
end;

end.

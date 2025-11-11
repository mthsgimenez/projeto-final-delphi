unit ToolTypeView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Grids, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, System.Generics.Collections,
  ToolTypeModel, ToolTypeController, ToolTypeDTO, Dependencies;

type
  TformToolType = class(TForm)
    pcontrolToolType: TPageControl;
    tabList: TTabSheet;
    tabCreate: TTabSheet;
    Image1: TImage;
    gridToolTypes: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure gridToolTypesDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
  private
    toolTypeController: TToolTypeController;
    toolTypes: TObjectList<TToolType>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formToolType: TformToolType;

implementation

{$R *.dfm}

constructor TformToolType.Create(AOwner: TComponent);
begin
  inherited;
  Self.toolTypeController := TDependencies.GetInstance.GetToolTypeController;
  Self.toolTypes := Self.toolTypeController.GetAll;
end;

destructor TformToolType.Destroy;
begin
  Self.toolTypes.Free;
  inherited;
end;

procedure TformToolType.FormCreate(Sender: TObject);
begin
  Self.gridToolTypes.ColCount := 3;
  Self.gridToolTypes.RowCount := 3;
  Self.gridToolTypes.DefaultRowHeight := 80;

  Self.gridToolTypes.RowHeights[0] := 30;

  Self.gridToolTypes.ColWidths[0] := 80;
  Self.gridToolTypes.ColWidths[1] := 325;
  Self.gridToolTypes.ColWidths[2] := 325;
end;

procedure TformToolType.gridToolTypesDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
begin
  with Self.gridToolTypes.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect);

      if ARow = 0 then begin
        Brush.Color := RGB($41, $69, $E1);
        Font.Color := clWhite;
        Font.Size := 12;

        FillRect(Rect);

        if ACol = 1 then TextOut(Rect.Left + 5, Rect.Top + 3, 'Nome / Descrição');
        if ACol = 2 then TextOut(Rect.Left + 5, Rect.Top + 3, 'Informações');

        Exit;
      end;

      if Assigned(Self.toolTypes) then begin
        if ARow < Self.toolTypes.Count then begin
          if ACol = 0 then
            StretchDraw(Rect, Self.Image1.Picture.Graphic);

          if ACol = 1 then begin
            Font.Color := clBlack;
            Font.Size := 16;
            TextOut(Rect.Left + 10, Rect.Top, Self.toolTypes[ARow].code);

            Font.Size := 12;
            TextOut(Rect.Left + 10, Rect.Top + 30, Self.toolTypes[ARow].description);

            Font.Color := clGrayText;
            Font.Size := 12;
            TextOut(Rect.Left + 10, Rect.Top + 54, '(' + Self.toolTypes[ARow].usage + ')');
          end;
        end;
      end;
    end;
end;

end.

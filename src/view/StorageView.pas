unit StorageView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Grids, System.Generics.Collections,
  Dependencies, StorageController, StorageModel, ToolTypeModel, StorageDTO, MessageHelper, ToolModel;

type
  TformStorage = class(TForm)
    pcontrolStorage: TPageControl;
    tabList: TTabSheet;
    gridStorages: TStringGrid;
    buttonCreate: TButton;
    buttonEdit: TButton;
    buttonDelete: TButton;
    buttonView: TButton;
    tabCreate: TTabSheet;
    tabToolTypes: TTabSheet;
    tabTools: TTabSheet;
    editName: TEdit;
    buttonCancel: TButton;
    buttonSave: TButton;
    gridToolTypes: TStringGrid;
    buttonToolTypesBack: TButton;
    buttonTools: TButton;
    gridTools: TStringGrid;
    buttonToolsBack: TButton;
    procedure buttonCancelClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
    procedure tabListShow(Sender: TObject);
    procedure gridStoragesDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure gridStoragesSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonCreateClick(Sender: TObject);
    procedure buttonEditClick(Sender: TObject);
    procedure buttonDeleteClick(Sender: TObject);
    procedure buttonViewClick(Sender: TObject);
    procedure tabToolTypesShow(Sender: TObject);
    procedure buttonToolTypesBackClick(Sender: TObject);
    procedure gridToolTypesDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure gridToolTypesSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonToolsClick(Sender: TObject);
    procedure tabToolsShow(Sender: TObject);
    procedure buttonToolsBackClick(Sender: TObject);
  private
    storageController: TStorageController;
    storages: TObjectList<TStorage>;
    selectedStorage: TStorage;

    toolTypes: TObjectList<TToolType>;
    selectedToolType: TToolType;

    tools: TObjectList<TTool>;
    selectedTool: TTool;

    procedure UpdateStorageGrid;
    procedure UpdateToolTypesGrid;
    procedure UpdateToolsGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formStorage: TformStorage;

implementation

{$R *.dfm}

{ TformStorage }

procedure TformStorage.buttonCancelClick(Sender: TObject);
begin
  Self.selectedStorage := nil;
  Self.pcontrolStorage.ActivePageIndex := 0;
  Self.editName.Clear;
end;

procedure TformStorage.buttonCreateClick(Sender: TObject);
begin
  Self.selectedStorage := nil;
  Self.pcontrolStorage.ActivePageIndex := 1;
end;

procedure TformStorage.buttonDeleteClick(Sender: TObject);
begin
  if not Assigned(Self.selectedStorage) then begin
    TMessageHelper.GetInstance.Error('Nenhum estoque selecionado');
    Exit;
  end;

  if TMessageHelper.GetInstance.Confirmation('Tem certeza de que quer remover o estoque ' + Self.selectedStorage.name + '?') then begin
    if not Self.storageController.DeleteStorage(Self.selectedStorage.id) then begin
      TMessageHelper.GetInstance.Error('Não foi possível remover o estoque, certifique-se de que não há ferramentas dentro do estoque antes de remove-lo.');
      Exit;
    end;

    Self.storages.Remove(Self.selectedStorage);
    Self.selectedStorage := nil;
    Self.UpdateStorageGrid;
  end;
end;

procedure TformStorage.buttonEditClick(Sender: TObject);
begin
  if not Assigned(Self.selectedStorage) then begin
    TMessageHelper.GetInstance.Error('Nenhum estoque selecionado');
    Exit;
  end;

  Self.pcontrolStorage.ActivePageIndex := 1;
  Self.editName.Text := Self.selectedStorage.name;
end;

procedure TformStorage.buttonSaveClick(Sender: TObject);
var
  data: TStorageDTO;
  updatedStorage: TStorage;
  newStorage: TStorage;
begin
  data.name := Self.editName.Text;
  if Trim(data.name) = '' then begin
    TMessageHelper.GetInstance.Error('Nome não pode estar vazio');
    Exit;
  end;

  if Assigned(Self.selectedStorage) then begin
    updatedStorage := Self.storageController.EditStorage(Self.selectedStorage.id, data);
    Self.storages.Remove(Self.selectedStorage);
    Self.storages.Add(updatedStorage);

    Self.pcontrolStorage.ActivePageIndex := 0;
    Self.editName.Clear;
    Exit;
  end;

  newStorage := Self.storageController.CreateStorage(data);
  Self.storages.Add(newStorage);
  Self.pcontrolStorage.ActivePageIndex := 0;
  Self.editName.Clear;
end;

procedure TformStorage.buttonToolsBackClick(Sender: TObject);
begin
  Self.pcontrolStorage.ActivePageIndex := 2;
end;

procedure TformStorage.buttonToolsClick(Sender: TObject);
begin
  if not Assigned(Self.selectedToolType) then begin
    TMessageHelper.GetInstance.Error('Nenhuma ferramenta selecionada');
    Exit;
  end;

  if Assigned(Self.tools) then
    Self.tools.Free;

  Self.tools := Self.storageController.GetTools(Self.selectedStorage, Self.selectedToolType);
  Self.pcontrolStorage.ActivePageIndex := 3;
end;

procedure TformStorage.buttonToolTypesBackClick(Sender: TObject);
begin
  Self.pcontrolStorage.ActivePageIndex := 0;
end;

procedure TformStorage.buttonViewClick(Sender: TObject);
begin
  if not Assigned(Self.selectedStorage) then begin
    TMessageHelper.GetInstance.Error('Nenhum estoque selecionado');
    Exit;
  end;

  if Assigned(Self.toolTypes) then
    Self.toolTypes.Free;

  Self.toolTypes := Self.storageController.GetToolTypes(Self.selectedStorage.id);
  Self.pcontrolStorage.ActivePageIndex := 2;
end;

constructor TformStorage.Create(AOwner: TComponent);
begin
  inherited;
  Self.storageController := TDependencies.GetInstance.GetStorageController;
  Self.storages := Self.storageController.GetStorages;
end;

destructor TformStorage.Destroy;
begin
  Self.storages.Free;
  if Assigned(Self.toolTypes) then
    Self.toolTypes.Free;

  if Assigned(Self.tools) then
    Self.tools.Free;
  inherited;
end;

procedure TformStorage.gridStoragesDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
var
  grid: TStringGrid;
begin
  grid := TStringGrid(Sender);

  if ARow = 0 then begin
    grid.Canvas.Brush.Color := RGB($41, $69, $E1);
    grid.Canvas.Font.Color := RGB($FF, $FF, $FF);
  end;

  grid.Canvas.FillRect(Rect);
  grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, grid.Cells[ACol, ARow]);
end;

procedure TformStorage.gridStoragesSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedStorage := TStorage(Self.gridStorages.Objects[0, ARow]);
end;

procedure TformStorage.gridToolTypesDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
var
  grid: TStringGrid;
begin
  grid := TStringGrid(Sender);

  if ARow = 0 then begin
    grid.Canvas.Brush.Color := RGB($41, $69, $E1);
    grid.Canvas.Font.Color := RGB($FF, $FF, $FF);
  end;

  grid.Canvas.FillRect(Rect);
  grid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 4, grid.Cells[ACol, ARow]);
end;

procedure TformStorage.gridToolTypesSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedToolType := TToolType(Self.gridToolTypes.Objects[0, ARow]);
end;

procedure TformStorage.tabListShow(Sender: TObject);
begin
  Self.UpdateStorageGrid;
end;

procedure TformStorage.tabToolsShow(Sender: TObject);
begin
  Self.UpdateToolsGrid;
end;

procedure TformStorage.tabToolTypesShow(Sender: TObject);
begin
  Self.UpdateToolTypesGrid;
end;

procedure TformStorage.UpdateStorageGrid;
var
  storage: TStorage;
  i: Integer;
begin
  with Self.gridStorages do begin
    RowCount := 1;
    Cells[0, 0] := 'Nome';
    Cells[1, 0] := 'Total ferramentas em estoque';
    Cells[2, 0] := 'Ferramentas em uso';
    Cells[3, 0] := 'Ferramentas disponíveis';
    ColWidths[0] := 170;
    ColWidths[1] := 170;
    ColWidths[2] := 170;
    ColWidths[3] := 170;

    if Assigned(Self.storages) then
      for storage in Self.storages do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := storage;
        Cells[0, i] := storage.name;
        Cells[1, i] := IntToStr(storage.quantityTotal);
        Cells[2, i] := IntToStr(storage.quantityInUse);
        Cells[3, i] := IntToStr(storage.quantityTotal - storage.quantityInUse);
      end;
  end;
end;

procedure TformStorage.UpdateToolsGrid;
var
  tool: TTool;
  i: Integer;
begin
  with Self.gridTools do begin
    RowCount := 1;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Estado';
    Cells[2, 0] := 'Número de afiações';
    Cells[3, 0] := 'Disponibilidade';
    ColWidths[0] := 170;
    ColWidths[1] := 170;
    ColWidths[2] := 170;
    ColWidths[3] := 175;

    if Assigned(Self.tools) then
      for tool in Self.tools do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := tool;
        Cells[0, i] := tool.code;
        Cells[1, i] := StateToString(tool.state);
        Cells[2, i] := IntToStr(tool.honingNum);
        Cells[3, i] := StatusToString(tool.status);
      end;
  end;
end;

procedure TformStorage.UpdateToolTypesGrid;
var
  tool: TToolType;
  i: Integer;
begin
  with Self.gridToolTypes do begin
    RowCount := 1;
    Cells[0, 0] := 'Código';
    Cells[1, 0] := 'Descrição';
    Cells[2, 0] := 'Qtde. em estoque';
    Cells[3, 0] := 'Qtde. em uso';
    Cells[4, 0] := 'Qtde. disponível';
    ColWidths[0] := 100;
    ColWidths[1] := 215;
    ColWidths[2] := 125;
    ColWidths[3] := 125;
    ColWidths[4] := 125;

    if Assigned(Self.toolTypes) then
      for tool in Self.toolTypes do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := tool;
        Cells[0, i] := tool.code;
        Cells[1, i] := tool.description;
        Cells[2, i] := IntToStr(tool.quantityTotal);
        Cells[3, i] := IntToStr(tool.quantityInUse);
        Cells[4, i] := IntToStr(tool.quantityTotal - tool.quantityInUse);
      end;
  end;
end;

end.

unit ToolTypeView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Grids, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, System.Generics.Collections,
  ToolTypeModel, ToolTypeController, ToolTypeDTO, Dependencies, Vcl.Buttons,
  Vcl.Skia, SupplierController, SupplierModel, MessageHelper, Session, Permissions;

type
  TformToolType = class(TForm)
    pcontrolToolType: TPageControl;
    tabCreate: TTabSheet;
    tabList: TTabSheet;
    imageBrokenIcon: TImage;
    gridToolTypes: TStringGrid;
    labelTitle: TLabel;
    panelButtonCreate: TPanel;
    shapeButtonCreate: TShape;
    buttonCreate: TSpeedButton;
    editCode: TEdit;
    editDescription: TEdit;
    comboSuppliers: TComboBox;
    buttonSelectImage: TButton;
    editImage: TEdit;
    editPrice: TEdit;
    comboUsage: TComboBox;
    labelSupplier: TLabel;
    labelUsage: TLabel;
    buttonSave: TButton;
    buttonCancel: TButton;
    comboFamily: TComboBox;
    labelFamily: TLabel;
    imageDeleteIcon: TImage;
    imageEditIcon: TImage;
    opdialogImage: TOpenDialog;
    labelPrice: TLabel;
    labelImage: TLabel;
    labelDescription: TLabel;
    labelCode: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure gridToolTypesDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure buttonCreateClick(Sender: TObject);
    procedure buttonCancelClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
    procedure gridToolTypesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure buttonSelectImageClick(Sender: TObject);
    procedure editPriceExit(Sender: TObject);
    procedure tabCreateHide(Sender: TObject);
  private
    supplierController: TSupplierController;
    toolTypeController: TToolTypeController;
    toolTypes: TObjectList<TToolType>;
    suppliers: TObjectList<TSupplier>;
    images: TDictionary<String, TPicture>;

    selectedTool: TToolType;

    procedure updateComboSuppliers;
    procedure clearEdits;
    procedure getImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formToolType: TformToolType;

implementation

{$R *.dfm}

procedure TformToolType.buttonCancelClick(Sender: TObject);
begin
  Self.pcontrolToolType.ActivePageIndex := 1;
  Self.selectedTool := nil;
end;

procedure TformToolType.buttonCreateClick(Sender: TObject);
begin
  Self.pcontrolToolType.ActivePageIndex := 0;
end;

procedure TformToolType.buttonSaveClick(Sender: TObject);
var
  data: TToolTypeDTO;
  price: double;
  problems: TStringList;
  newTool, updatedTool: TToolType;
  supplier: TSupplier;
  unformattedPrice: String;
begin
  data.code := Self.editCode.Text;
  data.description := Self.editDescription.Text;
  data.family := Self.comboFamily.Text;
  data.usage := Self.comboUsage.Text;
  if Self.comboSuppliers.ItemIndex <> -1 then begin
    supplier := TSupplier(Self.comboSuppliers.Items.Objects[Self.comboSuppliers.ItemIndex]);
    data.id_supplier := supplier.id;
  end;

  unformattedPrice := StringReplace(Self.editPrice.Text, 'R$', '', [rfReplaceAll]);
  unformattedPrice := StringReplace(unformattedPrice, '.', '', [rfReplaceAll]);
  unformattedPrice := StringReplace(unformattedPrice, ',', '.', [rfReplaceAll]);

  if not TryStrToFloat(unformattedPrice, price) then begin
    TMessageHelper.GetInstance.Error('Preço inválido');
    Exit;
  end;
  data.price := Currency(price);
  data.image := Self.editImage.Text;

  problems := data.Validate;
  if problems.Count > 0 then begin
    TMessageHelper.GetInstance.Error(problems.Text);
    problems.Free;
    Exit;
  end;
  problems.Free;

  if not Assigned(Self.selectedTool) then begin
    newTool := Self.toolTypeController.CreateToolType(data);
    if not Assigned(newTool) then begin
      TMessageHelper.GetInstance.Error('Não foi possível cadastrar a ferramenta');
      Exit;
    end;

    if not Assigned(Self.toolTypes) then
      Self.toolTypes := TObjectList<TToolType>.Create;

    Self.toolTypes.Add(newTool);
    Self.gridToolTypes.RowCount := Self.toolTypes.Count + 1;

    Self.pcontrolToolType.ActivePageIndex := 1;
    Exit;
  end;

  updatedTool := Self.toolTypeController.EditToolType(Self.selectedTool.id, data);

  if Assigned(updatedTool) then begin
    Self.toolTypes.Remove(Self.selectedTool);
    Self.selectedTool := nil;
    Self.toolTypes.Add(updatedTool);
  end;

  Self.pcontrolToolType.ActivePageIndex := 1;
end;

procedure TformToolType.buttonSelectImageClick(Sender: TObject);
begin
  if Self.opdialogImage.Execute then begin
    Self.editImage.Text := Self.opdialogImage.FileName;
  end;
end;

procedure TformToolType.clearEdits;
begin
  Self.editCode.Clear;
  Self.editDescription.Clear;
  Self.comboFamily.ItemIndex := -1;
  Self.comboUsage.ItemIndex := -1;
  Self.comboSuppliers.ItemIndex := -1;
  Self.editPrice.Clear;
  Self.editImage.Clear;
end;

constructor TformToolType.Create(AOwner: TComponent);
begin
  inherited;
  Self.toolTypeController := TDependencies.GetInstance.GetToolTypeController;
  Self.supplierController := TDependencies.GetInstance.GetSupplierController;
  Self.toolTypes := Self.toolTypeController.GetAll;
  Self.suppliers := Self.supplierController.GetSuppliers;
  Self.images := TDictionary<String, TPicture>.Create;
  Self.selectedTool := nil;
end;

destructor TformToolType.Destroy;
var
  image: TPicture;
begin
  Self.toolTypes.Free;
  Self.suppliers.Free;
  // Dicionário não da free nos objetos contidos nele
  for image in Self.images.Values do begin
    image.Free;
  end;
  Self.images.Free;
  inherited;
end;

procedure TformToolType.editPriceExit(Sender: TObject);
var
  value: Double;
  formattedValue: String;
begin
  formattedValue := Self.editPrice.Text;
  formattedValue := StringReplace(formattedValue, ',', '.', [rfReplaceAll]);

  if TryStrToFloat(formattedValue, value) then begin
    formattedValue := FormatFloat('#,0.00', value);
    formattedValue := StringReplace(formattedValue, '.', '#', [rfReplaceAll]);
    formattedValue := StringReplace(formattedValue, ',', '.', [rfReplaceAll]);
    formattedValue := StringReplace(formattedValue, '#', ',', [rfReplaceAll]);
    Self.editPrice.Text := 'R$' + formattedValue;
  end;
end;

procedure TformToolType.FormCreate(Sender: TObject);
begin
  Self.gridToolTypes.ColCount := 5;
  if Assigned(Self.toolTypes) then begin
    Self.gridToolTypes.RowCount := Self.toolTypes.Count + 1;
  end else begin
    Self.gridToolTypes.RowCount := 1;
  end;

  Self.gridToolTypes.DefaultRowHeight := 80;
  Self.gridToolTypes.RowHeights[0] := 30;

  Self.gridToolTypes.ColWidths[0] := 80;
  Self.gridToolTypes.ColWidths[1] := 280;
  Self.gridToolTypes.ColWidths[2] := 280;
  Self.gridToolTypes.ColWidths[3] := 40;
  Self.gridToolTypes.ColWidths[4] := 40;

  if Assigned(TSession.GetUser.permissionGroup) then begin
    Self.panelButtonCreate.Visible := TSession.GetUser.permissionGroup.hasPermission(TOOLS_CREATE);
  end else begin
    Self.panelButtonCreate.Visible := False;
  end;

  Self.updateComboSuppliers;
  Self.getImages;
end;

procedure TformToolType.getImages;
var
  tool: TToolType;
  image: TPicture;
begin
  if Assigned(Self.toolTypes) then
    for tool in Self.toolTypes do begin
      if tool.image <> '' then
        if not Self.images.ContainsKey(tool.image) then begin
          try
            image := Self.toolTypeController.LoadImage(tool.image);
            Self.images.Add(tool.image, image);
          except
            continue;
          end;
        end;
    end;
end;

procedure TformToolType.gridToolTypesDrawCell(Sender: TObject; ACol,
  ARow: LongInt; Rect: TRect; State: TGridDrawState);
var
  tool: TToolType;
  image: TPicture;
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
        if ARow - 1 < Self.toolTypes.Count then begin
          tool := Self.toolTypes[ARow - 1];

          if ACol = 0 then begin
            if Self.images.TryGetValue(tool.image, image) then begin
              StretchDraw(Rect, image.Graphic);
            end else begin
              StretchDraw(Rect, Self.imageBrokenIcon.Picture.Graphic);
            end;
          end;

          if ACol = 1 then begin
            Font.Color := clBlack;
            Font.Size := 16;
            TextOut(Rect.Left + 10, Rect.Top, tool.code);

            Font.Size := 12;
            TextOut(Rect.Left + 10, Rect.Top + 30, tool.description);

            Font.Color := clGrayText;
            Font.Size := 12;
            TextOut(Rect.Left + 10, Rect.Top + 54, '(' + tool.usage + ')');
          end else if ACol = 2 then begin
            Font.Color := clBlack;
            Font.Size := 12;
            TextOut(Rect.Left + 10, Rect.Top + 8, 'Família: ' + tool.family);
            TextOut(Rect.Left + 10, Rect.Top + 30, 'Fornecedor: ' + tool.supplier.tradeName);
            TextOut(Rect.Left + 10, Rect.Top + 50, 'Preço: ' + FormatCurr('R$ 0.00', tool.price));
          end else if ACol = 3 then begin
            if Assigned(TSession.GetUser.permissionGroup) then begin
              if TSession.GetUser.permissionGroup.hasPermission(TOOLS_UPDATE) then
                StretchDraw(Rect, Self.imageEditIcon.Picture.Graphic);
            end;
          end else if ACol = 4 then begin
            if Assigned(TSession.GetUser.permissionGroup) then begin
              if TSession.GetUser.permissionGroup.hasPermission(TOOLS_DELETE) then
                StretchDraw(Rect, Self.imageDeleteIcon.Picture.Graphic);
            end;
          end;
        end;
      end;
    end;
end;

procedure TformToolType.gridToolTypesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row, col: Integer;
  tool: TToolType;
  formattedValue: String;
begin
  Self.gridToolTypes.MouseToCell(X, Y, col, row);

  if row < 1 then Exit;

  try
    tool := Self.toolTypes[row - 1];
  except
    Exit;
  end;

  if col = 3 then begin // Edit
    if Assigned(TSession.GetUser.permissionGroup) then begin
      if TSession.GetUser.permissionGroup.hasPermission(TOOLS_UPDATE) then begin
        Self.selectedTool := tool;
        Self.pcontrolToolType.ActivePageIndex := 0;

        Self.editCode.Text := tool.code;
        Self.editDescription.Text := tool.description;
        Self.editImage.Text := tool.image;
        Self.comboFamily.ItemIndex := Self.comboFamily.Items.IndexOf(tool.family);
        Self.comboUsage.ItemIndex := Self.comboUsage.Items.IndexOf(tool.usage);
        Self.comboSuppliers.ItemIndex := Self.comboSuppliers.Items.IndexOf(tool.supplier.tradeName);

        formattedValue := FormatCurr('#,0.00', tool.price);
        formattedValue := StringReplace(formattedValue, '.', '#', [rfReplaceAll]);
        formattedValue := StringReplace(formattedValue, ',', '.', [rfReplaceAll]);
        formattedValue := StringReplace(formattedValue, '#', ',', [rfReplaceAll]);
        Self.editPrice.Text := 'R$' + formattedValue;

        Exit;
      end;
    end;
  end;

  if col = 4 then begin // Delete
    if Assigned(TSession.GetUser.permissionGroup) then begin
      if TSession.GetUser.permissionGroup.hasPermission(TOOLS_DELETE) then begin
        if not TMessageHelper.GetInstance.Confirmation(
          Format('Deseja mesmo excluir a ferramenta: %s?', [tool.code]))
        then Exit;

        if Self.toolTypeController.DeleteToolType(tool.id) then begin
          Self.toolTypes.Remove(tool);
          Self.gridToolTypes.RowCount := Self.toolTypes.Count + 1;
          Exit;
        end;

        TMessageHelper.GetInstance.Error('Não foi possível deletar a ferramenta.');
      end;
    end;
  end;
end;

procedure TformToolType.tabCreateHide(Sender: TObject);
begin
  Self.clearEdits;
end;

procedure TformToolType.updateComboSuppliers;
var
  supplier: TSupplier;
begin
  if Assigned(Self.suppliers) and (Self.suppliers.Count > 0) then begin
    Self.comboSuppliers.Clear;
    for supplier in Self.suppliers do begin
      Self.comboSuppliers.AddItem(supplier.tradeName, supplier);
    end;
  end;
end;

end.

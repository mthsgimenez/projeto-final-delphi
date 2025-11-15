unit SupplierView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, Vcl.StdCtrls, MessageHelper,
  Vcl.ExtCtrls, Vcl.Skia, Vcl.Mask, Dependencies, SupplierController, SupplierModel, SupplierDTO, System.Generics.Collections, UserModel, Session, Permissions;

type
  TformSupplier = class(TForm)
    pcontrolSupplier: TPageControl;
    tabView: TTabSheet;
    gridSuppliers: TStringGrid;
    buttonCreate: TButton;
    buttonDelete: TButton;
    buttonEdit: TButton;
    tabCreate: TTabSheet;
    Label1: TLabel;
    panelCNPJ: TPanel;
    imgSearch: TImage;
    panelTradeName: TPanel;
    editCNPJ: TMaskEdit;
    editTradeName: TEdit;
    labelTradeName: TLabel;
    panelLegalName: TPanel;
    labelLegalName: TLabel;
    editLegalName: TEdit;
    panelEmail: TPanel;
    labelEmail: TLabel;
    editEmail: TEdit;
    panelCEP: TPanel;
    labelCEP: TLabel;
    editCEP: TMaskEdit;
    panelPhone: TPanel;
    labelPhone: TLabel;
    editPhone: TMaskEdit;
    buttonSave: TButton;
    buttonBack: TButton;
    procedure gridSuppliersDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure imgSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure buttonBackClick(Sender: TObject);
    procedure gridSuppliersSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure buttonCreateClick(Sender: TObject);
    procedure buttonEditClick(Sender: TObject);
    procedure buttonDeleteClick(Sender: TObject);
    procedure tabViewShow(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
  private
    supplierController: TSupplierController;
    selectedSupplier: TSupplier;
    suppliers: TObjectList<TSupplier>;
    procedure UpdateGrid;
    procedure ClearEdits;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  formSupplier: TformSupplier;

implementation

{$R *.dfm}

procedure TformSupplier.buttonBackClick(Sender: TObject);
begin
  Self.selectedSupplier := nil;
  Self.pcontrolSupplier.ActivePage := Self.pcontrolSupplier.Pages[0];
  Self.ClearEdits;
end;

procedure TformSupplier.buttonCreateClick(Sender: TObject);
begin
  Self.selectedSupplier := nil;
  Self.pcontrolSupplier.ActivePage := Self.pcontrolSupplier.Pages[1];
end;

procedure TformSupplier.buttonDeleteClick(Sender: TObject);
begin
  if not Assigned(Self.selectedSupplier) then begin
    TMessageHelper.GetInstance.Error('Nenhum fornecedor selecionado');
    Exit;
  end;

  if TMessageHelper.GetInstance.Confirmation('Deseja mesmo desativar o fornecedor ' + Self.selectedSupplier.tradeName + ' ?') then begin
    if Self.supplierController.DeleteSupplier(Self.selectedSupplier.id) then begin
      Self.suppliers.Remove(Self.selectedSupplier);
      Self.selectedSupplier := nil;
      Self.UpdateGrid;
    end;
  end;
end;

procedure TformSupplier.buttonEditClick(Sender: TObject);
begin
  if not Assigned(Self.selectedSupplier) then begin
    TMessageHelper.GetInstance.Error('Nenhum fornecedor selecionado');
    Exit;
  end;

  Self.pcontrolSupplier.ActivePage := Self.pcontrolSupplier.Pages[1];

  Self.editCNPJ.Text := Self.selectedSupplier.CNPJ.getCNPJ;
  Self.editTradeName.Text := Self.selectedSupplier.tradeName;
  Self.editLegalName.Text := Self.selectedSupplier.legalName;
  Self.editEmail.Text := Self.selectedSupplier.email;
  Self.editPhone.Text := Self.selectedSupplier.phone;
  Self.editCEP.Text := Self.selectedSupplier.CEP;
end;

procedure TformSupplier.buttonSaveClick(Sender: TObject);
var
  data: TSupplierDTO;
  supplier: TSupplier;
  errors: TStringList;
begin
  data.CNPJ := Self.editCNPJ.Text;
  data.tradeName := Self.editTradeName.Text;
  data.legalName := Self.editLegalName.Text;
  data.email := Self.editEmail.Text;
  data.phone := Self.editPhone.Text;
  data.CEP := Self.editCEP.Text;

  if Assigned(Self.selectedSupplier) then begin
    errors := data.ValidateDTO();

    try
      if errors.Count > 0 then begin
        TMessageHelper.GetInstance.Error(errors.Text);
        Exit;
      end;
    finally
      errors.Free;
    end;

    supplier := Self.supplierController.EditSupplier(Self.selectedSupplier.id, data);

    if not Assigned(Self.suppliers) then
      Self.suppliers := TObjectList<TSupplier>.Create;

    Self.suppliers.Remove(Self.selectedSupplier);
    Self.selectedSupplier := nil;

    Self.suppliers.Add(supplier);
  end else begin
    errors := data.ValidateDTO;
    try
      if errors.Count > 0 then begin
        TMessageHelper.GetInstance.Error(errors.Text);
        Exit;
      end;
    finally
      errors.Free;
    end;

    if not Assigned(Self.suppliers) then
      Self.suppliers := TObjectList<TSupplier>.Create;

    supplier := Self.supplierController.CreateSupplier(data);
    Self.suppliers.Add(supplier);
  end;

  Self.ClearEdits;
  Self.pcontrolSupplier.ActivePage := Self.pcontrolSupplier.Pages[0];
end;

procedure TformSupplier.ClearEdits;
begin
  Self.editCNPJ.Clear;
  Self.editTradeName.Clear;
  Self.editLegalName.Clear;
  Self.editEmail.Clear;
  Self.editPhone.Clear;
  Self.editCEP.Clear;
end;

constructor TformSupplier.Create(AOwner: TComponent);
begin
  inherited;
  Self.supplierController := TDependencies.GetInstance.GetSupplierController;
  Self.suppliers := Self.supplierController.GetSuppliers;
end;

destructor TformSupplier.Destroy;
begin
  Self.suppliers.Free;
  inherited;
end;

procedure TformSupplier.FormCreate(Sender: TObject);
var
  user: TUserModel;
begin
  user := TSession.GetUser;

  if Assigned(user.permissionGroup) then begin
    Self.buttonCreate.Visible := user.permissionGroup.hasPermission(SUPPLIERS_CREATE);
    Self.buttonEdit.Visible := user.permissionGroup.hasPermission(SUPPLIERS_UPDATE);
    Self.buttonDelete.Visible := user.permissionGroup.hasPermission(SUPPLIERS_DELETE);
  end;
end;

procedure TformSupplier.gridSuppliersDrawCell(Sender: TObject; ACol,
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

procedure TformSupplier.gridSuppliersSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  if ARow = 0 then begin
    CanSelect := False;
    Exit;
  end;

  Self.selectedSupplier := TSupplier(Self.gridSuppliers.Objects[0, ARow]);
end;

procedure TformSupplier.imgSearchClick(Sender: TObject);
var
  searchResult: TSupplierDTO;
begin
  try
    searchResult := Self.supplierController.SearchCNPJ(Self.editCNPJ.Text);
  except
    TMessageHelper.GetInstance.Error('CNPJ Inválido');
    Exit;
  end;

  if searchResult.cnpj = '' then begin
    TMessageHelper.GetInstance.Error('Dados não encontrados');
    Exit;
  end;

  Self.editTradeName.Text := searchResult.tradeName;
  Self.editLegalName.Text := searchResult.legalName;
  Self.editCEP.Text := searchResult.cep;
  Self.editEmail.Text := searchResult.email;
  Self.editPhone.Text := searchResult.phone;
end;

procedure TformSupplier.tabViewShow(Sender: TObject);
begin
  Self.UpdateGrid;
end;

procedure TformSupplier.UpdateGrid;
var
  supplier: TSupplier;
  i: Integer;
begin
  with Self.gridSuppliers do begin
    RowCount := 1;
    Cells[0, 0] := 'Nome fantasia';
    Cells[1, 0] := 'CNPJ';
    Cells[2, 0] := 'Razão social';
    Cells[3, 0] := 'CEP';
    Cells[4, 0] := 'Email';
    Cells[5, 0] := 'Phone';
    ColWidths[0] := Width div 3;
    ColWidths[1] := Width div 3;
    ColWidths[2] := Width div 3;
    ColWidths[3] := Width div 3;
    ColWidths[4] := Width div 3;
    ColWidths[5] := Width div 3;

    if Assigned(Self.suppliers) then
      for supplier in Self.suppliers do begin
        i := RowCount;
        RowCount := RowCount + 1;
        Objects[0, i] := supplier;
        Cells[0, i] := supplier.tradeName;
        Cells[1, i] := supplier.CNPJ.getCNPJ;
        Cells[2, i] := supplier.legalName;
        Cells[3, i] := supplier.CEP;
        Cells[4, i] := supplier.email;
        Cells[5, i] := supplier.phone;
      end;
  end;
end;

end.

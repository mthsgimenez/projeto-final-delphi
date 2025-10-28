unit UserRepository;

interface

uses
  CrudRepositoryInterface, UserModel, DBHelper, UserDAO, PermissionGroupDAO,
  System.Generics.Collections;

type TUserRepository = class(TInterfacedObject, ICrudRepository<TUserModel>)
  private
    userDAO: TUserDAO;
    permissionDAO: TPermissionGroupDAO;
    helper: TDBHelper;
  public
    function Save(aUser: TUserModel): TUserModel;
    function FindById(aId: Integer): TUserModel;
    function FindAll(): TObjectList<TUserModel>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
    function FindByLogin(aLogin: String): TUserModel;
    constructor Create;
    destructor Destroy;
end;

implementation

{ TUserRepository }

constructor TUserRepository.Create;
begin
  Self.userDAO := TUserDAO.Create;
  Self.permissionDAO := TPermissionGroupDAO.Create;
  Self.helper := TDBHelper.Create;
end;

function TUserRepository.DeleteById(aId: Integer): Boolean;
begin
  Result := Self.userDAO.DeleteById(aId);
end;

destructor TUserRepository.Destroy;
begin
  Self.userDAO.Free;
  Self.permissionDAO.Free;
  Self.helper.Free;
end;

function TUserRepository.ExistsById(aId: Integer): Boolean;
begin
  Result := Self.helper.CheckIfAlreadyExists('users', 'id', aId);
end;

function TUserRepository.FindAll: TObjectList<TUserModel>;
var
  users: TObjectList<TUserModel>;
  user: TUserModel;
begin
  users := Self.userDAO.SelectAll;

  if Assigned(users) and (users.Count > 0) then
    for user in users do
      user.permissionGroup := Self.permissionDAO.SelectByUserId(user.id);

  Result := users;
end;

function TUserRepository.FindById(aId: Integer): TUserModel;
var
  user: TUserModel;
begin
  user := Self.userDAO.SelectById(aId);
  if Assigned(user) then
    user.permissionGroup := Self.permissionDAO.SelectByUserId(user.id);

  Result := user;
end;

function TUserRepository.FindByLogin(aLogin: String): TUserModel;
var
  user: TUserModel;
begin
  user := Self.userDAO.SelectByLogin(aLogin);
  if Assigned(user) then
    user.permissionGroup := Self.permissionDAO.SelectByUserId(user.id);

  Result := user;
end;

function TUserRepository.Save(aUser: TUserModel): TUserModel;
var
  user: TUserModel;
begin
  if aUser.id = 0 then begin
    user := Self.userDAO.Insert(aUser);
  end else begin
    user := Self.userDAO.Update(aUser);
  end;

  if Assigned(user) then
    user.permissionGroup := Self.permissionDAO.SelectByUserId(user.id);

  Result := user;
end;

end.

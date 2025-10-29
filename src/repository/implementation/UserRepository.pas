unit UserRepository;

interface

uses
  CrudRepositoryInterface, UserModel, DBHelper, UserDAOInterface, PermissionGroupDAOInterface,
  System.Generics.Collections;

type TUserRepository = class(TInterfacedObject, ICrudRepository<TUserModel>)
  private
    userDAO: IUserDAO;
    permissionDAO: IPermissionGroupDAO;
    helper: TDBHelper;
  public
    function Save(aUser: TUserModel): TUserModel;
    function FindById(aId: Integer): TUserModel;
    function FindAll(): TObjectList<TUserModel>;
    function ExistsById(aId: Integer): Boolean;
    function DeleteById(aId: Integer): Boolean;
    function FindByLogin(aLogin: String): TUserModel;
    constructor Create(aUserDAO: IUserDAO; aPermissionDAO: IPermissionGroupDAO);
    destructor Destroy; override;
end;

implementation

{ TUserRepository }

constructor TUserRepository.Create(aUserDAO: IUserDAO;
  aPermissionDAO: IPermissionGroupDAO);
begin
  Self.userDAO := aUserDAO;
  Self.permissionDAO := aPermissionDAO;
  Self.helper := TDBHelper.Create;
end;

function TUserRepository.DeleteById(aId: Integer): Boolean;
begin
  Result := Self.userDAO.DeleteById(aId);
end;

destructor TUserRepository.Destroy;
begin
  Self.helper.Free;
  inherited;
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

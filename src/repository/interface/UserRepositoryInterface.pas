unit UserRepositoryInterface;

interface

uses CRUDRepositoryInterface, UserModel;

type IUserRepository = interface(ICrudRepository<TUserModel>)
  function FindByLogin(aLogin: String): TUserModel;
end;

implementation

end.

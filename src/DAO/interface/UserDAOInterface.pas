unit UserDAOInterface;

interface

uses DAOInterface, UserModel;

type IUserDAO = interface(IDAO<TUserModel>)
  function SelectByLogin(aLogin: String): TUserModel;
end;

implementation

end.

unit DAOInterface;

interface

uses System.Generics.Collections;

type IDAO<TEntity: class> = interface
  function Insert(aEntity: TEntity): TEntity;
  function SelectById(aId: Integer): TEntity;
  function SelectAll(): TObjectList<TEntity>;
  function Update(aEntity: TEntity): TEntity;
  function DeleteById(aId: Integer): Boolean;
end;

implementation

end.

unit CrudRepositoryInterface;

interface

uses System.Generics.Collections;

type ICrudRepository<TEntity: class> = interface
  function Save(aEntity: TEntity): TEntity;
  function FindById(aId: Integer): TEntity;
  function FindAll(): TObjectList<TEntity>;
  function DeleteById(aId: Integer): Boolean;
  function ExistsById(aId: Integer): Boolean;
end;

implementation

end.

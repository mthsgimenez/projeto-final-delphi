INSERT INTO permissions VALUES
	(1, 'users_create', 'Permite cadastrar usuários'),
	(2, 'users_update', 'Permite editar dados de usuários existentes'),
	(3, 'users_delete', 'Permite desativar usuários'),
	(4, 'group_permissions', 'Permite criar e alterar grupos de permissões'),
	(5, 'suppliers_create', 'Permite cadastrar fornecedores'),
	(6, 'suppliers_update', 'Permite alterar dados de fornecedores'),
	(7, 'suppliers_delete', 'Permite desativar fornecedores');

INSERT INTO permission_groups ("name") VALUES ('admin');

INSERT INTO permission_groups_permissions (id_pgroup, id_permission) VALUES
	(1, 1),
	(1, 2),
	(1, 3),
	(1, 4),
	(1, 5),
	(1, 6),
	(1, 7);

-- Cria um usuário 'admin' com a senha 'admin'
INSERT INTO users ("name", login, hash, id_pgroup) VALUES
('admin', 'admin', '$2a$12$0G6QqmVBdx3i5KvcRQYauOk265bdBTC2DPA7a6RaUm1hLMU3fJ5ey', 1)
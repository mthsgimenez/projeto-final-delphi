CREATE TABLE storages (
	id serial NOT NULL,
	"name" varchar(50) NOT NULL,
	CONSTRAINT storages_pk PRIMARY KEY (id),
	CONSTRAINT storages_unique UNIQUE (name)
);

CREATE TABLE suppliers (
	id serial NOT NULL,
	trade_name varchar(80) NOT NULL,
	legal_name varchar(120) NOT NULL,
	cnpj varchar(14) NOT NULL,
	cep varchar(8) NOT NULL,
	email varchar(100) NULL,
	phone varchar(16) NULL,
	CONSTRAINT suppliers_pk PRIMARY KEY (id),
	CONSTRAINT suppliers_unique UNIQUE (cnpj),
	CONSTRAINT suppliers_unique_1 UNIQUE (trade_name),
	CONSTRAINT suppliers_unique_2 UNIQUE (legal_name)
);

CREATE TABLE permission_groups (
	id serial NOT NULL,
	"name" varchar(50) NOT NULL,
	CONSTRAINT permission_groups_pk PRIMARY KEY (id),
	CONSTRAINT permission_groups_unique UNIQUE (name)
);

CREATE TABLE users (
	id serial NOT NULL,
	"name" varchar(50) NOT NULL,
	login varchar(25) NOT NULL,
	hash varchar(60) NOT NULL,
	id_pgroup int NULL,
	CONSTRAINT users_pk PRIMARY KEY (id),
	CONSTRAINT users_unique UNIQUE (login),
	CONSTRAINT users_pgroup_fk FOREIGN KEY (id_pgroup) REFERENCES permission_groups(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE permissions (
	id int NOT NULL,
	"name" varchar(100) NOT NULL,
	description text NOT NULL,
	CONSTRAINT permissions_pk PRIMARY KEY (id),
	CONSTRAINT permissions_unique UNIQUE (name)
);

CREATE TABLE purchase_orders (
	id serial NOT NULL,
	id_supplier int NOT NULL,
	status varchar(15) DEFAULT 'OPEN'::character varying NOT NULL,
	issued_at TIMESTAMP NOT NULL DEFAULT NOW(),
	status_updated_at TIMESTAMP NULL,
	CONSTRAINT purchase_orders_pk PRIMARY KEY (id),
	CONSTRAINT purchase_orders_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE service_orders (
	id serial NOT NULL,
	id_supplier int NOT NULL,
	status varchar(15) DEFAULT 'OPEN'::character varying NOT NULL,
	issued_at TIMESTAMP NOT NULL DEFAULT NOW(),
	status_updated_at TIMESTAMP NULL,
	price money NOT NULL,
	CONSTRAINT service_orders_pk PRIMARY KEY (id),
	CONSTRAINT service_orders_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE tools_models (
	id serial NOT NULL,
	code varchar(20) NOT NULL,
	description varchar(100) NOT NULL,
	"family" varchar(15) NOT NULL,
	"usage" varchar(15) NOT NULL,
	id_supplier int NOT NULL,
	price money NOT NULL,
	image text NULL,
	CONSTRAINT tools_models_pk PRIMARY KEY (id),
	CONSTRAINT tools_models_unique UNIQUE (code),
	CONSTRAINT tools_models_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE permission_groups_permissions (
	id serial NOT NULL,
	id_permission int NOT NULL,
	id_pgroup int NOT NULL,
	CONSTRAINT permission_groups_permissions_pk PRIMARY KEY (id),
	CONSTRAINT permission_groups_permissions_permissions_fk FOREIGN KEY (id_permission) REFERENCES permissions(id) ON DELETE RESTRICT ON UPDATE CASCADE,
	CONSTRAINT permission_groups_permissions_pgroup_fk FOREIGN KEY (id_pgroup) REFERENCES permission_groups(id) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE purchase_order_tools (
	id serial NOT NULL,
	id_tool_model int NOT NULL,
	id_purchase_order int NOT NULL,
	tool_quantity int NOT NULL,
	CONSTRAINT purchase_order_tools_pk PRIMARY KEY (id),
	CONSTRAINT purchase_order_tools_purchase_orders_fk FOREIGN KEY (id_purchase_order) REFERENCES purchase_orders(id) ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT purchase_order_tools_tools_models_fk FOREIGN KEY (id_tool_model) REFERENCES tools_models(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE tools (
	id serial NOT NULL,
	code varchar(30) NOT NULL,
	id_tool_model int NOT NULL,
	state varchar(15) DEFAULT 'NEW'::character varying NOT NULL,
	honing_num int DEFAULT 0 NOT NULL,
	id_storage int NOT NULL,
	status varchar(30) DEFAULT 'AVAILABLE'::character varying NOT NULL, 
	CONSTRAINT tools_pk PRIMARY KEY (id),
	CONSTRAINT tools_unique UNIQUE (code),
	CONSTRAINT tools_storages_fk FOREIGN KEY (id_storage) REFERENCES storages(id) ON DELETE RESTRICT ON UPDATE CASCADE,
	CONSTRAINT tools_tools_models_fk FOREIGN KEY (id_tool_model) REFERENCES tools_models(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE service_order_tools (
	id serial NOT NULL,
	id_tool int NOT NULL,
	id_service_order int NOT NULL,
	CONSTRAINT service_order_tools_pk PRIMARY KEY (id),
	CONSTRAINT service_order_tools_service_orders_fk FOREIGN KEY (id_service_order) REFERENCES service_orders(id) ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT service_order_tools_tools_fk FOREIGN KEY (id_tool) REFERENCES tools(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

INSERT INTO permissions VALUES
	(1, 'users_create', 'Permite cadastrar usuários'),
	(2, 'users_update', 'Permite editar dados de usuários existentes'),
	(3, 'users_delete', 'Permite desativar usuários'),
	(4, 'group_permissions', 'Permite criar e alterar grupos de permissões'),
	(5, 'suppliers_create', 'Permite cadastrar fornecedores'),
	(6, 'suppliers_update', 'Permite alterar dados de fornecedores'),
	(7, 'suppliers_delete', 'Permite desativar fornecedores'),
	(8, 'tools_create', 'Permite cadastrar ferramentas'),
	(9, 'tools_update', 'Permite alterar dados de ferramentas'),
	(10, 'tools_delete', 'Permite desativar ferramentas'),
	(11, 'storage', 'Permite visualizar e movimentar o estoque'),
	(12, 'orders', 'Permite emitir e cancelar ordens de serviço e de compra'),
	(13, 'receipt', 'Permite receber ordens de compra e de serviço, dando entrada no estoque'),
	(14, 'reports', 'Permite emitir relatórios');

INSERT INTO permission_groups ("name") VALUES ('admin');

INSERT INTO permission_groups_permissions (id_pgroup, id_permission) VALUES
	(1, 1),
	(1, 2),
	(1, 3),
	(1, 4),
	(1, 5),
	(1, 6),
	(1, 7),
	(1, 8),
	(1, 9),
	(1, 10),
	(1, 11),
	(1, 12),
	(1, 13),
	(1, 14);

-- Cria um usuário 'admin' com a senha 'admin'
INSERT INTO users ("name", login, hash, id_pgroup) VALUES
('admin', 'admin', '$2a$12$0G6QqmVBdx3i5KvcRQYauOk265bdBTC2DPA7a6RaUm1hLMU3fJ5ey', 1)

-- ALTER SEQUENCE storages_id_seq RESTART WITH 1;
-- ALTER SEQUENCE suppliers_id_seq RESTART WITH 1;
-- ALTER SEQUENCE permission_groups_id_seq RESTART WITH 1;
-- ALTER SEQUENCE users_id_seq RESTART WITH 1;
-- ALTER SEQUENCE purchase_orders_id_seq RESTART WITH 1;
-- ALTER SEQUENCE service_orders_id_seq RESTART WITH 1;
-- ALTER SEQUENCE tools_models_id_seq RESTART WITH 1;
-- ALTER SEQUENCE permission_groups_permissions_id_seq RESTART WITH 1;
-- ALTER SEQUENCE purchase_order_tools_id_seq RESTART WITH 1;
-- ALTER SEQUENCE tools_id_seq RESTART WITH 1;
-- ALTER SEQUENCE service_order_tools_id_seq RESTART WITH 1;

-- DROP TABLE service_order_tools;
-- DROP TABLE purchase_order_tools;
-- DROP TABLE permission_groups_permissions;
-- DROP TABLE tools;
-- DROP TABLE tools_models;
-- DROP TABLE service_orders;
-- DROP TABLE purchase_orders;
-- DROP TABLE suppliers;
-- DROP TABLE storages;
-- DROP TABLE permissions;
-- DROP TABLE users;
-- DROP TABLE permission_groups;
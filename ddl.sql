CREATE TABLE storages (
	id serial NOT NULL,
	"name" varchar(50) NOT NULL,
	CONSTRAINT storages_pk PRIMARY KEY (id),
	CONSTRAINT storages_unique UNIQUE (name)
);

CREATE TABLE suppliers (
	id serial NOT NULL,
	"name" varchar(70) NOT NULL,
	cnpj varchar(14) NOT NULL,
	cep varchar(8) NOT NULL,
	CONSTRAINT suppliers_pk PRIMARY KEY (id),
	CONSTRAINT suppliers_unique UNIQUE (name),
	CONSTRAINT suppliers_unique_1 UNIQUE (cnpj)
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
	status varchar(15) DEFAULT 'open'::character varying NOT NULL,
	CONSTRAINT purchase_orders_pk PRIMARY KEY (id),
	CONSTRAINT purchase_orders_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE service_orders (
	id serial NOT NULL,
	id_supplier int NOT NULL,
	status varchar(15) DEFAULT 'open'::character varying NOT NULL,
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
	image varchar(100) NULL,
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
	id_tool_model int NOT NULL,
	state varchar(15) DEFAULT 'new'::character varying NOT NULL,
	batch varchar(30) NOT NULL,
	honing_num int DEFAULT 0 NOT NULL,
	id_storage int NOT NULL,
	CONSTRAINT tools_pk PRIMARY KEY (id),
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
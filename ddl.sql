-- DROP SCHEMA public;

CREATE SCHEMA public AUTHORIZATION pg_database_owner;

-- DROP SEQUENCE public.permissions_id_seq;

CREATE SEQUENCE public.permissions_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.permissions_users_id_seq;

CREATE SEQUENCE public.permissions_users_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.purchase_order_tools_id_seq;

CREATE SEQUENCE public.purchase_order_tools_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.purchase_orders_id_seq;

CREATE SEQUENCE public.purchase_orders_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.service_order_tools_id_seq;

CREATE SEQUENCE public.service_order_tools_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.service_orders_id_seq;

CREATE SEQUENCE public.service_orders_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.storages_id_seq;

CREATE SEQUENCE public.storages_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.suppliers_id_seq;

CREATE SEQUENCE public.suppliers_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.tools_id_seq;

CREATE SEQUENCE public.tools_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.tools_models_id_seq;

CREATE SEQUENCE public.tools_models_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;
-- DROP SEQUENCE public.users_id_seq;

CREATE SEQUENCE public.users_id_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 2147483647
	START 1
	CACHE 1
	NO CYCLE;-- public.permissions definição

-- Drop table

-- DROP TABLE public.permissions;

CREATE TABLE public.permissions (
	id serial4 NOT NULL,
	"name" varchar(100) NOT NULL,
	CONSTRAINT permissions_pk PRIMARY KEY (id)
);


-- public.storages definição

-- Drop table

-- DROP TABLE public.storages;

CREATE TABLE public.storages (
	id serial4 NOT NULL,
	"name" varchar(50) NOT NULL,
	CONSTRAINT storages_pk PRIMARY KEY (id),
	CONSTRAINT storages_unique UNIQUE (name)
);


-- public.suppliers definição

-- Drop table

-- DROP TABLE public.suppliers;

CREATE TABLE public.suppliers (
	id serial4 NOT NULL,
	"name" varchar(70) NOT NULL,
	cnpj varchar(14) NOT NULL,
	cep varchar(8) NOT NULL,
	CONSTRAINT suppliers_pk PRIMARY KEY (id),
	CONSTRAINT suppliers_unique UNIQUE (name),
	CONSTRAINT suppliers_unique_1 UNIQUE (cnpj)
);


-- public.users definição

-- Drop table

-- DROP TABLE public.users;

CREATE TABLE public.users (
	id serial4 NOT NULL,
	"name" varchar(50) NOT NULL,
	login varchar(25) NOT NULL,
	hash varchar(60) NOT NULL,
	CONSTRAINT users_pk PRIMARY KEY (id),
	CONSTRAINT users_unique UNIQUE (login)
);


-- public.permissions_users definição

-- Drop table

-- DROP TABLE public.permissions_users;

CREATE TABLE public.permissions_users (
	id serial4 NOT NULL,
	id_user int4 NOT NULL,
	id_permission int4 NOT NULL,
	CONSTRAINT permissions_users_pk PRIMARY KEY (id),
	CONSTRAINT permissions_users_permissions_fk FOREIGN KEY (id_permission) REFERENCES public.permissions(id) ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT permissions_users_users_fk FOREIGN KEY (id_user) REFERENCES public.users(id) ON DELETE CASCADE ON UPDATE CASCADE
);


-- public.purchase_orders definição

-- Drop table

-- DROP TABLE public.purchase_orders;

CREATE TABLE public.purchase_orders (
	id serial4 NOT NULL,
	id_supplier int4 NOT NULL,
	status varchar(15) DEFAULT 'open'::character varying NOT NULL,
	CONSTRAINT purchase_orders_pk PRIMARY KEY (id),
	CONSTRAINT purchase_orders_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES public.suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);


-- public.service_orders definição

-- Drop table

-- DROP TABLE public.service_orders;

CREATE TABLE public.service_orders (
	id serial4 NOT NULL,
	id_supplier int4 NOT NULL,
	status varchar(15) DEFAULT 'open'::character varying NOT NULL,
	CONSTRAINT service_orders_pk PRIMARY KEY (id),
	CONSTRAINT service_orders_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES public.suppliers(id) ON DELETE RESTRICT ON UPDATE CASCADE
);


-- public.tools_models definição

-- Drop table

-- DROP TABLE public.tools_models;

CREATE TABLE public.tools_models (
	id serial4 NOT NULL,
	code varchar(20) NOT NULL,
	description varchar(100) NOT NULL,
	"family" varchar(15) NOT NULL,
	"usage" varchar(15) NOT NULL,
	id_supplier int4 NOT NULL,
	price money NOT NULL,
	image varchar(100) NULL,
	CONSTRAINT tools_models_pk PRIMARY KEY (id),
	CONSTRAINT tools_models_unique UNIQUE (code),
	CONSTRAINT tools_models_suppliers_fk FOREIGN KEY (id_supplier) REFERENCES public.suppliers(id)
);


-- public.purchase_order_tools definição

-- Drop table

-- DROP TABLE public.purchase_order_tools;

CREATE TABLE public.purchase_order_tools (
	id serial4 NOT NULL,
	id_tool_model int4 NOT NULL,
	id_purchase_order int4 NOT NULL,
	tool_quantity int4 NOT NULL,
	CONSTRAINT purchase_order_tools_pk PRIMARY KEY (id),
	CONSTRAINT purchase_order_tools_purchase_orders_fk FOREIGN KEY (id_purchase_order) REFERENCES public.purchase_orders(id) ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT purchase_order_tools_tools_models_fk FOREIGN KEY (id_tool_model) REFERENCES public.tools_models(id) ON DELETE RESTRICT ON UPDATE CASCADE
);


-- public.tools definição

-- Drop table

-- DROP TABLE public.tools;

CREATE TABLE public.tools (
	id serial4 NOT NULL,
	id_tool_model int4 NOT NULL,
	state varchar(15) DEFAULT 'new'::character varying NOT NULL,
	batch varchar(30) NOT NULL,
	honing_num int4 DEFAULT 0 NOT NULL,
	id_storage int4 NOT NULL,
	CONSTRAINT tools_pk PRIMARY KEY (id),
	CONSTRAINT tools_storages_fk FOREIGN KEY (id_storage) REFERENCES public.storages(id) ON DELETE RESTRICT ON UPDATE CASCADE,
	CONSTRAINT tools_tools_models_fk FOREIGN KEY (id_tool_model) REFERENCES public.tools_models(id) ON DELETE RESTRICT ON UPDATE CASCADE
);


-- public.service_order_tools definição

-- Drop table

-- DROP TABLE public.service_order_tools;

CREATE TABLE public.service_order_tools (
	id serial4 NOT NULL,
	id_tool int4 NOT NULL,
	id_service_order int4 NOT NULL,
	CONSTRAINT service_order_tools_pk PRIMARY KEY (id),
	CONSTRAINT service_order_tools_service_orders_fk FOREIGN KEY (id_service_order) REFERENCES public.service_orders(id) ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT service_order_tools_tools_fk FOREIGN KEY (id_tool) REFERENCES public.tools(id) ON DELETE RESTRICT ON UPDATE CASCADE
);
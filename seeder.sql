-- ============================
-- STORAGES
-- ============================
INSERT INTO storages (name) VALUES
('Central Warehouse'),
('Secondary Warehouse'),
('Tool Room');

-- ============================
-- SUPPLIERS
-- ============================
INSERT INTO suppliers (trade_name, legal_name, cnpj, cep, email, phone) VALUES
('Alpha Tools', 'Alpha Ferramentas LTDA', '12345678000101', '01001000', 'contact@alphatools.com', '1111111111'),
('Beta Supplies', 'Beta Suprimentos LTDA', '12345678000202', '02002000', 'sales@betasupplies.com', '2222222222'),
('Gamma Equip', 'Gamma Equipamentos LTDA', '12345678000303', '03003000', 'support@gammaequip.com', '3333333333'),
('Delta Tools', 'Delta Ferramentas LTDA', '12345678000404', '04004000', 'info@deltatools.com', '4444444444'),
('Epsilon Mach', 'Epsilon Máquinas LTDA', '12345678000505', '05005000', 'service@epsilonmach.com', '5555555555');

-- ============================
-- TOOL MODELS
-- ============================
INSERT INTO tools_models (code, description, "family", "usage", id_supplier, price, image) VALUES
('BR-001', 'Broca de Aço Rápido 10mm', 'Brocas', 'Consumível', 1, '10.00', NULL),
('BR-002', 'Broca de Aço Rápido 12mm', 'Brocas', 'Consumível', 2, '12.00', NULL),
('FR-001', 'Fresa de topo 8mm', 'Fresas', 'Afiável', 1, '25.00', NULL),
('FR-002', 'Fresa de topo 12mm', 'Fresas', 'Afiável', 2, '30.00', NULL),
('CB-001', 'Cabeçote universal', 'Cabecotes', 'Afiável', 3, '150.00', NULL),
('CB-002', 'Cabeçote lateral', 'Cabecotes', 'Afiável', 4, '180.00', NULL),
('IN-001', 'Inserto PCD 10mm', 'Insertos', 'Consumível', 5, '50.00', NULL),
('IN-002', 'Inserto PCD 12mm', 'Insertos', 'Consumível', 1, '55.00', NULL),
('FR-003', 'Fresa de topo 16mm', 'Fresas', 'Afiável', 3, '35.00', NULL),
('BR-003', 'Broca de Aço Rápido 8mm', 'Brocas', 'Consumível', 4, '8.00', NULL);

-- ============================
-- TOOLS
-- ============================
INSERT INTO tools (code, id_tool_model, state, honing_num, id_storage, status) VALUES
('T-001', 1, 'NEW', 0, 1, 'AVAILABLE'),
('T-002', 2, 'NEW', 0, 1, 'AVAILABLE'),
('T-003', 3, 'HONED', 2, 2, 'HONING'),
('T-004', 4, 'NEW', 0, 2, 'AVAILABLE'),
('T-005', 5, 'HONED', 1, 3, 'IN_USE'),
('T-006', 6, 'NEW', 0, 3, 'AVAILABLE'),
('T-007', 7, 'HONED', 3, 1, 'HONING'),
('T-008', 8, 'NEW', 0, 1, 'AVAILABLE'),
('T-009', 9, 'HONED', 2, 2, 'HONING'),
('T-010', 10, 'NEW', 0, 2, 'AVAILABLE'),
('T-011', 1, 'HONED', 1, 1, 'HONING'),
('T-012', 2, 'HONED', 2, 1, 'HONING'),
('T-013', 3, 'HONED', 3, 2, 'HONING'),
('T-014', 4, 'HONED', 2, 2, 'HONING'),
('T-015', 5, 'HONED', 1, 3, 'HONING'),
('T-016', 6, 'HONED', 1, 3, 'HONING'),
('T-017', 7, 'HONED', 2, 1, 'HONING'),
('T-018', 8, 'HONED', 2, 1, 'HONING'),
('T-019', 9, 'HONED', 3, 2, 'HONING'),
('T-020', 10, 'HONED', 2, 2, 'HONING');

-- ============================
-- PURCHASE ORDERS (MOST CLOSED)
-- Dates distributed Jan → Jul 2025
-- ============================
INSERT INTO purchase_orders (id_supplier, status, issued_at, status_updated_at) VALUES
(1, 'CLOSED',     '2025-01-05', '2025-01-15'),
(2, 'CLOSED',     '2025-01-22', '2025-01-30'),
(3, 'CLOSED',     '2025-02-10', '2025-02-18'),
(4, 'CLOSED',     '2025-02-28', '2025-03-05'),
(5, 'CLOSED',     '2025-03-03', '2025-03-10'),
(1, 'OPEN',       '2025-03-25', NULL),
(2, 'CLOSED',     '2025-04-01', '2025-04-08'),
(3, 'CLOSED',     '2025-04-12', '2025-04-18'),
(4, 'OPEN',       '2025-04-25', NULL),
(5, 'CLOSED',     '2025-05-01', '2025-05-08'),
(1, 'CLOSED',     '2025-05-10', '2025-05-20'),
(2, 'OPEN',       '2025-06-01', NULL),
(3, 'CLOSED',     '2025-06-05', '2025-06-15'),
(4, 'CLOSED',     '2025-06-20', '2025-06-28'),
(5, 'CLOSED',     '2025-07-02', '2025-07-10'),
(1, 'OPEN',       '2025-07-12', NULL),
(2, 'CLOSED',     '2025-07-15', '2025-07-25'),
(3, 'CLOSED',     '2025-07-20', '2025-07-31'),
(4, 'CLOSED',     '2025-07-25', '2025-08-02'),
(5, 'OPEN',       '2025-07-30', NULL);

-- ============================
-- SERVICE ORDERS (MOST CLOSED)
-- Dates diversified Jan → Jul 2025
-- ============================
INSERT INTO service_orders (id_supplier, status, issued_at, status_updated_at, price) VALUES
(1, 'CLOSED', '2025-01-04', '2025-01-12', '300.00'),
(2, 'CLOSED', '2025-01-20', '2025-01-29', '450.00'),
(3, 'CLOSED', '2025-02-02', '2025-02-12', '500.00'),
(4, 'OPEN',   '2025-02-18', NULL,          '600.00'),
(5, 'CANCELLED', '2025-02-25', '2025-02-28', '250.00'),
(1, 'CLOSED', '2025-03-10', '2025-03-18', '350.00'),
(2, 'CLOSED', '2025-03-25', '2025-03-30', '400.00'),
(3, 'OPEN',   '2025-04-05', NULL,          '550.00'),
(4, 'CLOSED', '2025-04-20', '2025-04-28', '700.00'),
(5, 'CLOSED', '2025-04-30', '2025-05-05', '200.00'),
(1, 'CLOSED', '2025-05-10', '2025-05-18', '320.00'),
(2, 'OPEN',   '2025-05-20', NULL,          '480.00'),
(3, 'CLOSED', '2025-06-02', '2025-06-10', '530.00'),
(4, 'CLOSED', '2025-06-15', '2025-06-22', '620.00'),
(5, 'CANCELLED', '2025-06-28', '2025-07-02', '260.00'),
(1, 'CLOSED', '2025-07-05', '2025-07-10', '370.00'),
(2, 'CLOSED', '2025-07-10', '2025-07-18', '410.00'),
(3, 'OPEN',   '2025-07-20', NULL,          '560.00'),
(4, 'CLOSED', '2025-07-25', '2025-07-30', '710.00'),
(5, 'OPEN',   '2025-07-28', NULL,          '220.00');

-- ============================
-- PURCHASE ORDER TOOLS
-- ============================
INSERT INTO purchase_order_tools (id_tool_model, id_purchase_order, tool_quantity) VALUES
(1, 1, 10),
(2, 2, 15),
(3, 3, 5),
(4, 4, 7),
(5, 5, 2),
(6, 6, 3),
(7, 7, 6),
(8, 8, 8),
(9, 9, 4),
(10, 10, 9),
(1, 11, 12),
(2, 12, 14),
(3, 13, 6),
(4, 14, 5),
(5, 15, 3),
(6, 16, 4),
(7, 17, 7),
(8, 18, 8),
(9, 19, 5),
(10, 20, 10);

-- ============================
-- SERVICE ORDER TOOLS
-- ============================
INSERT INTO service_order_tools (id_tool, id_service_order) VALUES
(3, 1),
(7, 2),
(9, 3),
(3, 4),
(7, 5),
(9, 6),
(3, 7),
(7, 8),
(9, 9),
(3, 10),
(7, 11),
(9, 12),
(3, 13),
(7, 14),
(9, 15),
(3, 16),
(7, 17),
(9, 18),
(3, 19),
(7, 20);
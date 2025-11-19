# MTTools

Um sistema de gerenciamento de estoque voltado para ferramentas de usinagem.

## Principais funcionalidades

- Cadastro de usuários
- Controle de permissões através de grupos
- Cadastro de fornecedores
- Cadastro de ferramentas com vinculação de fornecedor
- Cadastro de estoques
- Visualizar unidades dentro do estoque, contendo informações sobre a disponibilidade e o estado da ferramenta.
- Movimentar unidades para outros estoques.
- Emitir ordens de compra e ordens de serviços.

## Tecnologias utilizadas

- Delphi 12
- PostgreSQL
- [Biblioteca de BCRYPT para Delphi](https://github.com/JackTrapper/bcrypt-for-delphi)
- API de CNPJ: [CNPJá](https://cnpja.com/api/open)

## Estrutura do projeto

```
├── assets/          → Imagens utilizadas no projeto
├── docs/            → Documentação
├── lib/             → Dependências externas
└── src/             → Código-fonte
    ├── controller/  → Camada de controle: regras de negócio e comunicação entre view e repository
    ├── DTO/         → Objetos de Transferência de Dados (Data Transfer Objects)
    ├── infra/       → Código de infraestrutura (ex: conexão com banco, logger)
    ├── model/       → Modelos da aplicação (entidades e lógica de domínio)
    ├── repository/  → Camada de persistência: Comunica com o banco de dados
    ├── util/        → Classes ajudantes usadas em várias camadas
    └── view/        → Telas

```

## Banco de dados

![Diagrama de entidade relacionamento](docs/imagens/DER.png)

```
- users             → Usuários
- permission_groups → Grupos de permissões
- permissions       → Permissões
- storages          → Estoques
- suppliers         → Fornecedores
- tools_models      → Modelos de ferramenta
- tools             → Unidades de ferramentas
- purchase_orders   → Ordens de compras
- service_orders    → Ordens de serviços
```
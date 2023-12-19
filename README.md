<p align="center">
  <a href="http://marcosnascimento.vercel.app/" target="blank"><img src="https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702862537/logo.mcine_mjupbm.svg" width="200" alt="mcine Logo" /></a>
</p>

## Descrição

Aplicação backend multi tenancy para gestão de cinemas desenvolvida como desafio técnico para uma vaga de desenvolvedor fullstack da empresa Jetimob com prazo de 15 dias de entrega. 
A arquitetura escolhida foi "Schema Based Multi Tenancy", na qual cada cinema (tenant) cadastrado tem seu próprio schema dentro do banco de dados, garantindo a independência e a segurança dos dados entre os tenants. 
O tenant "Root" é responsável por gerenciar os tenants filhos, podendo cadastrar cinemas, filmes, usuários e vinculá-los com os cinemas. 
A única informação disponível globalmente entre o tenant Raiz e o os tenant's filhos são os filmes disponíveis ao cadastrar as sessões.

- Stacks utilizadas:
	* PostgreSQL
	* Typescript
	* NestJS
	* Prisma
	* DTO's - Class Validator

 - Link para o Workspace no Postman com a documentação e exemplos da API
   * https://app.getpostman.com/join-team?invite_code=629684de949abc7ee95dd5949d22231e&target_code=04a30a178473354fac53b20992577a15
   
# Instalação

## Banco de Dados
- Dados locais utilizados:
	* host: localhost:5432 
	* username: postgres
	* password: postgres

- Criar um novo banco de dados PostgreSQL com nome "mcinedb"

    ```bash
    $ createdb -U postgres mcinedb
    ```
			
- Importar o backup do banco em "PastaDoProjeto"/db/db_backup.sql
  
    ```bash
    $ psql -U postgres -d mcinedb -f db_backup.sql
    ```
		
- Configurar a varíavel de ambiente "DATABASE_URL" para conexão do banco com o prisma:	
	* Modelo de URL da documentação: 
		* DATABASE_URL=postgresql://USER:PASSWORD@HOST:PORT/DATABASE?schema=SCHEMA

	* URL para os parâmetros informados acima:
		* DATABASE_URL="postgresql://postgres:postgres@localhost:5432/mcinedb?schema=public"
	
	PS: Se utiliza outro usuário e senha deve subistituir conforme o padrão da documentação.

## Aplicação
  - Fazer download do repositório
	- Abrir com o terminal
	- Instalar as dependências
    
	```bash
	$ yarn install
	```
  
	- Certifique-se de ter todas as 4 váriaveis de ambiente no arquivo .env:
	*  Database String URL for Prisma
		- DATABASE_URL="postgresql://postgres:postgres@localhost:5432/mcinedb?schema=public"

	* Root tenant identifier
		- ROOT_TENANT_IDENTIFIER="root"

	* JWT Sign Keys
		- JWT_SECRET_KEY="oZkGbnpOm8xpTU1y4Oa2ge6TPlEihZJWHyA/tWgU1GbvGz1Kk4S09gKQxyCKCMSy8D6kdqsPI8cmOQSzbWi7ZA=="
		- JWT_REFRESH_TOKEN_KEY="y1gdEYq9GtMlLrMY/nX1bjwn+Hmkag7oZiv6ZBlaQJwI64Q2Q7Gse1kpzmZECTsxbmfllVgbaNjqkbwt2/K/Ow=="
	
	- Executar a aplicação em Produção (Porta 8000)
    
	```bash
	$ yarn start:prod
	```


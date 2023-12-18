<p align="center">
  <a href="http://marcosnascimento.vercel.app/" target="blank"><img src="https://res.cloudinary.com/dpmbuqjqj/image/upload/v1702862537/logo.mcine_mjupbm.svg" width="200" alt="mcine Logo" /></a>
</p>

## Descrição

Aplicação backend multi tenancy para gestão de cinemas. 
A arquitetura escolhida foi "Schema Based Multi Tenancy", na qual cada cinema (tenant) cadastrado tem seu próprio schema dentro do banco de dados, garantindo a independência e a segurança dos dados entre os tenants. 
O tenant "Root" é responsável por gerenciar os tenants filhos, podendo cadastrar filmes, usuários e vinculá-los com os cinemas. 
A única informação disponível globalmente entre o tenant Raiz e o os tenant's filhos são os filmes disponíveis ao cadastrar as sessões.

- Stacks utilizadas:
	* PostgreSQL
	* Typescript
	* NestJS
	* Prisma
	* DTO's - Class Validator
 
## Instalação

# Banco de Dados
- Dados locais utilizados:
	* host: localhost:5432 
	* username: postgres
	* password: postgres
		
	 * 1. Criar um novo banco de dados PostgreSQL com nome "mcinedb"
	    ```bash
	    $ createdb -U postgres mcinedb
	    ```
			
	* 2. Importar o backup do banco em "PastaDoProjeto"/db/db_backup.sql
	    ```bash
	    $ psql -U postgres -d mcinedb -f db_backup.sql
	    ```
		
	* 3. Configurar a varíavel de ambiente "DATABASE_URL" para conexão com o prisma:	
	- Modelo de URL da documentação: 
		DATABASE_URL=postgresql://USER:PASSWORD@HOST:PORT/DATABASE?schema=SCHEMA
	
	- URL para os parâmetros informados acima:
		DATABASE_URL="postgresql://postgres:postgres@localhost:5432/mcinedb?schema=public"
	
	PS: Se utiliza outro usuário e senha deve subistituir conforme o padrão da documentação.

```bash
$ yarn install
```

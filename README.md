# PetAssistant - Yesod Web App
> *Nota: Esse projeto foi desenvolvido integralmente na plataforma Windows, podem haver falhas e bugs caso a execução seja realizada em outro SO.*

## Sobre o projeto
Este projeto foi desenvolvido utilizando Haskell, HTML e CSS como base de código para a matéria Tópicos Especiais em Informática da FATEC Rubens Lara.

Trata-se de uma platafora web utilizando Yesod focada no agendamento de consultas veterinárias realizadas por membros cadastrados e diagnósticos que são retornados aos usuários através de um membro com acessos elevados.

O usuário padrão pode realizar requisições POST para a tabela "pets" e GET para as tabelas "vets" e "consulta".

O usuário admin pode realizar requisições GET, POST, UPDATE e DELETE para as tabela "pets" e "vets" e somente GET e POST na tabela "consulta" que é composta de um INNER JOIN das tabelas "usuario" e "pets".

## Acessos para teste
A aplicação possui três perfis de acesso/visualização da plataforma: deslogado, logado-padrão e logado-admin.
* Deslogado
	* Acesso básico às páginas de introdução da plataforma, permite navegar em poucas páginas, realizar o cadastro de um novo email e realizar login.
* Logado-padrão
	* Realizar cadastro via platafora. Este login permite o acesso à maioria das páginas e botões de interação básicos do site.
* Logado-admin
	* Realizar login com acesso elevado `admin@admin.com / 123`. Este login permite acesso total a qualquer página da aplicação e visualização diferente dos botões de interação do site.

## Iniciando
Para o clonar, realizar a build e execução local do projeto é necessário ter instalado o [Github CLI](https://github.com/cli/cli), [Stack](https://haskell-lang.org/get-started) e o banco de dados [PostgreSQL](https://www.postgresql.org/download/windows/).

## Passo a passo
1. Clonar este repositório
```
git clone https://github.com/pereira-renan/p2-yesod.git
```
2. Entrar no repositório via CLI
```
cd p2-yesod
```
3. Instalar o GHC da build:
	* Este processo leva algum tempo.
```
stack setup
```
4. Compilar as bibliotecas do projeto:
	* O tempo de build varia e alguns alertas serão mostrados no prompt durante a execução
```	
stack build
```
5. Executar a build do projeto:
```
stack exec petparty
```
6. Abrir o projeto em seu navegador de preferência através do endereço [http://localhost:8080/](http://localhost:8080/).
	
## Bugs conhecidos

* É retornado um erro de SQL em tela ao deletar um pet que possui um registro ativo na tabela "consulta".

## Implementações futuras

* Requisições de UPDATE e DELETE para a tabela "consulta"
* Atualizar telas de retorno das requisições 403.

## Contribuindo ao projeto

Para contribuir à este projeto, siga estes passos:

1. Fork este repositório.
2. Crie uma nova branch: `git checkout -b <nome_branch>
3. Faça suas alterações e as commit: `git commit -m 'resumo do commit'`
4. Faça um push das suas alterações para a branch original: `git push origin <nome_projeto>/<localização>
5. Crie um pull request.

## Contribuidores

Agradeço aos colegas abaixo pelas contribuições ao projeto:
* @Wisinewski :computer:
* @morais-mateus :computer:
* @RobertoDSN :computer:

## Documentação de apoio
* [Yesod Book (em inglês)](https://www.yesodweb.com/book)
* [Haskell Uma introdução à programação funcional - por Alexandre Garcia de Oliveira (em português)](https://www.casadocodigo.com.br/products/livro-haskell)
* [Yesod e Haskell Aplicações web com Programação Funcional pura - por Alexandre Garcia de Oliveira, Patrick Augusto da Silva, Felipe Cannarozzo Lourenço (em português)](https://www.casadocodigo.com.br/products/livro-yesod-haskell)

## Licenciamento

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

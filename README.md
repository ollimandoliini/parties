# Parties

A simple web application for creating events, invites and keeping track of who will be participating.

https://invitee.to/

## Local development

### Requirements

- Node v16.13.0, npm v8.1.0
- Haskell Stack v2.7.5
- PostgreSQL 13 *OR* Docker 20.10
- Terraform v1.1.7

### Setup

#### Database

```bash
docker run --name parties \
            -d -e POSTGRES_USER=user \
            -e POSTGRES_PASSWORD=password \
            -e POSTGRES_DB=parties \
            -p 5432:5432 \
            postgres:13 postgres -N 500
```

#### Backend

```bash
cd backend
stack repl
>> main

# OR

cd backend
stack build
```

#### Frontend

```bash
cd frontend
npm install
npm start
```


## Misc

#### Generating TypeScript types from Haskell types

```bash
./generate-types.sh
```

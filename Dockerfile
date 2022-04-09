## BACKEND
FROM haskell:9.0.2 as build

RUN apt-get update && \
    apt-get install --yes libpq-dev

RUN mkdir /opt/build
WORKDIR /opt/build

COPY ./backend/stack.yaml ./backend/package.yaml ./backend/stack.yaml.lock ./
RUN stack build --dependencies-only

WORKDIR /opt/build
COPY /backend .
RUN stack build
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

## FRONTEND
FROM node:16-alpine AS frontend
WORKDIR /frontend

COPY ./frontend/package.json .
COPY ./frontend/package-lock.json .
RUN npm install

COPY ./frontend/ .
RUN npm run build


## DEPLOY
FROM debian:buster as deploy

RUN apt-get update && \
    apt-get install --yes libpq-dev ca-certificates libnuma-dev

RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY --from=build /opt/build/bin .
COPY --from=build /opt/build/migrations/ ./migrations
COPY --from=frontend /frontend/build ./frontend

# Execute your app 
CMD ["/opt/app/parties-backend-exe", "--verbose"]

FROM node:22.16.0-alpine3.22

WORKDIR app/

RUN npm install -g elm@0.19.1-6 --unsafe-perm=true -–allow-root

ENTRYPOINT elm reactor
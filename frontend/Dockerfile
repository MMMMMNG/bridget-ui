FROM node:22.16.0-alpine3.22

WORKDIR app/

RUN npm install -g elm@0.19.1-6 --unsafe-perm=true -–allow-root

COPY . .
#TODO: dont use elm reactor as the prod server lol. It would probably be best to use the webpack plugin for elm to bundle
#      everything and then do a multi stage build with nginx. And then also rerout only the /move/ path to the
#      backend container and not expose any ports of the backend container. There's no time rn for such negligibilities.
ENTRYPOINT elm reactor
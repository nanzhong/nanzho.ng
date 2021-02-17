FROM silex/emacs:27.1 AS build
ARG BASE_URL

RUN curl -sL https://deb.nodesource.com/setup_15.x | bash -
RUN apt-get install -y nodejs nginx
RUN npm install -g sass

WORKDIR /workspace
COPY . .
RUN sass --style=compressed theme/static/scss:theme/static/css

RUN mkdir -p output=
RUN emacs --script publish.el
RUN find posts -type f | grep -v .org | xargs -i cp '{}' 'output/{}'

ENTRYPOINT ["bash"]

FROM nginx:1.19
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /workspace/output /var/www/html
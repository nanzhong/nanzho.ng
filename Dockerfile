FROM silex/emacs:27.2 AS build
ARG BASE_URL

ENTRYPOINT ["bash"]

RUN curl -sL https://deb.nodesource.com/setup_16.x | bash -
RUN apt-get install -y nodejs nginx git build-essential
RUN npm install -g sass

WORKDIR /workspace

COPY init.el .emacs /workspace/
RUN emacs --script init.el

COPY . .
RUN sass --style=compressed theme/static/scss:theme/static/css
RUN mkdir -p output
RUN emacs --script export.el
RUN find posts -type f | grep -v .org | xargs -i cp '{}' 'output/{}'

FROM nginx:1.21
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /workspace/output /var/www/html
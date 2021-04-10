FROM nginx:alpine

# replace default.conf with our exciting nginx.conf 
COPY ./nginx.conf /etc/nginx/conf.d/default.conf

COPY ./build/. /usr/share/nginx/html/

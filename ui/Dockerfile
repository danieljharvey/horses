# Stage 1 - the build process
FROM node:20 as build-deps

ARG REACT_APP_MIMSA_API_URL
ARG REACT_APP_ROUTER_API_URL
 
WORKDIR /usr/src/app
COPY package.json yarn.lock ./ 
RUN yarn

COPY . ./
RUN REACT_APP_ROUTER_API_URL=${REACT_APP_ROUTER_API_URL} REACT_APP_MIMSA_API_URL=${REACT_APP_MIMSA_API_URL} yarn build

# Stage 2 - the production environment
FROM nginx:1.25-alpine

# replace default.conf with our exciting nginx.conf 
COPY ./nginx.conf /etc/nginx/conf.d/default.conf



COPY --from=build-deps /usr/src/app/build /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]

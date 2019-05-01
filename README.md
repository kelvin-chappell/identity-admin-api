# identity-admin-api

# Application configuration

Configuration files:
- Environment-specific configuration (`identity-admin-api/conf/<ENV>.conf`)
- Application configuration (`identity-admin-api/conf/application.conf`)
- System file with additional properties (`/etc/gu/identity-admin-api.conf`)

# Setting up Identity Admin locally

## Nginx setup

Clone [identity-platform](https://github.com/guardian/identity-platform) and follow its [README](https://github.com/guardian/identity-platform/blob/master/README.md#setup-nginx-for-local-development)

## Database setup

Follow the [Docker PostgreSQL Readme](https://github.com/guardian/identity-platform/blob/master/postgres/readme.md) to install and start up a database in a local Docker container.  If successful the database service should be listening on port 5555. 

## Running the Application

```
sbt devrun
```

# Hitting the API with curl

The API requires requests to have an authorization header to hit the API. A hmac token must be generated to be put in the header. This can be done in SBT

## Generating the authorization token

```
sbt
runMain util.HmacGenerator "*date*" "*uri to hit*" "*hmac secret value*"
```
This will generate a hmac token

### Example
```
sbt
runMain util.HmacGenerator "2015-10-26T18:13:04Z" "/v1/user/11111111" "secret"
```

## Curl

```
curl --header "Authorization: HMAC *hmac-token*" --header "Date: *date*" http://localhost:9500/*uri to hit*
```
Date must match the one used to generate the token and be in the form YYYY-MM-DDTHH:MM:SSZ.

### Example
```
curl --header "Authorization: HMAC 334ddfgdfgk4rt[q" --header "Date: 2015-10-26T18:13:04Z" http://localhost:9500/v1/user/11111111
```



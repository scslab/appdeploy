# TODO

## AppDeployer

1. Remove global hashtables (maybe use ReaderT)

2. Proactively notify controller of changes in app states
  * When app dies

## AppController

1. Remove global hashtables

2. Keep appht in sync with deployers

3. Handle restarts by filling in appht with already running apps by talking to deployers

## Nginx

1. Load balancing (http://wiki.nginx.org/LoadBalanceExample)

2. Reloading config through UNIX signal (SIGHUP) -- `kill -1 $NGINX_PID`

## Misc.

1. Heroku style client-side app deployment (buildpacks, command-line client, compile server, shared file-system with appcontroller, etc....)


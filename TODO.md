# TODO

## AppDeployer

1. Remove global hashtables (maybe use ReaderT)

2. Assign apps an actual name other than the shell command

## AppController

1. Remove global hashtables

2. Handle restarts by filling in appht with already running apps by talking to deployers

## Nginx

1. Load balancing (http://wiki.nginx.org/LoadBalanceExample)

## Misc.

1. Heroku style client-side app deployment (buildpacks, command-line client, compile server, shared file-system with appcontroller, etc....)


## Useful commands

SBT:
```
all/clean: scanimationJVM/clean scanimationJS/clean
jvm/build: scanimationJVM/clean scanimationJVM/compile scanimationJVM/assembly
js/build:  scanimationJS/clean  scanimationJS/compile  scanimationJS/fastOptJS nodeJS moveJS
js/deploy: scanimationJS/clean  scanimationJS/compile  scanimationJS/fullOptJS nodeJS moveJS pushJS
```

Server:
```
```

Heroku:
```
heroku container:login
docker push registry.heroku.com/wispy-scanimation/web
heroku container:release web --app wispy-scanimation
heroku open --app wispy-scanimation
heroku logs --tail --app wispy-scanimation
```
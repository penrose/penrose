start-dev:
	docker-compose -f docker-compose-dev.yml up

recreate-dev:
	docker-compose -f docker-compose-dev.yml build

shell-dev:
	docker-compose -f docker-compose-dev.yml exec penrose /bin/bash

dev-build:
	docker-compose -f docker-compose-dev.yml exec penrose sh -c "stack setup && stack build"
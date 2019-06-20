start-dev:
	docker-compose -f docker-compose-dev.yml up -d

stop-dev:
	docker-compose -f docker-compose-dev.yml stop

recreate-dev:
	docker-compose -f docker-compose-dev.yml build

shell-dev:
	docker-compose -f docker-compose-dev.yml exec penrose /bin/bash

penrose:
	docker-compose -f docker-compose-dev.yml exec penrose sh -c "$(MAKECMDGOALS)"

dev-build:
	docker-compose -f docker-compose-dev.yml exec penrose sh -c "stack setup --allow-different-user && stack install --allow-different-user"

attach-renderer:
	docker attach penrose_renderer_1


build-base:
	docker build -t penrose-base -f Dockerfile.base .

build-stack-native: build-base 
	stack docker pull
	stack --stack-yaml stack-native.yaml build
	stack --stack-yaml stack-native.yaml image container

run-stack-native:
	docker run penrose 
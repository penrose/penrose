build-local:
	stack build

build-base:
	docker build -t penrose-base -f Dockerfile.base .

build-stack-native: build-base 
	stack docker pull
	stack --stack-yaml stack-native.yaml build 
	stack --stack-yaml stack-native.yaml image container

run-stack-native:
	docker run -it --rm -p 3939:3939 -p 9160:9160 penrose 
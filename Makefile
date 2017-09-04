AWS_ACCOUNT_ID=$(shell aws sts get-caller-identity --output text --query 'Account')

default:
	# Build image
	stack build
	mkdir -p bin
	cp $(shell stack exec which fugacious-server) bin
	cp $(shell stack exec which fugacious-sendmail) bin
	docker build -t fugacious .

deploy:
	# Log into ECR and create repository
	$(shell aws ecr get-login --region us-east-1 --no-include-email)
	-aws ecr create-repository --repository-name fugacious
	docker tag fugacious:latest $(AWS_ACCOUNT_ID).dkr.ecr.us-east-1.amazonaws.com/fugacious:latest
	docker push $(AWS_ACCOUNT_ID).dkr.ecr.us-east-1.amazonaws.com/fugacious:latest

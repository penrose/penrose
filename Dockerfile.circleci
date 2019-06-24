FROM fpco/stack-build:lts-13.24

RUN mkdir -p /opt/penrose
COPY . /opt/penrose/
WORKDIR /opt/penrose

ENTRYPOINT ["./penrose"]
EXPOSE 9160
CMD ["--domain=0.0.0.0","editor"]

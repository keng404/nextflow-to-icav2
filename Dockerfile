FROM r-base:4.0.3
RUN apt-get update -y && \
    apt-get install -y curl libxml2-dev libssl-dev libcurl4-openssl-dev python-dev-is-python3
### copy in scripts
COPY *.R /usr/local/bin/
### install R packages 
RUN Rscript /usr/local/bin/install_packages.R
### install ica CLI
RUN wget -O /usr/local/bin/icav2 "https://stratus-documentation-us-east-1-public.s3.amazonaws.com/cli/2.2.0/linux/icav2"  && \
    chmod u+x /usr/local/bin/icav2
#### for automating CLI setup
RUN apt-get install -y expect
COPY *exp /usr/local/bin/
WORKDIR /usr/local/bin
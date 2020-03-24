# Based OpenCPU Dockerfile
FROM opencpu/base

# Copy all files into the container
WORKDIR /usr/local/src
COPY . /usr/local/src/app

# Install app as an R package on the server
RUN tar czf /tmp/univarejcrr.api.tar.gz app/ \
	&& /usr/bin/R CMD INSTALL /tmp/univarejcrr.api.tar.gz

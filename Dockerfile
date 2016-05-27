
FROM fpco/stack-build:lts-5.18

RUN apt-get update --yes
RUN apt-get install --yes git unzip libssl-dev libfile-slurp-perl libipc-run-perl libicu-dev

ADD ./ /gipeda

RUN cd /gipeda && \
    stack install --install-ghc --local-bin-path=/usr/bin && \
    cp -a install-jslibs.sh /usr/bin && \
    rm -rf /gipeda

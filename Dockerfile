FROM rocker/shiny:3.6.1

WORKDIR /srv/shiny-server

RUN apt-get update \
    && apt-get install -y libsasl2-dev libssl-dev libtiff5-dev libv8-dev

RUN echo \
   'options(repos=list(CRAN="https://cloud.r-project.org/"))' > \
   ".Rprofile"

RUN R -e "install.packages(c('shinyWidgets','imager','shinyjs','ggplot2'))"

ADD https://raw.githubusercontent.com/rocker-org/shiny/master/shiny-server.sh /usr/bin/

EXPOSE 3838

RUN chgrp -R shiny /srv/shiny-server

RUN chmod +x /usr/bin/shiny-server.sh

CMD /usr/bin/shiny-server.sh


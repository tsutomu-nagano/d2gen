FROM rocker/tidyverse:4

RUN install2.r --error --deps TRUE \
        tidyxl \
        argparse \
        DT

COPY R/ /home/d2gen/R/
COPY entry.R /home/d2gen/entry.R

WORKDIR /home/d2gen

ENTRYPOINT ["Rscript", "entry.R"]

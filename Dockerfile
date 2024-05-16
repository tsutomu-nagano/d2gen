FROM rocker/tidyverse:4

RUN install2.r --error --deps TRUE \
        tidyxl
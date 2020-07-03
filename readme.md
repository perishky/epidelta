# EpiDelta: Epigenome-wide change and variation in DNA methylation in childhood

An interactive web application 
for exploring DNA methylation change in childhood
using data generated for participants in 
[ALSPAC](http://www.bristol.ac.uk/alspac/)
and [Generation R](https://generationr.org.uk/).

> Rosa H. Mulder, Alexander Neumann, Charlotte A. M. Cecil, Esther
> Walton, Lotte C. Houtepen, Andrew J. Simpkin, Jolien Rijlaarsdam,
> Bastiaan T. Heijmans, Tom R. Gaunt, Janine F. Felix, Vincent
> W. V. Jaddoe, Marian J. Bakermans-Kranenburg, Henning Tiemeier,
> Caroline L. Relton, Marinus H. van IJzendoorn, Matthew Suderman.
> [Epigenome-wide change and variation in DNA methylation from birth to late adolescence](https://doi.org/10.1101/2020.06.09.142620)
> bioRxiv 2020.06.09.142620; doi: https://doi.org/10.1101/2020.06.09.142620

The app is implemented in R
using [Shiny](https://shiny.rstudio.com/).

## Installing the app

The app is designed to run as a docker container.
[Docker](https://www.docker.com/) will need to be installed.

Build the docker container. This may take a few minutes,
mainly to install R packages.
```
docker-compose build
```

Start the app:
```
docker-compose up -d 
```
Omit '-d' unless the app is ready for production.

View the app in a web browser at `http://localhost:3405/`.

## Image plots and data

The app displays images found in `app/plots` and data in `app/data`.
These are not provided on github because of space requirements;
there are over 900K plots and the data uses 180Mb. 

## Debugging the container and the app

To get a bash session in the running docker container:

```
docker exec -it epidelta_prod bash
```

Once in, it is possible to run basic linux commands
and run R.

To debug a Dockerfile, comment out the offending line
and every line after it.
Then, rebuild and restart the container,
initiate a bash session in the container
and then investigate the problematic command.

## Stopping the app

To stop the app:
```
docker-compose stop
```

Docker container output (for debugging purposes)
is saved in the `log/` directory.


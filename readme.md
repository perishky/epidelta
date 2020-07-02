# EpiDelta: Epigenome-wide change and variation in DNA methylation in childhood

An interactive web application 
for exploring DNA methylation change in childhood
using data generated for participants in 
[ALSPAC](http://www.bristol.ac.uk/alspac/)
and [Generation R](https://generationr.org.uk/).

The app is implemented in R
using [Shiny](https://shiny.rstudio.com/).

## Running the app

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

View the app in a web browser at `http://localhost:3401/`.

## Debugging the container and the app

To get a bash session in the running docker container:

```
docker exec -it epidelta bash
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


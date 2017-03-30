[![DOI](https://zenodo.org/badge/65920602.svg)](https://zenodo.org/badge/latestdoi/65920602)


## Instructions for setting up the benchmark

1. Installation
     1. Create an OpenML account, and generate an API key
     2. Install Github
     3. Optional : set up Docker as presented below


2. Open main.R
You should be able to launch everything from here.
	* Enter your OpenML API key at the beginning of the file so that you will be able to download data from OpenML
	* Enter the number of cores you want to use for the benchmark experiment


2. In your rstudio instance
     1. Open main.R, enter your API key and the number of cores you want tu use at the specified place in the file
     2. Get the data from OpenML and optionally compute an estimate of the training time (1-2 days)
     3. Do the benchmark (for 278 datasets, it takes around 8 hours with 7 cores and 8go RAM)
     4. Visualize the results
     5. Additional Simulations and computations






## Instructions for Setting up Docker

1. Install Docker
More information can be found on the [Docker Website](https://docs.docker.com/engine/installation/)

2. Set up Docker
	* Change the default docker parameters
	You might have to change the default parameters for your docker machine, such as the number of cpus and RAM that you decide to allow for a container.  
	This parameters can be found in the graphic interface, or via the command line, such as 

	%% Remove the default docker machine parameters
	> docker-machine rm default 
	%% Create new new default parameters for the docker machine, for example with 16gb RAM, 8 Cpus and 20Gb Hard-drive.
    > docker-machine create -d virtualbox --virtualbox-memory=16000 --virtualbox-cpu-count=8 --virtualbox-disk-size=20000 default


3. Get the Docker image associated with the benchmark
	The Docker image can be found on [DockerHub](https://hub.docker.com/r/shadoko/docker_java_packages/)..
	You can pull the image (around 2.4gb) to your system via the command line :
	> docker pull shadoko/docker_java_packages:version3

	Note that in DockerHub the Dockerbuild file was given so that the image can be recomputed. However, some dependancies may have changed since the version used for the benchmark, so we recommand using a fixed version of the image: shadoko/docker_java_packages:version3

4. Generate a Rstudio instance that you can connect to
    > docker run --rm -p 8787:8787 -v /Users/myname/myFolder:/home/rstudio/Docker-Benchmark/ shadoko/docker_java_packages:version3

    -- rm indicates that the container will be deleted when stopped
    -p 8787:8787 indicates that the Rstudio instance will be available on port 8787
    -v /myComputerPath/:/myContainerPath/ link a volume from your computer to your container VM, so that you can for example open your R project
    shadoko/docker_java_packages:version3  refers to the docker image you want to create a container from

    Here you should link your docker container with a folder containing the GitHub project.

	Note : for windows OS syntax is different, and the User Public is recommended for rights issues /c/Users/Public/MyFolder:/home/rstudio/Project 


5. Connect to your Rstudio instance
	1. Check the IP of your computer
	2. In your browser enter http://myContainerIP:8787 and sign in with id=rstudio and password=rstudio


6. 
	Close the container. In the command line use ctrl+c to close the container.
	Check that no container are running with the command :  
	> docker ps




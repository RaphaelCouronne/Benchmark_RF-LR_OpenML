[![DOI](https://zenodo.org/badge/65920602.svg)](https://zenodo.org/badge/latestdoi/65920602)


## Instructions for launching the benchmark

#### 1. Installation
1. Create an OpenML account, and generate an API key
2. Get the GitHub code [here](https://github.com/RaphaelCouronne/IBE_Benchmark-OpenML)
3. Optional : **set up Docker as presented below, for a reproducible environment**


#### 2. Set up main.R
1. Open main.R 
2. Enter your OpenML API key at the beginning of the file so that you will be able to download data from OpenML
3. Enter the number of cores you want to use for the benchmark experiment  

		# Enter below nCores and myapikey  
		nCores = ??? # number of Cpus you want to use   
		myapikey = "??????????????????????????????????" # OpenML API key  
		saveOMLConfig(apikey = myapikey, arff.reader = "RWeka", overwrite=TRUE)  

#### 3. Use main.R

Make sure you have all the required package if you do not use our Docker image.  
For each of the subsections (e.g. 1.1), you have to run all the code.  
For a more practical use, the results are already present in the GitHub in the folder Data. Thus, if you want only to generate the graphics and simulations you can skip parts 1. and 2.  
Graphics will be saved in Data/Pictures/

1. Benchmark Study
	1. Get the Data from OpenML  
Note that a fixed list of OpenML tasks is used here, so that we work with a fixed set of datasets. We first remove all the datasets that do not fit our criteria (binary classification problem, no NAs, no High dimension, no simulated datasets, no duplicates). We then use the file "Data/OpenML/df.infos.RData" to remove the dataset which failed to load. If you want to recompute this file, you can set the option force=TRUE. Computations should then last for several hours.

	2. Launch the benchmark  
You can here recompute the benchmark using batchtools. The function setBatchtoolsExperiment() will clear the current batchtools folder and prepare R for a new benchmark computation. You can then use the batchtools function submitJobs to compute the results for the datasets. getStatus() will help monitor the computation. For 278 datasets, it took around 8 hours with 7 i7 cores and 8go RAM.
2. Visualization of the results
	1. Convert the results  
Results are converted to a dataframe.
	2. Overall visualization  
Barplot of ranks is plotted, as well as boxplot of the performance and difference of performance for acc, auc and brier measures.
	3. Inclusion Criteria visualization  
We visualize the boxplot of difference considering different subgroups according to the values of the meta-features such as p and n.  
  
3. Analysis of the results  
	1. Overall results  
We present here the mean, standard deviation and boostrap confidence interval of the results, as well as power of the test.
	2. Meta-Learning  
Partial dependance plot of the model trained to predict the difference of performance between RF and LR based on the values of the meta-features.

4. Simulations
	1. Subset simulation
Computation of the performance of LR and RF for many sub-datasets of the OpenML dataset with id 1496. Sub-datasets are randomly generated according to subset of p0<p features or n0<n observations. We then visualize the dependancy of the difference between RF and LR according to increasing values of p0 and n0.
	2. Partial dependance plot simulation  
Computation of simple examples for partial dependance.
	3. Computation of the difference in partial dependance
Computation of the difference in partial dependance between RF and LR for all the 278 datasets we considered for this study. Computation may be very time expensive.




## Instructions for Setting up Docker

#### 1. Install Docker
More information can be found on the [Docker Website](https://docs.docker.com/engine/installation/)

#### 2. Set up Docker

##### Change the default docker parameters
You might have to change the default parameters for your docker machine, such as the number of Cpus and RAM that you decide to allow for a container. This parameters can be found in the graphic interface, or via the command line, such as :

	# Remove the default docker machine parameters  
	> docker-machine rm default   
	# Create new new default parameters for the docker machine, for example with 16gb RAM, 8 Cpus and 20Gb Hard-drive.  
	> docker-machine create -d virtualbox --virtualbox-memory=16000 --virtualbox-cpu-count=8 --virtualbox-disk-size=20000 default  


#### 3. Get the Docker image associated with the benchmark
The Docker image can be found on [DockerHub](https://hub.docker.com/r/shadoko/docker_java_packages/). You can pull the image (around 2.4gb) to your system via the command line :

	> docker pull shadoko/docker_java_packages:version3

Note that in DockerHub the Dockerbuild file was given so that the image can be recomputed. However, some dependancies may have changed since the version used for the benchmark, so we recommand using a fixed version of the image: shadoko/docker_java_packages:version3

#### 4. Generate a Rstudio instance that you can connect to
    > docker run --rm -p 8787:8787 -v /Users/myname/myFolder:/home/rstudio/Docker-Benchmark/ shadoko/docker_java_packages:version3

    -- rm indicates that the container will be deleted when stopped
    -p 8787:8787 indicates that the Rstudio instance will be available on port 8787
    -v /myComputerPath/:/myContainerPath/ link a volume from your computer to your container VM, so that you can for example open your R project
    shadoko/docker_java_packages:version3  refers to the docker image you want to create a container from

Here you should link your docker container with a folder containing the GitHub project.  
Note : for windows OS syntax is different, and the User Public is recommended for rights issues /c/Users/Public/MyFolder:/home/rstudio/Project 


#### 5. Connect to your Rstudio instance
1. Check the IP of your computer which should look like 192.168.0.12
2. In your browser enter http://myContainerIP:8787 and sign in with id=rstudio and password=rstudio


#### 6. After use, close Docker
Close the container. In the command line use ctrl+c to close the container.  
Check that no container are running with the command :  
	> docker ps




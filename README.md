# Streaming Greedy Maximum Random Deviation

Welcome to the GitHub repository for the paper:

- Edouard Fouché, Florian Kalinke, Klemens Böhm. 2020. Efficient Subspace Search in Data Streams. Preprint 

You will find all the required code, data, and documentation to reproduce the results from our study. This repository is released under the AGPLv3 license. Please see the [LICENSE.md](LICENSE.md) file. 

## Data

Our benchmark data sets are in the folder `/data`. Decompress `data.zip` to set up this folder:

```
unzip data.zip
```

## Quick Start

### Build it and run it 

**Requirements** : ([Oracle JDK 8](https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
or [OpenJDK 8](http://openjdk.java.net/install/)) and [sbt](https://www.scala-sbt.org/1.0/docs/Setup.html)

The project is built with sbt (version 1.2.8). You can compile, package or run the project as follows:

```
sbt compile
sbt package 
sbt "run <arguments>"
```

In case you need to adjust the amount of memory assigned to the JVM, you can set the `javaOptions` (`-Xmx` and `-Xms`) in `build.sbt`.

## Reproducing the experiments

The results of the experiments are stored in `.csv` files in separate folders in `experiments/`.

### SGMRD

Parameter sensitivity study (we use the pyro dataset)

```
sbt "run experiments.SGMRDsearchers_pi" # Run with various update strategies and v (step size)
sbt "run experiments.SGMRDsearchers_gold" # Run the "golden" baseline
sbt "run experiments.SGMRDsearchers_L" # Let L (the number of plays per round) vary
sbt "run experiments.SGMRDsearchers_runtime" # Runtime evaluation
```

Perform the search for the synthetic and the real-world data

```
sbt "run experiments.SGMRDsearchers"
```

Copy all the resulting `*-subspaces-0.txt` files into `data/subspaces`, then run the outlier detection:

```
sbt "run experiments.SGMRDminers"
```

Run the outlier detection on the subspaces from StreamHiCS (execute the StreamHiCS section below first)

```
sbt "run experiments.StreamHiCSminers"
```

### Baselines

`RS-Hash` and `LOF` are included. For the other approaches, with used the following sources: 

- xStream (cpp): https://github.com/cmuxstream/cmuxstream-core 
- StreamHiCS: https://github.com/vincentbecker/StreamHiCS

## Visualize the results, create figures

**Requirements**: Jupyter notebook, Python3, numpy, pandas

We provide in folder `visualize/` a set of Jupyter notebooks to reproduce the figures in our study

- `SGMRDsearchers_pi.ipynb`: Reproduces Figure 7,8,9,10. 
- `SGMRD_runtime.ipynb`: Reproduces Figure 14. 
- `SGMRD_success.ipynb`: Reproduces Figure 11. 
- `SGMRDsearchers_L.ipynb`: Reproduces Figure 12.
- `SGMRDsearchers_runtime.ipynb`: Reproduces Figure 13.
- `SGMRDminers.ipynb`: Fetch the outlier detection results for SGMRD. 
- `StreamHiCSminers.ipynb`: Fetch the outlier detection results for StreamHiCS. 


# HMLET Installation Guide for Developers

## Applications
* Github Desktop
* RStudio 

## Install the Project on your local device 
###### Github Desktop
1. File > Clone Repository 
2. Select the HMLET Repository and choose a path to store the project locally. 
![github_1](README_imgs/github_1.png)
![github_2](README_imgs/github_2.png)

###### RStudio 
1. File > Open Project in New Session... > [Local Path]/HMLET/RPackage/HMLET/HMLET.Proj
2. In the top right panel, Select Build > Check
![RStudio_1](README_imgs/RStudio_1.png)
3. Install any missing dependencies using Packages in the bottom right panel. 
![RStudio_2](README_imgs/RStudio_2.png)
4. If you see this error: `ERROR: dependency 'rray' is not available for package 'HMLET'`, run this command within the RStudio console of the project: `remotes::install_github("r-lib/rray")`
5. Run Clean + Install within More under Build in top right panel. 
![RStudio_3](README_imgs/RStudio_3.png)
6. If there was an error during installation, remove HMLET.Rcheck directory and the HMLET tar.gz file. 

## Access documentation for a function within HMLET 
1. Within the bottom right panel under Files, open all the files that define the functions under HMLET/R. These file names math the name of its corresponding function. 
![documentation_1](README_imgs/documentation_1.png)
2. Within the console of RStudio enter these commands to retrieve documentation for a function:
```
library(HMLET)
?[functionName]
```
![documentation_2](README_imgs/documentation_2.png)

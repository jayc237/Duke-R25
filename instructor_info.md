# Computing
There are instructions on how to [access the computing system here](ondemand_howto.md).

It defaults to 2 cores and 2 GB, but you can change these if you need more cores or more RAM, and if you need more than the set limits in the App, let us know and we can change the limits.

# Content
The git repo for content is here: <https://gitlab.oit.duke.edu/hiv_r25/2022-2023-hiv-workshop>.

# Shared Data
If you have shared datasets you can put them in /hpc/group/hiv-r25-2022. Please make an appropriate subdirectory for the shared data. You will also want to change permissions after you have placed the data there, so that students can't accidentally alter or delete it.

```
# make new directory
mkdir -p /hpc/group/hiv-r25-2022/rnaseq/bioinformatics

# transfer data into the new directory

# remove write permissions
chmod -R a-w /hpc/group/hiv-r25-2022/rnaseq/bioinformatics
```

## Uploading Shared Data
### Command-line Upload
OOD is just a pretty front end for DCC, so you can use the same tools you would use for DCC (scp, sftp, rsync, Globus). RC has some details here: <https://oit-rc.pages.oit.duke.edu/rcsupportdocs/dcc/files/#cluster-shared-storage-resources-work-and-scratch>

### GUI Upload
OOD has a GUI-based mechanism for uploading and downloading data. You can get to the OOD file browser by clicking on “Files” in the OOD menu bar. In the OOD file browser you will see *Upload* and *Download* buttons. I do not know how well the GUI handles large amounts of data, so command-line tools may be better for large datasets.

# Output 
For output files, We tell participants to make a subdirectory named based on their Netid in /work, and put output files here (space in home directories is limited). Here is a code snippet that does this.

```
scratch_dir="/work"
username=Sys.info()[["user"]]
out_dir=file.path(scratch_dir,username,"hiv2022","rnaseq”)
dir_create(out_dir)
```

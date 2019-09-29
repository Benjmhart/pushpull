# pushpull

This is a tool for managing files that may exist and be updated outside of a repo, such as dotfiles.

Here is an example usecase:

- a repo is being used to track files which are related, but are actually used in scattered paths all over your system
- the files may be updated by a user in their external paths, without being updated in the repo, and symlinks are not a good solution
- updates are directly written over the remote master branch, possibly by cronjob
- the files may also be updated on another machine, and you want changes across systems to propigate automatically without losing data

pushpull attempts to solve these problems

In the repo directory, place a `mappings.txt` file, where each line shows the file in the repo and the corresponding file location outside the repo, in this format:

```
./myfileinrepo.txt : /home/username/myfilenotinrepo.txt
./myfolder         : /home/username/myfolder
```

In the case of folders, the entire file tree within the folder will be copied.

each time pushpull is run from within your repo,  pushpull will

1. parse the mappings.txt file
2. ignore files/folders where either the file in the repo or the external file does not exist
3. ignore files where the external file is not newer than the file in the repo
4. copy external files to the repo
5. add changes in git
6. perform an automated commit
7. pull changes from the origin git remote
8. stop if there are conflicts
9. push over master on origin

On a future update, pushpull will also automatically copy files to the remote locations listed in `mappings.txt`.

installation

installation requires haskell stack.

- clone this repo
- from the repo, run `stack install`

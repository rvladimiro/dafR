# Miniclip R Tools

## Summary

## Convention over Configuration

Miniclip R Tools were designed under a coding by convention paradigm. These conventions allow faster analysis and development cycles, minimizing small decisions by handling them automatically.

These conventions include:

#### Directory structure

There is a predefined directory structure. The first level of this directory structure is R's working directory for this project. Manually changing the working directory will produce unwanted results.

The directory structure ensures that projects are packed as expected and that standard Miniclip R Tools functions always work.

#### YAML files

Many functions depend on YAML files. These files often contain user names and passwords. By convention we use YAML files outside the project ensuring that files with sensitive information are not added to repositories. By default, the location of the YAML configuration files is in the directory above the project's root directory (and R's working directory).

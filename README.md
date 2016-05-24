# Data Analysis Framework in R

## Summary

dafR was born at Miniclip's Game Analytics & Data Science team. From utility functions in loose repositories to work methodologies discussed and implemented over time, many of the conventions are closely related to how we think and see data analysis.

Although I think that data analysis is an iterative process that has a number of established steps, it is possible that how those steps are implemented in dafR don't exactly fit the way you work. Unfortunately that means that dafR won't help you fully.

Here's what you need to know about dafR.

## Convention over Configuration

dafR was designed under a coding by convention paradigm. These conventions allow faster analysis and development cycles, minimizing small decisions by handling them automatically.

These conventions include:

#### Directory structure

There is a predefined directory structure. The first level of this directory structure is R's working directory for this project. Manually changing the working directory will produce unwanted results.

The directory structure ensures that projects are packed as expected and that standard dafR functions always work. The structure is not mandatory and many of dafR functions will work without it. If a function absolutely needs the directory structure, it will throw an error.

#### YAML files

Many functions depend on YAML files. These files often contain user names and passwords. By convention we use YAML files outside the project ensuring that files with sensitive information are not added to repositories. By default, the location of the YAML configuration files is in the directory above the project's root directory (and R's working directory).

What matters is: we do our best to avoid exposing usernames and passwords in the code and use YAML files to do it.

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acquirePostgreSQLQuery.R
\name{PostgreSQLQuery}
\alias{PostgreSQLQuery}
\title{Query data from a PostgreSQL server}
\usage{
PostgreSQLQuery(query, id, yamlFile = "~/db.yml", printProgress = TRUE)
}
\arguments{
\item{query}{The query to be ran. It can be a file with SQL or a character vector}

\item{id}{The server ID in the yaml config file}

\item{yamlFile}{The server configuration yaml file}
}
\value{
A dataframe with the result of the query
}
\description{
This function performs all the actions needed to run a query that returns tabular data from a PostgreSQL server. It handles connecting, querying and disconnecting. The query can either be passed directly as a string or as a path to a file containing a SQL statement.

The yaml file must contain host, dbname, user and password. Port is optional, if not present in the yaml file, the default for PostgreSQL will be used.

This function was created to run quick queries on Redshift. The difference between this function and RedshiftQuery is that RedshiftQuery was built to unload results to an S3 bucket and should the prefered option for large queries.
}
\author{
Ricardo Vladimiro
}

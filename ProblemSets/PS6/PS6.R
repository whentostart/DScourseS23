pg <- dbConnect(RPostgres::Postgres(), bigint = "integer")

rs <- dbExecute(pg, "SET search_path TO crsp")
msf <- tbl(pg, "msf")
msi <- tbl(pg, "msi")
stocknames <- tbl(pg, "stocknames")
dsedist <- tbl(pg, "dsedist")
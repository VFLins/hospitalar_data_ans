library(RSQLite)
library(DBI)

conn <- DBI::dbConnect(SQLite(), "dataset.sqlite")

dbListFields(conn, dbListTables(conn)[1])

querry <- dbGetQuery(conn,
							"SELECT * FROM pda_tiss_hosp WHERE ID_EVENTO_ATENCAO_SAUDE IN (
	SELECT ID_EVENTO_ATENCAO_SAUDE FROM pda_tiss_hosp ORDER BY RANDOM() LIMIT 440000)")
saveRDS(querry, "pda_tiss_hosp.rds")

querry <- dbGetQuery(conn,
	"SELECT * FROM pda_tiss_hosp WHERE ID_EVENTO_ATENCAO_SAUDE IN (
	SELECT ID_EVENTO_ATENCAO_SAUDE FROM pda_tiss_hosp ORDER BY RANDOM() LIMIT 100000)")
saveRDS(querry, "pda_tiss_hosp_mini.rds")

dbDisconnect(conn)
rm(querry, conn)

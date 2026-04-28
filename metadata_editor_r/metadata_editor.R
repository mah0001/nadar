# Install the devtools package
#install.packages("devtools")

# Load the devtools package
library(devtools)

# Install nadar package using github
install_github("mah0001/nadar")

# Install Metadata Editor
install_github("ihsn/metadataeditr")


library(nadar)
library(metadataeditr)

#devtools::load_all()

# only set the api url, no API key is needed for downloading DDI and RDF files
nadar::nada_set_api_url("https://catalog.ihsn.org/index.php/api")

# set the api key for Metadata Editor
metadataeditr::nada_set_api_key("a75223482971545f3ee331f39f450a9f")
metadataeditr::nada_set_api_url("http://localhost/metadata-editor/index.php/api")


# get first 10 results
# to get all contents of a catalog, set ps param to total number of entries in the catalog
results<-nadar::nada_study_search(api_base_url = "https://catalog.ihsn.org/index.php/api", page=1, ps=10)

if (is.null(results$content$result$rows$idno)) {
  stop("No results found")
}else{
    print(paste0("Results found: ", length(results$content$result$rows$idno)))
}


# create a folder to save the downloaded files
output_folder="downloads"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

total_rows=nrow(results$content$result$rows)

#Download all DDIs, RDFs and import into the Editor
for (i in 1:total_rows) {

    idno<-results$content$result$rows$idno[i]

    # download DDI if not exists
    if (!file.exists(paste0(output_folder, "/", idno, ".xml"))) {
        print(paste0("Downloading ", idno))
        ddi<-nadar::nada_study_download_ddi(idno, output_file = paste0(output_folder, "/", idno, ".xml"))
        print(paste0("DDI downloaded: ", ddi$file_path))
    }else{
        print(paste0("DDI already exists: ", idno))
        ddi$file_path=paste0(output_folder, "/", idno, ".xml")
    }

    # download RDF if not exists
    if (!file.exists(paste0(output_folder, "/", idno, ".rdf"))) {
        print(paste0("Downloading ", idno))
        rdf<-nadar::nada_study_download_rdf(idno, output_file = paste0(output_folder, "/", idno, ".rdf"))
        print(paste0("RDF downloaded: ", rdf$file_path))
    }else{
        print(paste0("RDF already exists: ", idno))
        rdf$file_path=paste0(output_folder, "/", idno, ".rdf")
    }


    #add a sleep of 1 second
    Sys.sleep(1)

    #add a progress bar
    cat(paste0("Downloading ", idno, " ", i, " of ", total_rows, "\n"))


    #import to Metadata Editor
    print(paste0("Importing ", idno))
    import_project<-metadataeditr::import_project("survey", ddi$file_path, idno=idno)
    print(paste0("Imported ", idno))

    #import RDF file
    print(paste0("Importing RDF ", idno))
    import_rdf<-metadataeditr::resources_import_rdf(idno=idno, rdf$file_path)
    print(paste0("Imported RDF ", idno))


    Sys.sleep(1)
    cat(paste0("Imported ", idno, " ", i, " of ", total_rows, "\n"))
}


# Usage examples for NADAR function to download DDI and RDF

# download DDI using IDNO
idno="CRI_2021_HFPS_v02_M"
ddi<-nadar::nada_study_download_ddi(idno, output_file = "downloads/CRI_2021_HFPS_v02_M.xml")
print(ddi$file_path)


# download DDI using direct URL
ddi_url="http://web.nso.mn/nadamn/index.php/catalog/ddi/365"
ddi<-nadar::nada_study_download_ddi(ddi_url=ddi_url, output_file = "ddi-365.xml")
print(ddi$file_path)


#download RDF using direct URL
rdf_url="http://web.nso.mn/nadamn/index.php/catalog/rdf/365"
rdf<-nadar::nada_study_download_rdf(rdf_url=rdf_url, output_file = "rdf-365.rdf")
print(rdf$file_path)

#download RDF using idno
idno="CRI_2021_HFPS_v02_M"
rdf<-nadar::nada_study_download_rdf(idno, output_file = "CRI_2021_HFPS_v02_M.rdf")
print(rdf)


# Usage examples for MetadataEditR functions for DDI and RDF import

# Import DDI to create a new project
# option 1: without providing any project identifier (IDNO)
# This will create a new project with an auto-generated ID every time
import_project<-metadataeditr::import_project("survey",ddi$file_path)

# option 2: with IDNO
# Project will only be created if IDNO does not exist already
import_project<-metadataeditr::import_project("survey", ddi$file_path, idno="project-identifier-goes-here")


# import rdf file
# note: running this multiple times will result in duplicate resources
import_rdf<-metadataeditr::resources_import_rdf(idno="project-idno-goes-here", rdf$file_path)



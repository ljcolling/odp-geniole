xfun::pkg_attach2("osfr")

geniole_download_info = ODPHelper::download_geniole()
dir.create("made")
saveRDS(object = geniole_download_info, file = "made/geniole_download_info.Rdata")


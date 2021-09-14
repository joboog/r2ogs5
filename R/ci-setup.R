pkgs <- scan("./DESCRIPTION", what = "character", quiet = TRUE)


pkgs <- stringr::str_remove(
    na.omit(
        pkgs[c((which(pkgs == "Imports:") + 1):
                   (which(pkgs == "Depends:") - 1),
               (which(pkgs == "Suggests:") + 1):
                   (which(pkgs == "VignetteBuilder:") - 1))]),
                            pattern = ",")


install.packages(setdiff(pkgs,
                         rownames(installed.packages(lib = "/usr/local/lib/R/site-library"))),
                 lib = "/usr/local/lib/R/site-library")

loadPackages = function(requiredPackages) {
    installPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
    if(length(installPackages) > 0)
        install.packages(installPackages)
    
    invisible(lapply(requiredPackages, library, character.only = TRUE))
}

loadData = function(fileName)   {
    load(fileName); 
    return (data) 
}

saveData = function(data, fileName) { 
    save(data, file=fileName)
}

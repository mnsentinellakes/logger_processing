




#Download Data----
output$download <- downloadHandler(
  filename = function() { 

  },
  content = function(file) {
    dataforexport=exportdata()
    write.csv(dataforexport,file,row.names=F)
  })

# CRAN Note avoidance
if(getRversion() >= "2.15.1") 
  utils::globalVariables(
    # sample file names from taxstats
    c('tree2ha', 'i.tree2ha', 'plot.id', 'BA.m2', 'dbh.mm', 'stands.ff', 'stand.age.years',
      'waiting.time', 'dev.class', 'SI.spp', 'SI.m', 'kom',  'tree.sp', 'height.dm', 'i.kom',
      'vol.wo.tr.m3', 'V1', 'period', 'plot.id', 'x', 'id'
      )
  )


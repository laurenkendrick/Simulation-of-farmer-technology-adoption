set.seed(x)
hogAFOs<-hogAFOsbackup
hogAFOs$X<-NULL
hogAFOs$cost_impcover_wo_manure[is.na(hogAFOs$cost_impcover_wo_manure)]<-0
hogAFOs$cost_percover_wo_manure[is.na(hogAFOs$cost_percover_wo_manure)]<-0
hogAFOs$cost_impcover[is.na(hogAFOs$cost_impcover)]<-0
hogAFOs$cost_percover[is.na(hogAFOs$cost_percover)]<-0
hogAFOs$cost_biofilter_wo_manure<-hogAFOs$cost_biofilter
hogAFOs$cost_scrubbers_wo_manure<-hogAFOs$cost_scrubbers
hogAFOs$cost_trees_wo_manure<-hogAFOs$cost_trees
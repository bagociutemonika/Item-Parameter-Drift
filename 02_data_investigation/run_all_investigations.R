
# RUN ALL DATA INVESTIGATIONS


source("02_data_investigation/investigate_dataset.R")


# SIMULATION 1.1


source("01_simulations/simulation_1_1.R")

investigate_dataset(
  data_long = data_long,
  dif_items = dif_items,
  J_total   = J_total,
  sim_name  = "sim11"
)


# SIMULATION 1.2


source("01_simulations/simulation_1_2.R")

investigate_dataset(
  data_long = data_long,
  dif_items = dif_items,
  J_total   = J_total,
  sim_name  = "sim12"
)


# SIMULATION 2.1


source("01_simulations/simulation_2_1.R")

investigate_dataset(
  data_long = data_long,
  dif_items = dif_items,
  J_total   = J_total,
  sim_name  = "sim21"
)


# SIMULATION 2.2


source("01_simulations/simulation_2_2.R")

investigate_dataset(
  data_long = data_long,
  dif_items = dif_items,
  J_total   = J_total,
  sim_name  = "sim22"
)


# SIMULATION 2.3


source("01_simulations/simulation_2_3.R")

investigate_dataset(
  data_long = data_long,
  dif_items = dif_items,
  J_total   = J_total,
  sim_name  = "sim23"
)

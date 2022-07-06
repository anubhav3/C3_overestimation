# 02.01.2022


using BioEnergeticFoodWebs, EcologicalNetworks, JLD2, Statistics, Plots, CSV, DataFrames, Random

# Network structure: three trophic level (diatoms, macroinvertebrates and fish) food chain
# With second trophic level divided into aerial dispersers and aquatic dispersers
mat = [[0, 0, 0, 0] [1, 0, 0, 0] [1, 0, 0, 0] [0, 1, 1, 0]]

# Setting the model parameters
p = model_parameters(mat)

# Assign biomass
bm = rand(size(mat,1))

# simulate
out = simulate(p, bm, start=0, stop=20)

# Plotting
Plots.plot(out[:t], out[:B], legend = true, ylabel = "Biomass", xlabel = "Time")

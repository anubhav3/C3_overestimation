# 02.02.2022

using BioEnergeticFoodWebs, EcologicalNetworks, JLD2, Statistics, Plots, CSV, DataFrames, Random

# Network structure: three trophic level food chain
mat = [[0, 0, 0] [1, 0, 0] [0, 1, 0]]


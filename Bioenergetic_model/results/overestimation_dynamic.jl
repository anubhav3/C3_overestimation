# 24.09.2021
# First, we load some predation matrix

using BioEnergeticFoodWebs, EcologicalNetworks, JLD2, Statistics, Plots, CSV, DataFrames, Random

pred_mat = Matrix(CSV.read("../data/csv_files/Benguela Pelagic.web.csv", DataFrame))

A = pred_mat

p = model_parameters(A)

begin
    bm = rand(size(A,1))
    out = simulate(p, bm, start=0, stop=2000)
end



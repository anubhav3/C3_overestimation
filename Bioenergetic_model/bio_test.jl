using BioEnergeticFoodWebs, EcologicalNetworks, JLD2, Statistics, Plots, CSV, DataFrames, Random
Random.seed!(21)


begin
    # generate network
    A_bool = EcologicalNetworks.nichemodel(20,0.15)
    # convert the UnipartiteNetwork object into a matrix of 1s and 0s
    A = Int.(A_bool.edges)
end


A = Array(A)

co = sum(A)/(size(A,1)^2)

p = model_parameters(A)

p[:trophic_rank]


begin
    bm = rand(size(A,1))
    out = simulate(p, bm, start=0, stop=2000)
end

Plots.plot(out[:t], out[:B], legend = true, ylabel = "Biomass", xlabel = "Time")

biomass = total_biomass(out, last=1000)

diversity = foodweb_evenness(out, last=1000)

persistence = species_persistence(out, last=1000)

stability = population_stability(out, last=1000)


using DifferentialEquations
using Plots

function LV_model(du,u,p,t)
    # growth rate of the resource (modelled as a logistic growth function)
    GrowthR = p.growthrate * u[1] * (1 - u[1]/p.K)
    # rate of resource ingestion by consumer (modelled as a type I functional response)
    IngestC = p.ingestrate * u[1] * u[2]
    # mortality of consumer (modelled as density independent) 
    MortC = p.mortrate * u[2]
    # calculate and store changes in abundance (du/dt):
    # change in resource abundance
    du[1] = GrowthR - IngestC
    # change in consumer abundance
    du[2] = p.assimeff * IngestC - MortC
end

# Parameters
p = (growthrate = 1.0, ingestrate = 0.2, mortrate = 0.2, assimeff = 0.5, K = 10)

# Initial values
u0 = [1.0, 1.0]

# Timespan
tspan = (0.0, 100.0)

prob = ODEProblem(LV_model, u0, tspan, p)

sol = solve(prob)

plot(sol,
    ylabel = "Abundance",
    xlabel = "Time",
    title = "Lotka-Volterra",
    label = ["prey" "predator"], 
    linestyle = [:dash :dot],
    lw=2)
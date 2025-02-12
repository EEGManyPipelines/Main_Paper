using Pkg

temp_dir = mktempdir() * "/unfoldstats"
Pkg.activate(temp_dir)
# Pkg.activate(tempdir()*"/unfoldstats")
Pkg.add(["UnfoldMakie", "CairoMakie", "PlutoUI", "UnfoldSim", "Unfold", "MixedModelsPermutations", "PlutoExtras", "ClusterDepth", "StatsModels", "Random", "DataFramesMeta"])
ENV["JULIA_SSL_CA_ROOTS_PATH"] = ""


# Pkg.add(url="https://github.com/unfoldtoolbox/UnfoldStats.jl")
# Pkg.add(PackageSpec(url="https://github.com/unfoldtoolbox/UnfoldStats.jl", rev="lmm_perm"))
# Pkg.add(PackageSpec(url="https://github.com/unfoldtoolbox/UnfoldStats.jl", rev="lmm_perm"))
Pkg.add(PackageSpec(path="C:/Users/ncb623/unfold/UnfoldStats.jl", rev="lmm_perm"))
# Pkg.add(url="https://github.com/unfoldtoolbox/UnfoldStats.jl",rev="lmm_perm")
Pkg.add("MAT")
Pkg.add("CSV")

###
using UnfoldMakie
using CairoMakie
using PlutoUI
using UnfoldSim
using Unfold
using MixedModelsPermutations
using PlutoExtras
using ClusterDepth
using StatsModels
using Random
using DataFramesMeta
using UnfoldStats
using CSV
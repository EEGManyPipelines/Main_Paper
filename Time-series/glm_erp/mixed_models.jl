# using Pkg
using MAT
using DataFrames
using DataFramesMeta
using CategoricalArrays
using Unfold
using UnfoldMakie, CairoMakie       # for plotting
using UnfoldSim
using CSV

#################################################################################################################
# READ DATA
#################################################################################################################
## Load the EEG data .mat file
file = matopen("C:\\Users\\ncb623\\EMP\\data\\alldatmat.mat")
varnames = keys(file)
alldat = read(file, "alldatmat")                # grp x subj x chan x time
close(file)

## Get data size
n_grp, n_subj, n_elec, n_t = size(alldat)

grp_id = 1:n_grp
subj_id = 1:n_subj
elec_id = 1:n_elec
times = 1:size(alldat, 4)
t_idx = (times .- 1) * n_t .+ 1

function reshape_with_factors(matrix_4d)
    # Get the size of each dimension
    dim1, dim2, dim3, dim4 = size(matrix_4d)
    
    # Reshape the 4D matrix to a 3D matrix
    matrix_3d = reshape(matrix_4d, dim3, dim4, dim1 * dim2)
    
    # Create the DataFrame for the factors
    factors = DataFrame(
        grp = repeat(1:dim1, inner=dim2),
        subj = repeat(1:dim2, outer=dim1),
        latency = collect(0:(dim1*dim2 -1)) * dim4 .+ 1
    )
    # insertcols!(factors, :latency => (collect(1:size(factors, 1)) .- 1) * dim4 .+ 1)
    factors.grp = string.(factors.grp)
    factors.subj = string.(factors.subj)

    return matrix_3d, factors
end

# Arrange data and make event DataFrame
long_data, events = reshape_with_factors(alldat)
# long_data = replace(long_data, NaN=>missing)      # This and model fit does not work

# NOTE: time dimension is just from 1 to length of data. Should somewhere be aligned to the correct time axis.
times_cont = range(-0.2,length=n_t, step=1/256) 

## Get questionnaire data and arrange
qdat = CSV.read("C:\\Users\\ncb623\\EMP\\data\\all_var_AQ_h1.csv", DataFrame)
iddat = CSV.read("C:\\Users\\ncb623\\EMP\\data\\IDs_linear_models.csv", DataFrame)
gdat = CSV.read("C:\\Users\\ncb623\\EMP\\data\\grptab.csv", DataFrame)

# Convert Var1 column to a factor
# gdat.Var1 = categorical(gdat.Var1)

# Convert teamID to a factor, filter qdat, and sort to correspond to ERP data 
# [NOTE: why is only 77 of 168 teams included ???]
qdat.Team_name = iddat.teamID
qdat = qdat[vcat([findall(isequal(x), qdat.Team_name) for x in gdat.Var1]...), :]
qdat.order = [findfirst(isequal(x), gdat.Var1) for x in qdat.Team_name]
qdat = sort(qdat, :order)

# Update software column
qdat.software[(qdat.software .== "eeglab_erplab") .| (qdat.software .== "eeglab_limo")] .= "eeglab"
qdat.software[qdat.software .== "custom"] .= "other"

# Create ica column
qdat.ica = ifelse.(ismissing.(qdat.ans_ica_algo), "no", "yes")

# Update reref column
qdat.reref[(qdat.reref .== "unknown")] .= "other"

# Convert columns to factors
# qdat.ans_ica_algo = categorical(qdat.ans_ica_algo)
# qdat.result_h1 = categorical(qdat.result_h1)
# qdat.software = categorical(qdat.software)

qdat.grp = string.(collect(1:length(qdat.Team_name)))

df = innerjoin(events, qdat, on = :grp)

#################################################################################################################
# GLMM
#################################################################################################################
# formula: random effects only [NOTE: so far not working]
f = @formula(0 ~ reref + (reref | subj))

# Fit the model
m = fit(UnfoldModel, f, df, long_data, times_cont, show_progress=true);

m

# first(coeftable(m))
coef(m)
coeftable(m)

results = coeftable(m)

plot_erp(coeftable(m))
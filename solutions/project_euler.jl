# Julia: https://julialang.org/
# online: https://julialang.org/learning/tryjulia/

p001 = sum([i for i in 1:999 if any(i .% [3, 5] .< 1)])

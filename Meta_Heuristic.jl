using JuMP, Statistics, Printf, DataFrames, XLSX, Combinatorics, Random, HiGHS

# Set current directory
cd("C:/Users/brunb/OneDrive/Skrivebord/Master Thesis Code")

include("Function_Library.jl")
include("Meta_Heuristic_Library.jl")
include("Constructor_Heuristics.jl")

destroy = (Destroy_Remove_Compartments,Destroy_Remove_Random_Route,Destroy_Remove_Least_Filled_Route,Destroy_Add_Good_Route)
repair  = (Repair_Compartments ,Repair_Closest_Tank,Repair_Remove_Max_Violations,Repair_Remove_Time_Violations)
clean   = (Clean_Push_Routes,Clean_Remove_Route,Clean_Remove_Station,Clean_Fill_Route)

# Acceptance criteria
function Simulated_Annealing(temp_obj, current_obj,t)
    return rand() <= exp((current_obj- temp_obj) / t)
end
function Update_T(t,T_end,T_start,max_iter) 
        return t*(T_end/T_start)^(1/max_iter)
end

function Meta_Heuristic(solution,max_iter,t,T_end,T_start,print_iter)
    println("Running metaheuristic...")
    println(" ")
    opt_des_loop = [2,1,1,1]
    best_solution    = deepcopy(solution)
    current_solution = deepcopy(best_solution);
    temp_solution    = deepcopy(best_solution);
    it = 1
    while it < max_iter
        temp_solution = deepcopy(current_solution)
        feasibility   = 3
        amount_diff   = 0
        while feasibility == 3 
            counter_d = 0
            checker = Inf 
            d = rand(setdiff(1:length(destroy),[]))
            first_time = 1
            while feasibility == 3 || counter_d != checker 
                counter_d += 1
                obj_diff_destroy, amount_diff_destroy = destroy[d](temp_solution)
                amount_diff += amount_diff_destroy
                max,min,time2 = Check_Feasibility(temp_solution)
                feasibility = sum([max,min,time2])
                if feasibility < 3 && first_time == 1
                    checker = counter_d*opt_des_loop[d]
                    first_time = 0
                end 
            end
        end
        set= [1,2,3,4]
        r = 0
        iter_repair_loop = 0
        while feasibility < 3     
            set_repair = intersect(setdiff(1:length(repair),[r]) ,set  )
            r = rand(set_repair) 
            if r== 2 || r == 3 
                set = [2,3,4]
            end          
            if repair[r](temp_solution)[1] == "exception_reset"
                return "exception_reset" 
            end
            obj_diff_repair, amount_diff_repair = repair[r](temp_solution)
            max,min,time2 = Check_Feasibility(temp_solution)
            feasibility = sum([max,min,time2])
            amount_diff += amount_diff_repair
            if iter_repair_loop > 1000
                return "exception_reset" 
            end
            iter_repair_loop += 1
        end
        if Simulated_Annealing(temp_solution.Objective, current_solution.Objective,t)
            current_solution = deepcopy(temp_solution)
        end
        if temp_solution.Objective < best_solution.Objective
            best_solution = deepcopy(temp_solution)
        end
        t = Update_T(t,T_end,T_start,max_iter)      
        if print_iter == 1    
            println("Iteration $it. Best Obj.: $(best_solution.Objective). Current Accepted Obj.: $(current_solution.Objective). Feasible: $(feasibility == 3 ? "Yes" : "no"). T: $(round(t,digits=5))")
        end
        it += 1
    end
    return best_solution
end
        
error_codes = 0
max_iter = 3000
T_end   = 1
T_start = 10
T= T_start
print_iter = 1
number_of_runs = 1

fuel_deliv = Constructor_Single_Stop(1.0);
sol = Fuel_To_Struct(fuel_deliv);

dictionary_of_solutions = Dict()
for i = 1:number_of_runs
    @label exception_reset
    meta_solution = Meta_Heuristic(sol,max_iter,T,T_end,T_start,print_iter)
    if meta_solution == "exception_reset"
        @goto exception_reset
    end
    dictionary_of_solutions[i] = meta_solution
    println("The best solution had an objective value of $(meta_solution.Objective)")
end




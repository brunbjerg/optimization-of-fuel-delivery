using HiGHS
#Support functions
if true
    #Gives all available vehicles for a specific day and time
    function Available_Vehicles(routes_dt,d,t)
        time_for_each_vehicle = Dict()
        remaining_time_for_each_vehicle = Dict() 
        #Initialize all vehicles
        for (k,v) in Vehicles
            time_for_each_vehicle[k] = 0
            remaining_time_for_each_vehicle[k] = 0
        end

        #Extract the time of all routes and add them together to find the total time length each vehicle
        for i = 1:length(routes_dt)
            time_for_each_vehicle[routes_dt[i].Vehicle] += routes_dt[i].RouteTime
            #println(900 + 600*length(list_of_routes[i].Station) + list_of_routes[i].TotalAmount/fuel_flow_rate)
        end 

        time_for_each_vehicle[39211] += time_for_each_vehicle[3921]
        time_for_each_vehicle[3921]   = time_for_each_vehicle[39211]
        time_for_each_vehicle[39231] += time_for_each_vehicle[3923]
        time_for_each_vehicle[3923]   = time_for_each_vehicle[39231]

        for (k,v) in remaining_time_for_each_vehicle
            remaining_time_for_each_vehicle[k] = Trips[k].TimePeriodLengths[d][t] - time_for_each_vehicle[k]
        end

        return remaining_time_for_each_vehicle, time_for_each_vehicle
    end

    #Gives the remaining compartment capacities on a route as well as the products contained
    function Compartment_Capacity(route)  
        used_amount      = collect(Iterators.flatten(Iterators.flatten(route.CompsAmount)))
        used_compartment = collect(Iterators.flatten(Iterators.flatten(route.Comps)))
        
        total_capacity = Vehicles[route.Vehicle].VolCompartment
        used_capacity = zeros(Float64,length(Vehicles[route.Vehicle].VolCompartment))
        used_product  = zeros(Int64,length(Vehicles[route.Vehicle].VolCompartment))

        for i=1:length(used_compartment)
            used_capacity[used_compartment[i]] += used_amount[i] 
        end

        for i =1:length(route.Station), a = 1:length(route.Tank[i]), c = 1:length(route.Comps[i][a]), j = 1:length(Vehicles[route.Vehicle].VolCompartment)
            if route.Comps[i][a][c]==j
                used_product[j] = StationsArray[route.Station[i]].Tanks[route.Tank[i][a]].ProductNumber
            end
        end
        if sum(used_product) >= 1
            weight_left = Vehicles[route.Vehicle].WeightCap - sum(used_capacity[i]*Products[ProductIDs[used_product[i]]].Density for i = 1:length(used_product) if used_product[i] != 0 ) 
        else
            weight_left =  Vehicles[route.Vehicle].WeightCap
        end

        return total_capacity .- used_capacity, used_product, weight_left
    end

    #Gives the remaining compartment capacities on a route for a specific product
    function Compartment_Capacity_Product(route,product)  
        Stats=route.Station
        Tanks=route.Tank
        Comps=route.Comps
        CompsAmount=route.CompsAmount

        CompCapacity = deepcopy(Vehicles[route.Vehicle].VolCompartment)

        for i=1:length(Stats),a=1:length(Tanks[i])
            if StationsArray[Stats[i]].Tanks[Tanks[i][a]].ProductNumber!=product
                CompCapacity[Comps[i][a]].=0.0
            else
                curComps = Comps[i][a]
                for k=1:length(curComps)
                    CompCapacity[curComps[k]]-=CompsAmount[i][a][k]
                end
            end
        end
        
        if sum(CompCapacity)<=1000.0
            return CompCapacity,[],0
        end

        weightLeft = Vehicles[route.Vehicle].WeightCap
        Amounts = route.Amount

        for i=1:length(Stats),a=1:length(Tanks[i])
            weightLeft -= Amounts[i][a]*Products[ProductIDs[StationsArray[Stats[i]].Tanks[Tanks[i][a]].ProductNumber]].Density
        end
        
        if weightLeft <= 0.01
            return 0,[],0
        end
        
        AvailComps=[]
        for cc=1:length(CompCapacity)
            if CompCapacity[cc]>0.01
                push!(AvailComps,cc)
            end
        end
          
        return CompCapacity, AvailComps, weightLeft
    end

    #Function that swaps route orders for linear decreasing consumption
    #=
    function Swap_Route_Order(SolDT,Order)
    end
    =#

    #Swaps order of stations in a route corresponding to given order
    function Swap_Station_Order(route,order)
        newStat = []
        newTank = []
        newAmount = []
        newComps = []
        newCompsAmount = []
    
        for i=1:length(route.Station)
            push!(newStat,         route.Station[order[i]])
            push!(newTank,         route.Tank[order[i]])
            push!(newAmount,       route.Amount[order[i]])
            push!(newComps,        route.Comps[order[i]])
            push!(newCompsAmount,  route.CompsAmount[order[i]])
        end
        RouteTime=0
        RouteDist=0
        Stats=[1;newStat;1]
        for i=1:length(Stats)-1
            RouteTime+=TimeMatrix[Stats[i],Stats[i+1]]
            RouteDist+=DistMatrix[Stats[i],Stats[i+1]]
        end
        obj_diff=RouteDist-route.RouteDist
        RouteTime+=length(route.Station)*600+900+fuel_flow_rate*route.TotalAmount
        route.RouteTime=RouteTime
        route.RouteDist=RouteDist

        
        route.Station = newStat
        route.Tank = newTank
        route.Amount = newAmount
        route.Comps = newComps
        route.CompsAmount = newCompsAmount

        return obj_diff
    end

    #Swaps the order of a route around in order to minimize distance in that given route
    function Optimize_Route_Order(route)
        minStats = deepcopy(route.Station)
        newStats = collect(permutations(minStats))
        minLength = deepcopy(route.RouteDist)
        obj_diff=0
        
        for j = 1:length(newStats)
            newStat = [1;newStats[j];1]
            newLength = 0
            for k = 1:length(newStat)-1
                newLength += DistMatrix[newStat[k],newStat[k+1]]
            end
            if newLength < minLength
                minLength = newLength
                minStats = newStat[2:end-1]
            end
        end
        
        newOrder = indexin(minStats,route.Station)
        if newOrder!=1:length(newOrder)
            obj_diff = Swap_Station_Order(route,newOrder)
        end
        return obj_diff
    end

    #More of an improvement heuristic so on pause
    function Add_Closest_Station(route,Remain_k,curlvl,day)

        minLength=deepcopy(route.RouteDist)*10
        Stats=route.Station
        minStats= 0
        minTime = 0

        for i in setdiff(2:nStation,Stats)
            if !(k in StationsArray[i].Allowed_Vehicles)
                continue
            end
            newStats = collect(permutations([Stats;i]))
            for j = 1:length(newStats)
                newStat = [1;newStats[j];1]
                newLength = 0
                newTime = 0
                for k = 1:length(newStat)-1
                    newLength += DistMatrix[newStat[k],newStat[k+1]]
                    newTime += TimeMatrix[newStat[k],newStat[k+1]]
                end
                if newLength < minLength
                    minLength = newLength
                    minStats = newStat[2:end-1]
                    minTime = newTime
                end
            end
        end
        newStat = setdiff(minStats,Stats)[1]
        curlvl = deepcopy(curlvl[newStat][:,day])
        nTanks = StationsArray[newStat].nTank
        sump = collect(StationsArray[newStat].Tanks[i].Consumption[day:day+1] for i=1:nTanks)
        sumpCur = collect(sump[i][2]/curlvl[i] for i=1:nTanks)

        CompCapacity,CompProducts,weightLeft = Compartment_Capacity(route)

        for a = 1:nTanks
            tank = findmax(sumpCur)[2]
            sumpCur[tank]=0
            TankProd = StationsArray[newStat].Tanks[tank].ProductNumber
            AvailComps=findall(x->x>0,CompCapacity[findall(x->x==TankProd,CompProducts)])
            if length(AvailComps)==0
                continue
            else
                fuelAmount = max(0,min(
                        capLeft,
                        StationsArray[newStat].Tanks[tank].realCap-curlvl[tank]+sump[tank],
                        weightLeft/Products[ProductIDs[TankProd]].Density))
            end

        end

        return minLength,minStats,newStat
    end

    #Gives all routes that visit a tank sorted by amount/dist ascending, each list element is [route,d,t,r,amount/dist]
    function Support_All_Tank_Routes(solution,station,tank)
        tank_routes = []
        for d=1:nDays,t=1:Trips[3706].nTimePeriod[d],r=1:length(solution.Routes[d,t])
            route = solution.Routes[d,t][r]
            Stats = route.Station
            station_index=0
            for j=1:length(Stats)
                if Stats[j]==station
                    station_index=j
                    break
                end
            end
            if station_index!=0 && tank in route.Tank[station_index[1]]
                push!(tank_routes,[route,d,t,r,round(route.TotalAmount/route.RouteDist,digits=2)]) 
            end
        end
        tank_routes = sort(tank_routes,by = x->x[5])
        return tank_routes
    end

    #Gives all routes that visit a station sorted by amount/dist ascending, each list element is [route,d,t,r,amount/dist]
    function Support_All_Station_Routes(solution,station)
        station_routes = []
        for d=1:nDays,t=1:Trips[3706].nTimePeriod[d],r=1:length(Sol.Routes[d,t])
            route = solution.Routes[d,t][r]
            if station in route.Station
                push!(station_routes,[route,d,t,r,round(route.TotalAmount/route.RouteDist,digits=2)]) 
            end
        end
        station_routes = sort(station_routes,by = x->x[5])
        return station_routes
    end

    #Given some amounts and a route, allocates fuel to fill the vehicle as best as possible
    function Optimal_Compartment_To_Stations_Allocation(stations,amounts,product_in_amounts,capacity,k)
        product = []
        for i = 1:length(amounts)
            temp = []
            for a = 1:length(amounts[i])
                value = zeros(6)
                value[product_in_amounts[i][a]] = 1 
                push!(temp,value)
            end
            push!(product, temp)
        end
        # println("CAPACITY" )
        # println(capacity)
        # println("Amounts")
        # println(amounts)
        
        
        nSta = length(amounts)
        nTan = length.(amounts)
        nCom = length(capacity)
        nPro = length(ProductIDs)
        M = 60000
        weights_for_each_station = [2,1,1,1,1,1,1,1]
        fill_in_first_station = max

        Density = [0.72,0.72,0.83,0.72,0.72,0.83]
    
        Optimal_Route = Model()
        set_optimizer(Optimal_Route, HiGHS.Optimizer)
        set_optimizer_attribute(Optimal_Route,"output_flag" , false)
        @variable(Optimal_Route, 0<=x[i = 1:nSta, a = 1:nTan[i], c = 1:nCom,p=1:nPro ])
        @variable(Optimal_Route, P[c = 1:nCom, p = 1:nPro],Bin)
    
        @constraint(Optimal_Route, con1[i=1:nSta,a = 1:nTan[i]],    sum(x[i,a,c,p] for c = 1:nCom for p = 1:nPro) <= amounts[i][a])
        @constraint(Optimal_Route, con2[c=1:nCom],                  sum(x[i,a,c,p] for i = 1:nSta for a = 1:nTan[i] for p = 1:nPro) <= capacity[c])
        #@constraint(Optimal_Route, con_force_first_station,         sum(x[1,a,c,p] for a = 1:nTan[1] for c = 1:nCom for p = 1:nPro ) >= min(sum(amounts[1]),sum(capacity) )  )
        @constraint(Optimal_Route, con3[c=1:nCom],                  sum(P[c,p] for p = 1:nPro) == 1)
        @constraint(Optimal_Route, con4[i=1:nSta,a = 1:nTan[i],p=1:nPro, c = 1:nCom], x[i,a,c,p]  <= P[c,p]*M)
        #println(product)
        @constraint(Optimal_Route, con5[i=1:nSta,a = 1:nTan[i],p=1:nPro, c = 1:nCom], x[i,a,c,p]  <= product[i][a][p]*M  )
        @constraint(Optimal_Route, con6, sum(x[i,a,c,p]*Density[p]  for i=1:nSta for a = 1:nTan[i] for c = 1:nCom for p = 1:nPro) <= Vehicles[k].WeightCap  )
    
        @objective(Optimal_Route, Max, sum(weights_for_each_station[i]*x[i,a,c,p] for i=1:nSta for a = 1:nTan[i] for c = 1:nCom for p = 1:nPro ))
        optimize!(Optimal_Route)
    
        PPP = value.(P)
        Sol_X = Dict()
        XXX = value.(x)
        # println("PPP")
        # println(PPP)

        for i=1:nSta
            for a = 1:nTan[i]
                for c = 1:nCom
                    for p = 1:nPro
                        if XXX[i,a,c,p] > 0.01
                            Sol_X[i,a,c,p] = XXX[i,a,c,p]
                        end
                    end
                end
            end
        end
        # println(Sol_X)
        tanks = []
        amount = []
    
        compartment_to_tank = []
        compartment_to_tank_amount = []
    
        for i = 1:nSta
            Sta = filter(x->x.first[1] == i,Sol_X)
            temp_tanks = []
            temp_amount = []
            temp_comp_first = []
            temp_comp_amount_first = []
            for a = 1:nTan[i]
                Tan = filter(x->x.first[2] == a, Sta)
                if !isempty(Tan)
                    push!(temp_tanks,a)
                    push!(temp_amount,sum(collect(values(Tan))))
                end
                temp_comp_second = []
                temp_comp_amount_second = []
                for c = 1:nCom
                    Com = filter(x->x.first[3]==c,Tan)
                    if !isempty(Com)
                        push!(temp_comp_second,c)
                        push!(temp_comp_amount_second,sum(collect(values(Com))) ) 
                    end
    
                end
    
                if !isempty(temp_comp_second)
                    push!(temp_comp_first,temp_comp_second)
                    push!(temp_comp_amount_first,temp_comp_amount_second)
                end
    
            end
    
            if !isempty(temp_amount)
                push!(tanks, temp_tanks)
                push!(amount,temp_amount)
            end
    
            if !isempty(temp_comp_first)
                push!(compartment_to_tank , temp_comp_first)
                push!(compartment_to_tank_amount , temp_comp_amount_first)
            end
    
    
        end
    

        stations_temp = unique(x->x.first[1],Sol_X)
        stations_index = sort(collect(stations_temp[i][1][1] for i=1:length(stations_temp)))
    
        new_stations = stations[stations_index]
        
        return new_stations,tanks,amount,compartment_to_tank,compartment_to_tank_amount
    end

    #Checks the feasibility of a solution wrt. max cap, min cap and time
    function Check_Feasibility(solution)
        routes = solution.Routes
        feas_max  = 1
        feas_min  = 1
        feas_time = 1
        if minimum(minimum.(solution.CurrentLevel))<0.0
            minini=findmin(findmin.(solution.CurrentLevel))
            stat = minini[2]
            tank = minini[1][2][1]
            day = minini[1][2][2]
            #println("min i,a,d $stat $tank $day")
            # println(minimum(minimum.(solution.CurrentLevel)))
            # if stat == 21 && tank == 2
            #     println("curlvl")
            #     println(solution.CurrentLevel[stat][tank,:])
            # end
            # println("violations: ")
            # println(length(filter(x->x<0,minimum.(solution.CurrentLevel))))
            feas_min=0
        end
        temp_curlvl = deepcopy(solution.CurrentLevel)
        for i=2:nStation, a=1:StationsArray[i].nTank
            if StationsArray[i].Tanks[a].StartLevel+StationsArray[i].Tanks[a].Consumption[1]>StationsArray[i].Tanks[a].realCap
                temp_curlvl[i][a,1] -= StationsArray[i].Tanks[a].StartLevel+StationsArray[i].Tanks[a].Consumption[1]-StationsArray[i].Tanks[a].realCap
            end
        end
        if 0.01<maximum(maximum.(temp_curlvl[i][a,:].-StationsArray[i].Tanks[a].realCap.+StationsArray[i].Tanks[a].Consumption[1:nDays] for i=2:60 for a=1:StationsArray[i].nTank))
            minini=findmax(findmax.(collect(temp_curlvl[i][a,:].-StationsArray[i].Tanks[a].realCap.+StationsArray[i].Tanks[a].Consumption[1:nDays] for i=2:60 for a=1:StationsArray[i].nTank)))
            stat,tank = Tank_To_Station[minini[2]]
            day = minini[1][2]
           
            feas_max=0
        end
        for day=nDays:-1:1,time=Trips[3706].nTimePeriod[day]:-1:1
            if sort(collect(Available_Vehicles(routes[day,time],day,time)[1]),by = x->x.second)[1][2]<0.0
                feas_time=0
                break
            end
        end
        return feas_max,feas_min,feas_time
    end

end

#Delete functions
if true
    #Deletes a given route
    function Delete_Route(routes_dt,route_number,curlvl,day)
        Route = routes_dt[route_number]
        
        amount_diff=-Route.TotalAmount
        obj_diff=-Route.RouteDist

        for i=1:length(Route.Station)
            stat = Route.Station[i]
            for a=1:length(Route.Tank[i])
                tank = Route.Tank[i][a]
                amount = Route.Amount[i][a]
                curlvl[stat][tank,day:end].-=amount
            end
        end

        deleteat!(routes_dt,route_number)

        return obj_diff,amount_diff
    end

    #Deletes a given station
    function Delete_Station(routes_dt, route,station,curlvlstat,day)
        obj_diff=0
        amount_diff=0

        if length(route.Station)==1
            rNum = findmax(collect(route.Station==routes_dt[i].Station && route.Tank==routes_dt[i].Tank && 
            route.Comps==routes_dt[i].Comps && route.CompsAmount==routes_dt[i].CompsAmount && route.Vehicle==routes_dt[i].Vehicle   for i=1:length(routes_dt)))[2]
            
            curlvl = fill(Array{Float64}(undef, 0, 0),nStation)
            curlvl[station]=curlvlstat

            obj_diff,amount_diff = Delete_Route(routes_dt,rNum,curlvl,day)
        else
            pushfirst!(route.Station,1)
            push!(route.Station,1)
    
            statIndex=findall(x->x==station,route.Station)[1]
    
            obj_diff=DistMatrix[route.Station[statIndex-1],route.Station[statIndex+1]]-DistMatrix[route.Station[statIndex-1],route.Station[statIndex]]-DistMatrix[route.Station[statIndex],route.Station[statIndex+1]]
            time_diff=-600+TimeMatrix[route.Station[statIndex-1],route.Station[statIndex+1]]-TimeMatrix[route.Station[statIndex-1],route.Station[statIndex]]-TimeMatrix[route.Station[statIndex],route.Station[statIndex+1]]-fuel_flow_rate*sum(route.Amount[statIndex-1])
    
            route.RouteDist+=obj_diff
           
            route.RouteTime+=time_diff
    
            route.Station=route.Station[2:(end-1)]
            statIndex-=1
            for i=1:length(route.Tank[statIndex])
                curlvlstat[route.Tank[statIndex][i],day:end].-=route.Amount[statIndex][i]
            end
            amount_diff=sum(route.Amount[statIndex])
            route.TotalAmount-=amount_diff
            deleteat!(route.Station,statIndex)
            deleteat!(route.Tank,statIndex)
            deleteat!(route.Amount,statIndex)
            deleteat!(route.Comps,statIndex)
            deleteat!(route.CompsAmount,statIndex)
        end

        return obj_diff,-amount_diff
    end

    #Deletes a given tank
    function Delete_Tank(routes_dt,route,station,tank,curlvlstat,day)
        obj_diff=0
        amount_diff=0

        statIndex=findall(x->x==station,route.Station)[1]
        if length(route.Tank[statIndex])==1
            obj_diff,amount_diff = Delete_Station(routes_dt,route,station,curlvlstat,day)
        else                
            tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]
            amount_diff=route.Amount[statIndex][tankIndex]
            TimeChange=-fuel_flow_rate*amount_diff

            route.RouteTime+=TimeChange

            curlvlstat[route.Tank[statIndex][tankIndex],day:end].-=amount_diff
            
            route.TotalAmount-=amount_diff

            deleteat!(route.Tank[statIndex],tankIndex)
            deleteat!(route.Amount[statIndex],tankIndex)
            deleteat!(route.Comps[statIndex],tankIndex)
            deleteat!(route.CompsAmount[statIndex],tankIndex)
        end
        
        return obj_diff,-amount_diff
    end

    #Deletes a given compartment
    function Delete_Compartment(routes_dt,route,station,tank,comp,curlvlstat,day)
        statIndex=findall(x->x==station,route.Station)[1]
        tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]
        obj_diff = 0
        amount_diff = 0

        if length(route.Comps[statIndex][tankIndex])==1
            obj_diff,amount_diff = Delete_Tank(routes_dt,route,station,tank,curlvlstat,day)
        else             
            compIndex=findall(x->x==comp,route.Comps[statIndex][tankIndex])[1]
            
            amount_diff=route.CompsAmount[statIndex][tankIndex][compIndex]
            time_diff=-fuel_flow_rate*amount_diff

            route.RouteTime+=time_diff

            for d=day:nDays
                curlvlstat[tank,d]-=amount_diff
            end
            # curlvlstat[tank,day:end].-=amount_diff
            # solution.CurrentLevel[route.Tank[statIndex][tankIndex],day:end].-=amount_diff
            
            route.Amount[statIndex][tankIndex]-=amount_diff
            route.TotalAmount-=amount_diff

            deleteat!(route.Comps[statIndex][tankIndex],compIndex)
            deleteat!(route.CompsAmount[statIndex][tankIndex],compIndex)
        end

        return obj_diff,-amount_diff
    end
end

#Insert functions
if true
    #Inserts a single given route and its amounts
    function Insert_Route(routes_dt, stations, tanks, amounts, k, comps, comps_amount, curlvl, day, rNum)
        #Error codes start:
        if error_codes==1
            product = zeros(length(stations),5,8)
            amount_in_each_compartment = zeros(Vehicles[k].nCompartment)
            #Return error if there are not an equal amount of stations and tank allocations
            if length(stations) != length(tanks)
                throw("Error: There are $(length(stations)) station and $(length(tanks)) tank allocations on day $day for a route using vehicle $k")
            end

            #Return error if there are not an equal amount of stations and compartment allocations
            if length(stations) != length(comps)
                throw("Error: There are $(length(stations)) station and $(length(comps)) compartment allocations on day $day for a route using vehicle $k")
            end

            for i = 1:length(stations)  
                #Return error if inserting a station that is not in the set of possible stations
                if stations[i] ∉ 2:nStation
                    throw("Error: Station $(stations[i]) is not in the set of possible stations on day $day for a route using vehicle $k")
                end 
            
                #Return error if the station is closed on the day we want to deliver fuel to the station
                if day in StationsArray[stations[i]].Closed_Days 
                    throw("Error: Station $(stations[i]) is closed on day $day using vehicle $k")
                end

                #Return error if a vehicle is not qualified for a given station
                if k ∉ StationsArray[stations[i]].Allowed_Vehicles
                    throw("Error: vehicle $k is not qualified for station $(stations[i]) (Error on day $day)")
                end

                #Return error if the length of tanks[i] and comps[i] is not the same
                if length(tanks[i]) != length(comps[i]) 
                    throw("Error: There are $(length(tanks[i])) tanks but $(length(comps[i])) compartment allocations to these tanks (error on day $day for vehicle $k)")
                end

                #Return error if the length of tanks and amounts are not the same
                if length(tanks[i]) != length(amounts[i]) 
                    throw("Error: There are $(length(tanks[i])) tanks and $(length(amounts[i])) amounts (Error on day $day for vehicle $k)") 
                end

                for a = 1:length(tanks[i])
                    #Return error if the inserting a tank at a station where the tank does not exist
                    if tanks[i][a] ∉ 1:StationsArray[stations[i]].nTank
                        throw("Error: Tank $(tanks[i][a]) does not exist for station $(stations[i]) (Error on day $day for vehicle $k)")
                    end

                    #Return error if amounts put in tanks does not match the amount in the compartments

                    if round(amounts[i][a],digits=3) != round(sum(comps_amount[i][a]),digits=3)
                        throw("Error: The amount ($(amounts[i][a])L) in tank $(tanks[i][a]) on station $(stations[i]) does not match the amount ($(sum(comps_amount[i][a]))L) in the allocated compartments ($(comps[i][a])) on vehicle $(k) (Error on day $day)")
                    end

                    for c = 1:length(comps[i][a])
                        #Return error if inserting a compartment that is not valid for the given vehicle
                        if comps[i][a][c] ∉ 1:Vehicles[k].nCompartment
                            throw("Error: Compartment $(comps[i][a][c]) does not exist for vehicle $k (Error on day $day)")
                        end
                        #Return error if there are more compartments amounts than actual compartments
                        if length(comps[i][a]) != length(comps_amount[i][a])
                            throw("Error: For station $(temp_route.Station[i]) there are $(length(comps[i][a])) compartments and $(length(comps_amount[i][a])) compartment amounts (Error on day $day for vehicle $k)")
                        end

                        #Error if the total capacity for each compartment is exceeded
                        amount_in_each_compartment[comps[i][a][c]] += comps_amount[i][a][c]
                    end

                    #Return error if there are two different products in one compartment
                    for c in comps[i][a]
                        product[i,a,c] = StationsArray[stations[i]].Tanks[tanks[i][a]].ProductNumber
                    end
                end
            end 
            
            #Return error if there are two different products in one compartment
            for c  = 1:8
                for i = 1:length(stations), j = 1:length(stations), a = 1:5, aa = 1:5
                    if !(product[i,a,c] == product[j,aa,c] || product[i,a,c] == 0 || 0 == product[j,aa,c])
                        throw("Error: Product $(product[i,a,c]) and $(product[j,aa,c]) are both in compartment $c on vehicle $k")
                    end
                end
            end

            #Error if the total capacity for each compartment is exceeded
            for c = 1:length(amount_in_each_compartment)
                if amount_in_each_compartment[c] > 0.01+Vehicles[k].VolCompartment[c]
                    throw("Error: Maximum level on vehicle $k in compartment $c is exceeded by $(amount_in_each_compartment[c]- Vehicles[k].VolCompartment[c]) (Error on day $day)")
                end
            end

            #Return error if weight limit is exceeded of the vehicle
            if Vehicles[k].WeightCap+0.001 < sum(comps_amount[i][a][c]*Products[ProductIDs[StationsArray[stations[i]].Tanks[tanks[i][a]].ProductNumber]].Density for i =1:length(stations) for a = 1:length(tanks[i]) for c = 1:length(comps[i][a]))
                throw("Error: The weight cap for vehicle $k has been exceeded by $(sum(comps_amount[i][a][c]*Products[ProductIDs[StationsArray[stations[i]].Tanks[tanks[a]].ProductNumber]].Density for i =1:length(stations) for a = 1:length(tanks[i]) for c = 1:length(comps[i][a]))-Vehicles[k].WeightCap ) on day $day")
            end
        end
        #Error codes stop

        obj_diff=0
        amount_diff=0
        route = Route([],[],[],0,k,[],[],0,0)

        for i = 1:length(stations)
            obj_diff2,amount_diff2=Insert_Station(route,stations[i],tanks[i],amounts[i],comps[i],comps_amount[i],curlvl[stations[i]],day)
            obj_diff+=obj_diff2
            amount_diff+=amount_diff2
        end
        if rNum==0
            push!(routes_dt,route)
        else
            insert!(routes_dt,rNum,route)
        end

        return obj_diff, amount_diff
    end

    #Inserts a single given station and its amounts
    function Insert_Station(route, station, tanks, amounts, comps, comps_amount, curlvlstat, day)
        #Error codes start
        if error_codes==1
            product = zeros(length(route.Station)+1,5,8)
            amount_in_each_compartment = zeros(Vehicles[route.Vehicle].nCompartment)
            #Return error if inserting a station that is not in the set of possible stations
            if station ∉ 2:nStation
                throw("Error: Station $station is not in the set of possible stations (Error on day $day for vehicle $(route.Vehicle))")  
            end 

            #Return error if the station is closed on the day we want to deliver fuel to the station
            if day in StationsArray[station].Closed_Days
                throw("Error: Station $(station) is closed on day $day (For vehicle $(route.Vehicle))")    
            end

            #Return error if a vehicle is not qualified for a given station
            if route.Vehicle ∉ StationsArray[station].Allowed_Vehicles
                throw("Error: vehicle $(route.Vehicle) is not qualified for station $station (Error on day $day)")
            end

            #Return error if the length of tanks[i] and comps[i] is not the same
            if length(tanks) != length(comps)
                throw("Error: There are $(length(tanks)) tanks but $(length(comps)) compartment allocations to these tanks (Error on day $day for vehicle $(route.Vehicle))")
            end

            for a = 1:length(tanks)
                #Return error if the inserting a tank at a station where the tank does not exist
                if tanks[a] ∉ 1:StationsArray[station].nTank
                    throw("Error: Tank $(tanks[a]) does not exist for station $station (Error on day $day for vehicle $(route.Vehicle))") 
                end

                #Return error if amounts put in tanks does not match the amount in the compartments
                if round(amounts[a],digits=3) != round(sum(comps_amount[a]),digits=3)
                    throw("Error: The amount ($(amounts[a])L) in tank $(tanks[a]) on station $station does not match the amount ($(sum(comps_amount[a]))L) in the allocated compartments ($(comps[a])) on vehicle $(route.Vehicle) (Error on day $day)")
                end

                for c = 1:length(comps[a])
                    #Return error if inserting a compartment that is not valid for the given vehicle
                    if comps[a][c] ∉ 1:Vehicles[route.Vehicle].nCompartment
                        throw("Error: Compartment $(comps[a][c]) does not exist for vehicle $(route.Vehicle) (Error on day $day)")
                    end

                    #Return error if there are more compartments amounts than actual compartments
                    if length(comps[a]) != length(comps_amount[a])
                        throw("Error: There are $(length(comps[a])) compartment(s) and $(length(comps_amount[a])) compartment amounts (Error on day $day for vehicle $(route.Vehicle))")
                    end
                end
            end
            
            #Creating a temporary route to base calculations on
            temp_route = deepcopy(route)
            push!(temp_route.Station, station)
            push!(temp_route.Tank,tanks)
            push!(temp_route.Amount,amounts)
            push!(temp_route.Comps,comps)
            push!(temp_route.CompsAmount,comps_amount)

            #Return error if there are not an equal amount of stations and tank allocations
            if length(temp_route.Station) != length(temp_route.Tank)
                throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Station)) station(s) but $(length(temp_route.Tank)) tank allocation(s) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are not an equal amount of stations and compartment allocations
            if length(temp_route.Station) != length(temp_route.Comps)
                throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Station)) station(s) but $(length(temp_route.Comps)) compartment allocation(s) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are two different products in one compartment
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c in temp_route.Comps[i][a]
                        product[i,a,c] = StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber
                    end
                end
            end
            for c  = 1:8
                for i = 1:length(temp_route.Station), j = 1:length(temp_route.Station), a = 1:5, aa = 1:5
                    if !(product[i,a,c] == product[j,aa,c] || product[i,a,c] == 0 || 0 == product[j,aa,c])
                        throw("Error: Product $(product[i,a,c]) and $(product[j,aa,c]) are both in compartment $c on vehicle $(route.Vehicle) (Error on day $day)")
                    end
                end
            end

            #Error if the total capacity for each compartment is exceeded
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c = 1:length(temp_route.Comps[i][a])
                        amount_in_each_compartment[temp_route.Comps[i][a][c]] += temp_route.CompsAmount[i][a][c]
                    end
                end
            end
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c = 1:length(amount_in_each_compartment)
                        if amount_in_each_compartment[c] > 0.01+Vehicles[route.Vehicle].VolCompartment[c]
                            throw("Error: Maximum level on vehicle $(route.Vehicle) in compartment $c is exceeded by $(amount_in_each_compartment[c]-Vehicles[route.Vehicle].VolCompartment[c])L (Error on day $day)")
                        end
                    end
                end
            end

            for i = 1:length(temp_route.Station)
                #Return error if the length of tanks[i] and comps[i] is not the same
                if length(temp_route.Tank) != length(temp_route.Comps) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tanks but $(length(temp_route.Comps[i])) compartment allocations to these tanks (Error on day $day for vehicle $(route.Vehicle))")
                end
                #Return error if the length of tanks and amounts are not the same
                if length(temp_route.Tank[i]) != length(temp_route.Amount[i]) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tanks and $(length(temp_route.Amount[i])) amounts (Error on day $day for vehicle $(route.Vehicle))")
                end
            end
            #Return error if weight limit is exceeded of the vehicle
            if Vehicles[temp_route.Vehicle].WeightCap+0.001 < sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Station) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]))
                throw("Error: The weight cap for vehicle $(temp_route.Vehicle) has been exceeded by $(sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Station) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]))-Vehicles[temp_route.Vehicle].WeightCap )")
            end

            #Return error if amounts put in tanks does not match the amount in the compartments
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    if round(temp_route.Amount[i][a],digits=3) != round(sum(temp_route.CompsAmount[i][a]),digits=3)
                        throw("Error: The amount ($(temp_route.Amount[i][a])L) in tank $(temp_route.Tank[i][a]) on station $(temp_route.Station[i])) does not match the amount ($(sum(temp_route.CompsAmount[i][a]))L) in the allocated compartments ($(temp_route.Comps[i][a])) on vehicle $(route.Vehicle) (Error on day $day)")
                    end
                end
            end
        end
        #Error codes stop

        obj_diff=0
        amount_diff=0
        if station in route.Station
            for a = 1:length(tanks)
                obj_diff2,amount_diff2 = Insert_Tank(route, station, tanks[a], amounts[a],comps[a],comps_amount[a],curlvlstat,day)
                obj_diff+=obj_diff2
                amount_diff+=amount_diff2
            end  
        else
                    
            push!(route.Station, station)
            push!(route.Tank,tanks)
            push!(route.Amount,amounts)
            push!(route.Comps,comps)
            push!(route.CompsAmount,comps_amount)
            amount_diff = sum(sum.(amounts))
            route.TotalAmount += amount_diff

            if length(route.Station)==1
                obj_diff = DistMatrix[1,station]+DistMatrix[station,1]

                route.RouteDist += obj_diff
                route.RouteTime += amount_diff*fuel_flow_rate + 600.0 + TimeMatrix[1,station]+TimeMatrix[station,1]
            else
                obj_diff = DistMatrix[route.Station[end-1],station]+DistMatrix[station,1]-DistMatrix[route.Station[end-1],1]

                route.RouteDist += obj_diff
                route.RouteTime += amount_diff*fuel_flow_rate + 600.0 + TimeMatrix[route.Station[end-1],station]+TimeMatrix[station,1]-TimeMatrix[route.Station[end-1],1]
            end

            for t = 1:length(tanks)
                curlvlstat[tanks[t],day:end] .+= amounts[t]
            end

        end
        
        obj_diff2 = Optimize_Route_Order(route)
        obj_diff+=obj_diff2

        return obj_diff, amount_diff
    end

    #Inserts a single given tank and its amounts
    function Insert_Tank(route, station, tank, amount, comps, comps_amount, curlvlstat, day)
        #Error Codes Start
        if error_codes==1 && !(tank in route.Tank[findall(x->x==station,route.Station)[1]])
            product = zeros(length(route.Station),5,8)
            amount_in_each_compartment = zeros(Vehicles[route.Vehicle].nCompartment)
            #Return error if inserting a station that is not in the set of possible stations
            if station ∉ 2:nStation
                throw("Error: Station $station is not in the set of possible stations (Error on day $day for vehicle $(route.Vehicle))")
            end 

            #Return error if the station is closed on the day we want to deliver fuel to the station
            if day in StationsArray[station].Closed_Days 
                throw("Error: Station $(station) is closed on day $day (Error for vehicle $(route.Vehicle))")
            end

            #Return error if a vehicle is not qualified for a given station
            if route.Vehicle ∉ StationsArray[station].Allowed_Vehicles
                throw("Error: vehicle $(route.Vehicle) is not qualified for station $station (Error on day $day)")
            end
           
            #Return error if the inserting a tank at a station where the tank does not exist
            if tank ∉ 1:StationsArray[station].nTank
                throw("Error: Tank $tank does not exist for station $station")
            end

            #Return error if amounts put in tanks does not match the amount in the compartments
            if round(amount,digits=5) != round(sum(comps_amount),digits=5)
                throw("Error: The amount ($(amount)L) in tank $tank on station $station does not match the amount ($(sum(comps_amount))L) in the allocated compartments ($(comps)) on vehicle $(route.Vehicle) (Error on day $day)")
            end

            for c = 1:length(comps)
                #Return error if inserting a compartment that is not valid for the given vehicle
                if comps[c] ∉ 1:Vehicles[route.Vehicle].nCompartment
                    throw("Error: Compartment $(comps[c]) does not exist for vehicle $(route.Vehicle) (Error on day $day)")
                end

                #Return error if there are more compartments amounts than actual compartments
                if length(comps) != length(comps_amount)
                    throw("Error: There are $(length(comps)) compartment(s) and $(length(comps_amount)) compartment amount(s) (Error on day $day for vehicle $(route.Vehicle))")
                end
            end
            
            
            #Creating a temporary route to base calculations on
            temp_route = deepcopy(route)
            #find the index of the station
            station_index = findall(x->x==station, temp_route.Station)[1]
            push!(temp_route.Tank[station_index],tank)
            push!(temp_route.Amount[station_index],amount)
            push!(temp_route.Comps[station_index],comps)
            push!(temp_route.CompsAmount[station_index],comps_amount)

            #Return error if there are not an equal amount of stations and tank allocations
            if length(temp_route.Station) != length(temp_route.Tank)
                throw("Error: There are $(length(temp_route.Station)) station(s) and $(length(temp_route.Tank)) tank allocation(s) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are not an equal amount of stations and compartment allocations
            if length(temp_route.Station) != length(temp_route.Comps)
                throw("Error: There are $(length(temp_route.Station)) station(s) and $(length(temp_route.Comps)) compartment allocation(s) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are two different products in one compartment
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c in temp_route.Comps[i][a]
                        product[i,a,c] = StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber
                    end
                end
            end
            for c  = 1:8
                for i = 1:length(temp_route.Station), j = 1:length(temp_route.Station), a = 1:5, aa = 1:5
                    if !(product[i,a,c] == product[j,aa,c] || product[i,a,c] == 0 || 0 == product[j,aa,c])
                        throw("Error: Product $(product[i,a,c]) and $(product[j,aa,c]) are both in compartment $c on vehicle $(temp_route.Vehicle) (Error on day $day)")
                    end
                end
            end

            #Error if the total capacity for each compartment is exceeded
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c = 1:length(temp_route.Comps[i][a])
                        amount_in_each_compartment[temp_route.Comps[i][a][c]] += temp_route.CompsAmount[i][a][c]
                    end
                end
            end
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c = 1:length(amount_in_each_compartment)
                        if amount_in_each_compartment[c] > 0.01+Vehicles[route.Vehicle].VolCompartment[c]
                            throw("Error: Maximum level on vehicle $(temp_route.Vehicle) in compartment $c is exceeded by $(amount_in_each_compartment[c]-Vehicles[temp_route.Vehicle].VolCompartment[c])L (Error on day $day)")
                        end
                    end
                end
            end

            for i = 1:length(temp_route.Station)
                #Return error if the length of tanks[i] and comps[i] is not the same
                if length(temp_route.Tank[i]) != length(temp_route.Comps[i]) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tank(s) but $(length(temp_route.Comps[i])) compartment allocation(s) to these tanks (Error on day $day for vehicle $(route.Vehicle))")
                end
                #Return error if the length of tanks and amounts are not the same
                if length(temp_route.Tank[i]) != length(temp_route.Amount[i]) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tank(s) and $(length(temp_route.Amount[i])) amount(s) (Error on day $day for vehicle $(route.Vehicle))")
                end
            end
            #Return error if weight limit is exceeded of the vehicle
            if Vehicles[temp_route.Vehicle].WeightCap+0.001 < sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Station) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]))
                throw("Error: The weight cap for vehicle $(temp_route.Vehicle) has been exceeded by $(sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Stations) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]))-Vehicles[temp_route.Vehicle].WeightCap) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if amounts put in tanks does not match the amount in the compartments
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    if round(temp_route.Amount[i][a],digits=3) != round(sum(temp_route.CompsAmount[i][a]),digits=3)
                        throw("Error: The amount ($(temp_route.Amount[i][a])L) in tank $(temp_route.Tank[i][a]) on station $(temp_route.Station[i])) does not match the amount ($(sum(temp_route.CompsAmount[i][a]))L) in the allocated compartments ($(temp_route.Comps[i][a])) on vehicle $(route.Vehicle) (Error on day $day)")
                    end
                end
            end
        end
        #Error Codes Stop
        
        obj_diff=0
        amount_diff=0
        index = findall(x->x == station, route.Station)[1]

        if tank in route.Tank[index]
            for c=1:length(comps)
                obj_diff2,amount_diff2 = Insert_Compartment(route, station, tank, comps[c], comps_amount[c], curlvlstat, day)
                obj_diff+=obj_diff2
                amount_diff+=amount_diff2
            end
        else
            push!(route.Tank[index]       ,         tank)
            push!(route.Amount[index]     ,       amount)
            push!(route.Comps[index]      ,       comps )
            push!(route.CompsAmount[index], comps_amount)

            route.TotalAmount += amount
            route.RouteTime += amount*fuel_flow_rate

            curlvlstat[tank,day:end] .+= amount

            amount_diff=amount
        end
        
        return obj_diff, amount_diff
    end

    #Inserts a single given compartment and its amounts
    function Insert_Compartment(route, station, tank, comp, comp_amount, curlvlstat, day) 
        #Error Codes Start
        if error_codes==1 && !(comp in route.Comps[findall(x->x==station,route.Station)[1]][findall(x->x==tank,route.Tank[findall(x->x==station,route.Station)[1]])[1]])
            product = zeros(length(route.Station),5,8)
            amount_in_each_compartment = zeros(Vehicles[route.Vehicle].nCompartment)
            #Return error if inserting a station that is not in the set of possible stations
            if station ∉ 2:nStation
                throw("Error: Station $station is not in the set of possible stations (Error on day $day for vehicle $(route.Vehicle))")
            end 

            #Return error if the station is closed on the day we want to deliver fuel to the station
            if day in StationsArray[station].Closed_Days 
                throw("Error: Station $(station) is closed on day $day (Error for vehicle $(route.Vehicle))")
            end

            #Return error if a vehicle is not qualified for a given station
            if route.Vehicle ∉ StationsArray[station].Allowed_Vehicles
                throw("Error: vehicle $(route.Vehicle) is not qualified for station $station (Error on day $day)")
            end

            #Return error if the length of tanks[i] and comps[i] is not the same
            if length(tank) != length(comp)
                throw("Error: For station $(station) there are $(length(tank)) tanks but $(length(comp)) compartment allocations to these tanks (Error on day $day for vehicle $(route.Vehicle))")
            end

            
            #Return error if the inserting a tank at a station where the tank does not exist
            if tank ∉ 1:StationsArray[station].nTank
                throw("Error: Tank $tank does not exist for station $station (Error on day $day for vehicle $(route.Vehicle))")
            end

            
            #Return error if inserting a compartment that is not valid for the given vehicle
            if comp ∉ 1:Vehicles[route.Vehicle].nCompartment
                throw("Error: Compartment $(comp) does not exist for vehicle $(route.Vehicle) (Error on day $day)")
            end

            #Return error if there are more compartments amounts than actual compartments
            if length(comp) != length(comp_amount)
                throw("Error: There are $(length(comp)) compartments and $(length(comp_amount)) compartment amounts which should be 1 (Error on day $day for vehicle $(route.Vehicle))")
            end
            
            #Creating a temporary route to base calculations on
            temp_route = deepcopy(route)
            #find the index of station and tank
            station_index = findall(x->x==station, temp_route.Station)[1]
            tank_index = findall(x->x==tank, temp_route.Tank[station_index])[1]
            temp_route.Amount[station_index][tank_index] += comp_amount
            push!(temp_route.Comps[station_index][tank_index],comp)
            push!(temp_route.CompsAmount[station_index][tank_index],comp_amount)

            #Return error if there are not an equal amount of stations and tank allocations
            if length(temp_route.Station) != length(temp_route.Tank)
                throw("Error: There are $(length(temp_route.Station)) station(s) and $(length(temp_route.Tank)) tank allocation(s) (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are not an equal amount of stations and compartment allocations
            if length(temp_route.Station) != length(temp_route.Comps)
                throw("Error: There are $(length(temp_route.Station)) station and $(length(temp_route.Comps)) compartment allocations (Error on day $day for vehicle $(route.Vehicle))")
            end

            #Return error if there are two different products in one compartment
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c in temp_route.Comps[i][a]
                        product[i,a,c] = StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber
                    end
                end
            end

            for c  = 1:8
                for i = 1:length(temp_route.Station), j = 1:length(temp_route.Station), a = 1:length(temp_route.Tank[i]), aa = 1:length(temp_route.Tank[j])
                    if !(product[i,a,c] == product[j,aa,c] || product[i,a,c] == 0 || 0 == product[j,aa,c])
                        throw("Error: Product $(product[i,a,c]) and $(product[j,aa,c]) are both in compartment $c on vehicle $(temp_route.Vehicle) (Error on day $day)")
                    end
                end
            end

            #Error if the total capacity for each compartment is exceeded
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    for c = 1:length(temp_route.Comps[i][a])
                        amount_in_each_compartment[temp_route.Comps[i][a][c]] += temp_route.CompsAmount[i][a][c]
                    end
                end
            end
            
            for c = 1:length(amount_in_each_compartment)
                if amount_in_each_compartment[c] > 0.01+Vehicles[route.Vehicle].VolCompartment[c]
                    throw("Error: Maximum level on vehicle $(temp_route.Vehicle) in compartment $c is exceeded by $(amount_in_each_compartment[c]-Vehicles[temp_route.Vehicle].VolCompartment[c])L (Error on day $day)")
                end
            end


            for i = 1:length(temp_route.Station)
                #Return error if the length of tanks[i] and comps[i] is not the same
                if length(temp_route.Tank[i]) != length(temp_route.Comps[i]) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tank(s) but $(length(temp_route.Comps[i])) compartment allocations  to these tanks (Error on day $day for vehicle $(route.Vehicle))")
                end
                #Return error if the length of tanks and amounts are not the same
                if length(temp_route.Tank[i]) != length(temp_route.Amount[i]) 
                    throw("Error: For station $(temp_route.Station[i]) there are $(length(temp_route.Tank[i])) tanks and $(length(temp_route.Amount[i])) amounts (Error on day $day for vehicle $(route.Vehicle))")
                    
                end
            end
            #Return error if weight limit is exceeded of the vehicle
            if Vehicles[temp_route.Vehicle].WeightCap+0.001 < sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[temp_route.Station[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Station) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]))
                throw("Error: The weight cap for vehicle $(temp_route.Vehicle) has been exceeded by $(sum(temp_route.CompsAmount[i][a][c]*Products[ProductIDs[StationsArray[stations[i]].Tanks[temp_route.Tank[i][a]].ProductNumber]].Density for i =1:length(temp_route.Stations) for a = 1:length(temp_route.Tank[i]) for c = 1:length(temp_route.Comps[i][a]) )-Vehicles[temp_route.Vehicle].WeightCap ) (Error on day $day for vehicle $(route.Vehicle))")
                
            end

            #Return error if amounts put in tanks does not match the amount in the compartments
            for i = 1:length(temp_route.Station)
                for a = 1:length(temp_route.Tank[i])
                    if round(temp_route.Amount[i][a],digits=3) != round(sum(temp_route.CompsAmount[i][a]),digits=3)
                        throw("Error: The amount ($(temp_route.Amount[i][a])L) in tank $(temp_route.Tank[i][a]) on station $(temp_route.Station[i])) does not match the amount ($(sum(temp_route.CompsAmount[i][a]))L) in the allocated compartments ($(temp_route.Comps[i][a])) on vehicle $(route.Vehicle) (Error on day $day)")           
                    end
                end
            end
        end
        #Error Codes End
        
        index_station = findall(x->x == station, route.Station)[1]
        index_tank    = findall(x->x == tank, route.Tank[index_station[1]])[1]

        if comp in route.Comps[index_station][index_tank]
            index_comp    = findall(x->x == comp, route.Comps[index_station][index_tank])[1]
            if  route.CompsAmount[index_station][index_tank][index_comp] < -comp_amount
                println("Error: Trying to remove $comp_amount but there is only ",route.CompsAmount[index_station][index_tank][index_comp]," left")
                println("Error at day $day station $station tank $tank comp $comp")
                return "E","E"
            end

            route.CompsAmount[index_station][index_tank][index_comp]+=comp_amount
        else
            push!(route.Comps[index_station][index_tank],comp) 
            push!(route.CompsAmount[index_station][index_tank],comp_amount)
        end
        
        route.Amount[index_station][index_tank] += comp_amount
        
        route.TotalAmount += comp_amount
        route.RouteTime += comp_amount*fuel_flow_rate

        curlvlstat[tank,day:end] .+= comp_amount

        obj_diff=0
        amount_diff = comp_amount

        

        return obj_diff, amount_diff
    end
end

#Clean functions
if true
    #Moves any routes that can be moved to the next day if possible
    function Clean_Push_Routes(solution)
        for kkk=1:nDays
            solution = deepcopy(solution)
            for d = 1:nDays,t = 1:Trips[3706].nTimePeriod[d]
                nDeleted=0
                for r=1:length(solution.Routes[d,t])
                    Route=solution.Routes[d,t][r-nDeleted]
                    Stats = Route.Station
                    nStat = length(Stats)
                    for i=1:nStat
                        statNumber = Stats[i]
                        Tanks = Route.Tank[i]
                        nTank = length(Tanks)
                        for a=1:nTank 
                            tankNumber = Tanks[a]
                            if solution.CurrentLevel[statNumber][tankNumber,d]-Route.Amount[i][a]<0.0
                                #println("Escaped!")
                                @goto escape
                            elseif i==nStat && a==nTank 
                                if d==nDays
                                    ObjChange,AmountChange=Delete_Route(solution.Routes[d,t],r-nDeleted,solution.CurrentLevel,d)
                                    solution.Objective+=ObjChange
                                    nDeleted+=1
                                    # println("This one right here officer: d $d, t $t, r $r,")
                                    @goto escape
                                else
                                    for tt = 1:Trips[3706].nTimePeriod[d+1]
                                        Remain,Used = Available_Vehicles(solution.Routes[d+1,tt],d+1,tt)

                                        if !(d+1 in Stations[Index_To_StatID[statNumber]].Closed_Days) && Route.RouteTime<Remain[Route.Vehicle]
                                            ObjChange,AmountChange=Insert_Route(  solution.Routes[d+1,tt],
                                                Stats,
                                                Route.Tank, 
                                                Route.Amount, 
                                                Route.Vehicle, 
                                                Route.Comps,
                                                Route.CompsAmount,
                                                solution.CurrentLevel,
                                                d+1,
                                                0)
                                            solution.Objective+=ObjChange

                                            # println("This one right here officer: d $d, t $t, r $r,")
                                            ObjChange,AmountChange=Delete_Route(solution.Routes[d,t],r-nDeleted,solution.CurrentLevel,d)
                                            solution.Objective+=ObjChange
                                            nDeleted+=1
                                            @goto escape
                                        end
                                    end
                                end
                            end
                        end  
                    end
                    @label escape 
                end
            end
        end
    end

    #Removes all routes that can be removed without getting under current level
    function Clean_Remove_Route(solution)
        total_obj_diff = 0
        total_amount_diff = 0
        for (k,v) in solution.Routes
            i = 1

            removed = 0
            while i <= length(v)-removed
                Sol_Temp_routes = deepcopy(solution.Routes[k])
                Sol_Temp_Currentlevel = deepcopy(solution.CurrentLevel)

                obj_diff, fill_diff = Delete_Route(Sol_Temp_routes,i,Sol_Temp_Currentlevel,k[1])
                
                if minimum(minimum.(Sol_Temp_Currentlevel)) >= 0
                    obj_diff, fill_diff = Delete_Route(solution.Routes[k],i,solution.CurrentLevel,k[1])#Delete_Route(solution.Routes[k],i,solution.CurrentLevel,k[1])
                    solution.Objective += obj_diff
                    total_obj_diff += obj_diff
                    total_amount_diff += fill_diff

                    removed += 1 
                    # println("Route $i on day: $(k[1]), time period: $(k[2]) ")
                end
                i += 1
            end

        end
        
        return total_obj_diff,total_amount_diff,1
    end

    #Removes all stations that can be removed without getting under current level
    function Clean_Remove_Station(solution)
        for (k,v) in solution.Routes
            for i = 1:length(v)
                j = 1
                removed = 0
                while j <= length(v[i].Station)-removed

                    Sol_Temp = deepcopy(solution)

                    obj_diff,amount_diff = Delete_Station(Sol_Temp.Routes[k],Sol_Temp.Routes[k][i],v[i].Station[j],Sol_Temp.CurrentLevel[v[i].Station[j]],k[1])
                    Sol_Temp.Objective += obj_diff
                    if minimum(minimum.(Sol_Temp.CurrentLevel)) >= 0
                        obj_diff,amount_diff = Delete_Station(solution.Routes[k],solution.Routes[k][i],v[i].Station[j],solution.CurrentLevel[v[i].Station[j]],k[1])
                        solution.Objective += obj_diff

                        removed += 1 
                        # println("Removed Station")
                    end
                    
                    j += 1
                end
                
            end
        end
        return obj_diff,amount_diff
    end
    
    #Delivers as much fuel as possible to tanks at stations already on the route
    function Clean_Fill_Route(route,CurrentLevel,day)
        min_empty_deliver = 3000.0
        total_obj_diff=0
        total_amount_diff=0
        
        for i=shuffle(1:length(route.Station))
            station = route.Station[i]
            curlvlstat = CurrentLevel[station]
            curlvlstatcopy = deepcopy(curlvlstat)
            for a = shuffle(1:length(route.Tank[i]))
                CompCapacity,CompProducts,weightLeft = Compartment_Capacity(route)
                tank = route.Tank[i][a]
                product = StationsArray[station].Tanks[tank].ProductNumber
                AvailComps=intersect(findall(x->x>0,CompCapacity),findall(x->x==product || x==0,CompProducts))
                AvailCompsEmpty=intersect(findall(x->x>0,CompCapacity),findall(x->x==x==0,CompProducts))
                AvailCompsExcess=intersect(findall(x->x>0,CompCapacity),findall(x->x==product,CompProducts))
                amounts=0
                comps=[]
                comps_amount=[]

                for i=1:length(AvailCompsExcess)
                    fuelAmount = max(0,min(
                            CompCapacity[AvailCompsExcess[i]],
                            StationsArray[station].Tanks[tank].realCap-maximum(curlvlstatcopy[tank,day:nDays]+StationsArray[station].Tanks[tank].Consumption[day:nDays]),
                            weightLeft/Products[ProductIDs[product]].Density))
                    if fuelAmount>0.01
                        curlvlstatcopy[tank,day:end].+=fuelAmount
                        amounts+=fuelAmount
                        weightLeft-=fuelAmount*Products[ProductIDs[product]].Density
                        # Insert_Compartment(route,station,tank,AvailCompsExcess[i],comps_amount,curlvlstat)
                        push!(comps,AvailCompsExcess[i])
                        push!(comps_amount,fuelAmount)
                    end
                end
                for i=1:length(AvailCompsEmpty)
                    fuelAmount = max(0,min(
                            CompCapacity[AvailCompsEmpty[i]],
                            StationsArray[station].Tanks[tank].realCap-maximum(curlvlstatcopy[tank,day:nDays]+StationsArray[station].Tanks[tank].Consumption[day:nDays]),
                            weightLeft/Products[ProductIDs[product]].Density))

                    if fuelAmount>min_empty_deliver || fuelAmount==CompCapacity[AvailCompsEmpty[i]]
                        curlvlstatcopy[tank,day:end].+=fuelAmount
                        amounts+=fuelAmount
                        weightLeft-=fuelAmount*Products[ProductIDs[product]].Density
                        push!(comps,AvailCompsEmpty[i])
                        push!(comps_amount,fuelAmount)
                    end
                end
                if length(comps)>0
                    obj_diff,amount_diff = Insert_Tank(route, station, tank, amounts, comps, comps_amount, curlvlstat, day)
                    # println("Inserted tank at station $station, tank $tank, day $day, with $amount_diff extra")
                    total_obj_diff+=obj_diff
                    total_amount_diff+=amount_diff
                end
            end
        end
        return total_obj_diff,total_amount_diff
    end
end

#Destroy functions
if true
    #Destroys a random compartment from the solution
    function Destroy_Remove_Compartments(solution)
        total_obj_diff = 0
        total_amount_diff = 0
        number_of_compartments = 10
        routes = solution.Routes

        for i = 1:number_of_compartments
            d,t = Convert_Indices_To_Time(rand(1:50))
            if length(solution.Routes[d,t]) <1
                @goto no_routes_left
            end 
            r = rand(1:length(solution.Routes[d,t]))

            route = routes[d,t][r]

            index_station = rand(1:length(route.Station))
            Station = route.Station[index_station]
            index_tank = rand(1:length(route.Tank[index_station]))
            Tank = route.Tank[index_station][index_tank]
            
            if length(route.Comps[index_station][index_tank])>= 1
                index_compartment = rand(1:length(route.Comps[index_station][index_tank]))
                Comp = route.Comps[index_station][index_tank][index_compartment]
            
                currentLevelStat = solution.CurrentLevel[Station]
                obj_diff,amount_diff = Delete_Compartment(routes[d,t],route,Station,Tank,Comp,currentLevelStat,d)
                total_obj_diff += obj_diff
                total_amount_diff += amount_diff
            end
        end
        @label no_routes_left

        solution.Objective += total_obj_diff

        return total_obj_diff, total_amount_diff
    end

    #Removes a random route from the solution
    function Destroy_Remove_Random_Route(solution)
        index=rand(1:nTimePeriods)
        day,time=Convert_Indices_To_Time(index)
        routes = solution.Routes
        curlvl = solution.CurrentLevel
        rNumber=length(routes[day,time])
        
        while rNumber==0
            index=rand(1:nTimePeriods)
            day,time=Convert_Indices_To_Time(index)
            rNumber=length(routes[day,time])
        end

        rNum = rand(1:rNumber)
        obj_diff,amount_diff=Delete_Route(routes[day,time],rNum,curlvl,day)
        solution.Objective+=obj_diff

        return obj_diff,amount_diff,[(day,time)],rNum
    end

    #Finds the route where the vehicle is filled least by percentage and removes it
    function Destroy_Remove_Least_Filled_Route(solution)
        minFill = 10.0
        minDayTime = (0,0)
        minRouteNumber = 0
        fill_percent = 0
        for (dayTime,routes) in solution.Routes
            for i=1:length(routes)
                fill_percent = routes[i].TotalAmount/Vehicles[routes[i].Vehicle].VolCap

                if fill_percent<minFill
                    minFill=fill_percent
                    minDayTime=dayTime
                    minRouteNumber=i
                end
            end
        end

        if minDayTime==(0,0)
            println("Error: No routes left in solution")
        end
        obj_diff,amount_diff = Delete_Route(solution.Routes[minDayTime],minRouteNumber,solution.CurrentLevel,minDayTime[1])
        solution.Objective+=obj_diff

        return obj_diff,amount_diff,minDayTime,minRouteNumber
    end

    #Destroys the solution by creating a good route and inserting it in the solution,
    #thereby most likely exceeding the max capacity.
    function Destroy_Add_Good_Route(solution)
        stations = []
        amounts = []
        product_in_amounts = []
        
        #! Edit here
        exceed_percentage = 1.8
        number_of_stations = 7

        d,t = Convert_Indices_To_Time(rand(1:50))
        b = 1 
        i = 0
        while  1 == b
            i = rand(2:nStation)
            if d ∉ StationsArray[i].Closed_Days
                break
            end
        end

        push!(stations,i)
        push!(amounts,min.(0.9*collect(StationsArray[i].Tanks[a].realCap for a = 1:StationsArray[i].nTank),collect(exceed_percentage*StationsArray[i].Tanks[a].realCap -solution.CurrentLevel[i][a,d] for a = 1:StationsArray[i].nTank)))
        push!(product_in_amounts, collect(StationsArray[i].Tanks[a].ProductNumber for a = 1:StationsArray[i].nTank))
        

        list_of_vehicles = intersect(StationsArray[i].Allowed_Vehicles,[VehicleIDs;39211;39231])
        k = list_of_vehicles[rand(1:length(list_of_vehicles))]

        capacity = sum(Vehicles[k].VolCompartment) - sum(exceed_percentage*StationsArray[i].Tanks[a].realCap -solution.CurrentLevel[i][a,d] for a = 1:StationsArray[i].nTank)
        weight = 0
        while sum(capacity) > 1000 
            best_length = Inf*ones(number_of_stations)
            best_index  = zeros(Int64,number_of_stations)
            for j in setdiff(2:nStation,i)
                if d ∉ StationsArray[j].Closed_Days && k ∈ StationsArray[j].Allowed_Vehicles
                    length = DistMatrix[i,j] + DistMatrix[j,1] - DistMatrix[1,i]
                    if sum(length .< best_length) >= 1
                        better = findfirst(length .< best_length)
                        best_length[better] = length
                        best_index[better] = j
                    end
                end
            end
            chosen_station = rand(1:number_of_stations)
            push!(stations, best_index[chosen_station])
            capacity -= sum(exceed_percentage*StationsArray[best_index[chosen_station]].Tanks[a].realCap -solution.CurrentLevel[best_index[chosen_station]][a,d] for a = 1:StationsArray[best_index[chosen_station]].nTank)
        
            push!(amounts,min.(0.9*collect(StationsArray[best_index[chosen_station]].Tanks[a].realCap for a = 1:StationsArray[best_index[chosen_station]].nTank),collect(exceed_percentage*StationsArray[best_index[chosen_station]].Tanks[a].realCap -solution.CurrentLevel[best_index[chosen_station]][a,d] for a = 1:StationsArray[best_index[chosen_station]].nTank)))
            push!(product_in_amounts, collect(StationsArray[best_index[chosen_station]].Tanks[a].ProductNumber for a = 1:StationsArray[best_index[chosen_station]].nTank))
        end

            capacity_in_compartments = Vehicles[k].VolCompartment
            
            stations,tanks,amounts_in_tanks,comps,comps_amount = Optimal_Compartment_To_Stations_Allocation(stations,amounts,product_in_amounts,capacity_in_compartments,k)
            
            obj_diff, amount_diff = Insert_Route(solution.Routes[d,t], stations, tanks, amounts_in_tanks, k, comps, comps_amount, solution.CurrentLevel, d,0)

            solution.Objective += obj_diff

        return obj_diff,amount_diff

    end

end

#Repair functions
if true
    #Repair minimum level violations by finding the cheapest way to raise the current level to be feasible, 
    #either by adding a stop to an existing route (at the violation day or any days before) or by adding a new route
    function Repair_Closest_Tank(solution)
        total_obj_diff = 0
        total_amount_diff = 0
        #Minimum volume delivered from an empty compartment
        #Set this to 12000 to only only use full compartments
        #! Edit here
        min_empty_deliver = 4000.0
        

        #Minimum volume remaining after delivering up to 0 currentlevel, eg. 2000.0 above 0
        #calculated as at least minVolume-currentLevel, so if currentLevel is -500 and mV 3000 then minimum 3500 volume delivered
        #println(minVolume)
        #! Edit here
        minVolume = 0.0
        count = [0,0]
        countt = [0,0,0,0,0,0,0,0,0,0,0]



        for station=shuffle(2:nStation)
            index=findall(x->x<0,solution.CurrentLevel[station])
            if length(index)>0
                # println("index length $(length(index))")
            end
            while length(index)>0
                # println("perms $perms")

                unique_index=unique(x->x[1],index)[1]
                tank=unique_index[1]
                violation_day=unique_index[2]
                product = StationsArray[station].Tanks[tank].ProductNumber
                # println("i,a,d $station $tank $violation_day p $product lvl $(round(Int64,solution.CurrentLevel[station][tank,violation_day]))")

                curlvlstatcopy = deepcopy(solution.CurrentLevel[station])
                extraLength=1000000

                minRNum = 0
                minTime = 0
                minDay  = 0
                
                for day=setdiff(1:violation_day,StationsArray[station].Closed_Days),time=1:Trips[3706].nTimePeriod[day]
                    routes_dt = solution.Routes[day,time]
                    #Finds the minimum extra length
                    for i = 1:length(routes_dt)
                        curRoute = routes_dt[i]
                        if !(curRoute.Vehicle in StationsArray[station].Allowed_Vehicles) || length(curRoute.Station)>10
                            #println()
                            continue

                        end
                         
                        # CompCapacity,CompProducts,weightLeft = Compartment_Capacity(curRoute)

                        # AvailComps=[]
                        # for cc=1:length(CompCapacity)
                        #     if CompCapacity[cc]>0 && (CompProducts[cc]==product || CompProducts[cc]==0)
                        #         push!(AvailComps,cc)
                        #     end
                        # end
                        
                        CompCapacity,AvailComps,weightLeft = Compartment_Capacity_Product(curRoute,product)


                        countt[1+length(AvailComps)]+=1
                        # println("length(Comps)")
                        # print(length(AvailComps))

                        if length(AvailComps)==0
                            continue
                        end

                        AvailVolume=sum(CompCapacity[AvailComps])

                        max_volume_deliverable = minimum(-curlvlstatcopy[tank,day:violation_day].-StationsArray[station].Tanks[tank].Consumption[day:violation_day].+StationsArray[station].Tanks[tank].realCap)

                        if AvailVolume<-curlvlstatcopy[tank,violation_day]+minVolume || -curlvlstatcopy[tank,violation_day]+minVolume>max_volume_deliverable || weightLeft/Products[ProductIDs[product]].Density<-curlvlstatcopy[tank,violation_day]+minVolume || AvailVolume<1000.0
                            continue
                        end
                        
                        # println("CompCapacity")
                        # println(CompCapacity)
                        # println("CompCapacity2")
                        # println(CompCapacity2)
                        # println("AvailComps")
                        # println(AvailComps)
                        # println("AvailComps2")
                        # println(AvailComps2)

                        newStats = collect(permutations(union(curRoute.Station,station)))

                        for j = 1:length(newStats)
                            newStat = [1;newStats[j];1]
                            newLength = 0
                            for k = 1:length(newStat)-1
                                newLength += DistMatrix[newStat[k],newStat[k+1]]
                            end
            
                            newDiff=newLength-curRoute.RouteDist
            
                            if newDiff < extraLength
                                extraLength = newDiff
                                minRNum = i
                                minTime = time
                                minDay = day
                                if extraLength<0.01
                                    @goto escape
                                end 
                            end
                        end
                    end
                end
                @label escape
                newRouteLength = DistMatrix[1,station]+DistMatrix[station,1]
                #Insert route if it incurs less objective change, otherwise insert station on found route

                if newRouteLength < extraLength
                    maxKTime = -50000
                    min_time_period = 0
                    k=0
                    day = violation_day
                    for time=1:Trips[3706].nTimePeriod[day]
                        routes_dt = solution.Routes[day,time]
                        remain,u=Available_Vehicles(routes_dt,day,time)

                        kTime=sort(collect(filter(x->x.first in StationsArray[station].Allowed_Vehicles,remain)), by=x->(-x.second,x.first))[1][2]

                        if kTime>maxKTime
                            maxKTime=kTime
                            min_time_period = time
                            k=sort(collect(filter(x->x.first in StationsArray[station].Allowed_Vehicles,remain)), by=x->(-x.second,x.first))[1][1]
                        end
                    end

                    CompCapacity = Vehicles[k].VolCompartment
                    # CompProducts = zeros(Vehicles[k].nCompartment)
                    weightLeft = Vehicles[k].WeightCap
                    amounts=0
                    comps=[]
                    comps_amount=[]
                    time = min_time_period
                    
                    for c=1:length(CompCapacity)
                        fuelAmount = max(0,min(
                            CompCapacity[c],
                            StationsArray[station].Tanks[tank].realCap-curlvlstatcopy[tank,day]-StationsArray[station].Tanks[tank].Consumption[day],
                            weightLeft/Products[ProductIDs[product]].Density))

                        if fuelAmount==CompCapacity[c] || fuelAmount==StationsArray[station].Tanks[tank].realCap-curlvlstatcopy[tank,day]-StationsArray[station].Tanks[tank].Consumption[day] && fuelAmount > 0.01
                            curlvlstatcopy[tank,day:end].+=fuelAmount
                            amounts+=fuelAmount
                            weightLeft-=fuelAmount*Products[ProductIDs[product]].Density
                            push!(comps,c)
                            push!(comps_amount,fuelAmount)
                        end
                    end
                    # routeTime = 1500+TimeMatrix[1,station]+TimeMatrix[station,1]+fuel_flow_rate*amounts

                    curlvlTemp=fill(Array{Float64}(undef, 0, 0),nStation)
                    curlvlTemp[station]=solution.CurrentLevel[station]
                    
                    count[1]+=1
                    obj_diff,amount_diff = Insert_Route(solution.Routes[day,time], [station], [[tank]], [[amounts]], k, [[comps]], [[comps_amount]], curlvlTemp, day,0)

                    #println("Insert route with $station with obj $obj_diff at d,t $day, $time ")
                    # CompCapacity,CompProducts,weightLeft = Compartment_Capacity(solution.Routes[day,time][end])

                else
                    day = minDay
                    route=solution.Routes[day,minTime][minRNum]

                    CompCapacity,AvailComps,weightLeft = Compartment_Capacity_Product(route,product)

                    # CompCapacity,CompProducts,weightLeft = Compartment_Capacity(route)
                    # AvailComps=[]
                    # for cc=1:length(CompCapacity)
                    #     if CompCapacity[cc]>0 && (CompProducts[cc]==product || CompProducts[cc]==0)
                    #         push!(AvailComps,cc)
                    #     end
                    # end
                    # println("AvailComps")
                    # println(AvailComps)
                    AvailCompsEmpty=[]
                    AvailCompsExcess=[]
                    for aa=1:length(AvailComps)
                        compp=AvailComps[aa]
                        if CompCapacity[compp]==Vehicles[route.Vehicle].VolCompartment[compp]
                            push!(AvailCompsEmpty,compp)
                        else
                            push!(AvailCompsExcess,compp)
                        end
                    end
                    # println("AvailCompsEmpty1")
                    # println(AvailCompsEmpty)
                    # AvailCompsEmpty=intersect(findall(x->x>0,CompCapacity2),findall(x->x==0,CompProducts))
                    # println("AvailCompsEmpty2")
                    # println(AvailCompsEmpty)
                    # println("AvailCompsExcess1")
                    # println(AvailCompsExcess)
                    # AvailCompsExcess=intersect(findall(x->x>0,CompCapacity2),findall(x->x==product,CompProducts))
                    # println("AvailCompsExcess2")
                    # println(AvailCompsExcess)
                    countt[1+length(AvailComps)]+=1

                    amounts=0
                    comps=[]
                    comps_amount=[]

                    for i=1:length(AvailCompsExcess)
                        max_volume_deliverable = minimum(-curlvlstatcopy[tank,day:violation_day].-StationsArray[station].Tanks[tank].Consumption[day:violation_day].+StationsArray[station].Tanks[tank].realCap)

                        fuelAmount = max(0,min(
                                CompCapacity[AvailCompsExcess[i]],
                                max_volume_deliverable,
                                weightLeft/Products[ProductIDs[product]].Density))
                        if fuelAmount>0.01
                            curlvlstatcopy[tank,day:end].+=fuelAmount
                            amounts+=fuelAmount
                            weightLeft-=fuelAmount*Products[ProductIDs[product]].Density
                            push!(comps,AvailCompsExcess[i])
                            push!(comps_amount,fuelAmount)
                        end
                    end
                    for i=1:length(AvailCompsEmpty)
                        max_volume_deliverable = minimum(-curlvlstatcopy[tank,day:violation_day].-StationsArray[station].Tanks[tank].Consumption[day:violation_day].+StationsArray[station].Tanks[tank].realCap)

                        fuelAmount = max(0,min(
                                CompCapacity[AvailCompsEmpty[i]],
                                max_volume_deliverable,
                                weightLeft/Products[ProductIDs[product]].Density))
                        if fuelAmount>min_empty_deliver || fuelAmount==CompCapacity[AvailCompsEmpty[i]] || length(comps)==0
                            curlvlstatcopy[tank,day:end].+=fuelAmount
                            amounts+=fuelAmount
                            weightLeft-=fuelAmount*Products[ProductIDs[product]].Density
                            push!(comps,AvailCompsEmpty[i])
                            push!(comps_amount,fuelAmount)
                        end
                    end
                    routes_dt = solution.Routes[day,minTime]

                    count[2]+=1
                    obj_diff,amount_diff = Insert_Station(route,station,[tank],[amounts],[comps],[comps_amount],solution.CurrentLevel[station],day)
                    
                    remain,u=Available_Vehicles(routes_dt,day,minTime)

                end
                
                solution.Objective += obj_diff
                total_obj_diff+=obj_diff
                total_amount_diff+=amount_diff

                index=findall(x->x<0,solution.CurrentLevel[station])
            end
        end

        # println("countt")
        # println(countt)

        return total_obj_diff,total_amount_diff
    end

    #Finds negative current levels and assigns available compartments to those tanks
    function Repair_Compartments(solution)
        count= 0
        total_obj_diff = 0
        total_amount_diff = 0
        
        for i = 2:nStation
            for a = 1:StationsArray[i].nTank
                if sum(solution.CurrentLevel[i][a,:] .< 0)>=1
                    for d = 1:nDays
                    #Change here to change so that the function can fill at all days during the 30 day time period. 
                    #if solution.CurrentLevel[i][a,d]<0
                        curlvlstat = solution.CurrentLevel[i]
                        for t = 1:Trips[3923].nTimePeriod[d]
                            for n = 1:length( solution.Routes[d,t])
                                route = solution.Routes[d,t][n]
                                if i in route.Station && a in route.Tank[findall(x->x == i,route.Station)[1]]
                                    amounts,products, weight_left = Compartment_Capacity(route)
                                    product_in_tank = StationsArray[i].Tanks[a].ProductNumber
                                    for ka = 1:length(products)
                                        if product_in_tank == products[ka] && min(StationsArray[i].Tanks[a].realCap - curlvlstat[a,d] , amounts[ka]) > 10

                                            compamount = min(StationsArray[i].Tanks[a].realCap - curlvlstat[a,d] , amounts[ka])
                                            weight_left -= compamount*Products[ProductIDs[products[ka]]].Density
                                            if weight_left <= 0 
                                                @goto weight_constraint
                                            end

                                            obj_diff,amount_diff = Insert_Compartment(route,i,a,ka,compamount,curlvlstat,d)
                                            total_obj_diff += obj_diff
                                            total_amount_diff += amount_diff

                                            # println("Filled: Station: $(i), Tank: $(a), Day: $d, Timeperiod: $t, compartment: $ka with $compamount. EXCESS")
                                        
                                            solution.Objective += obj_diff
                                        end
                                    end
                                    for ka = 1:length(products)
                                        if 0 == products[ka] && min(StationsArray[i].Tanks[a].realCap - curlvlstat[a,d] , amounts[ka]) > 10

                                            compamount = min(StationsArray[i].Tanks[a].realCap - curlvlstat[a,d] , amounts[ka])
                                            weight_left -= compamount*Products[ProductIDs[product_in_tank]].Density
                                            if weight_left < 0
                                                @goto weight_constraint
                                            end

                                            obj_diff,amount_diff = Insert_Compartment(route,i,a,ka,compamount,curlvlstat,d)
                                            # println("Filled: Station: $(i), Tank: $(a), Day: $d, Timeperiod: $t, compartment: $ka with $compamount. EMPTY")
                                            total_obj_diff += obj_diff
                                            total_amount_diff += amount_diff
                                            
                                            solution.Objective += obj_diff
                                        end
                                    end

                                    @label weight_constraint
                                end
                            end
                        end
                    end
                end
            end
        end

        return total_obj_diff,total_amount_diff
    end

    #Finds places in the solution where max capacity for a tank is violated and fixes them by first trying to delete a route,
    #then a station, then a tank, and then a compartment until it works
    function Repair_Remove_Max_Violations(solution)
        all_routes = solution.Routes
        curlvl = solution.CurrentLevel
        total_obj_diff = 0
        total_amount_diff = 0
        # count = [0,0,0,0,0,0]
        max_tanks = findmax.(curlvl[i][a,:].-StationsArray[i].Tanks[a].realCap.+StationsArray[i].Tanks[a].Consumption[1:nDays] for i=2:nStation for a=1:StationsArray[i].nTank)
        max_tanks_index = findall(x->x[1]>0.01,max_tanks)
        max_stat_tank=collect(Tank_To_Station[max_tanks_index[i]] for i=1:length(max_tanks_index))
        
        if length(max_tanks_index) == 0
            return 0,0
        end

        max_capacity_violation = findmax(findmax.(curlvl[max_stat_tank[i][1]][max_stat_tank[i][2],:].-StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].realCap.+StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].Consumption[1:nDays] for i=1:length(max_stat_tank)))
        #println(max_capacity_violation)
        violation_amount = max_capacity_violation[1][1] 
        violation_day = max_capacity_violation[1][2]
        station,tank = max_stat_tank[max_capacity_violation[2]]

        #Used to fix the problem with capacity exceeded in day 1
        max_capacity_change = Dict()

        tank_routes = filter(x->x[2]<=violation_day,Support_All_Tank_Routes(solution,station,tank))
        counter = 0
        while violation_amount>0.001 
           # println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
           # println(violation_amount)
            counter+=1
            if counter>=200
                
                return "exception_reset", counter

            end
            #First try to delete a route
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                routes = all_routes[day,time]
                route = routes[rNum]
                

                min_before = minimum(minimum.(curlvl[route.Station]))
                
                obj_diff, fill_diff = Delete_Route(routes,rNum,curlvl,day)
                
                if minimum(minimum.(curlvl[route.Station])) >= minimum([0,min_before])
                    total_obj_diff += obj_diff
                    total_amount_diff += fill_diff
                    #count[1]+=1
               
                    @goto escape
                end
                obj_diff , _ = Insert_Route(routes,route.Station,route.Tank,route.Amount,route.Vehicle,route.Comps,route.CompsAmount,curlvl,day,rNum)
            end
            curlvlstat = curlvl[station]
            #If that doesn't work then try to delete a station instead
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                routes = all_routes[day,time]
                route = routes[rNum]
                if length(route.Station)==1
                    continue
                end

                statIndex=findall(x->x==station,route.Station)[1]
                
                tanks = route.Tank[statIndex]
                amounts = route.Amount[statIndex]
                comps = route.Comps[statIndex]
                compsAmounts = route.CompsAmount[statIndex]
                
                min_before = minimum(curlvlstat[route.Tank[statIndex],day:end])
               
                obj_diff, fill_diff = Delete_Station(routes,route,station,curlvlstat,day)                
                total_obj_diff += obj_diff
          
                if minimum(curlvlstat[tanks,day:end]) >= minimum([0,min_before])

                    total_amount_diff += fill_diff
                    ##count[2]+=1
                   
                    @goto escape
                end

                obj_diff , _ = Insert_Station(route,station,tanks,amounts,comps,compsAmounts,curlvlstat,day)
                total_obj_diff += obj_diff

            end
            #If that doesn't work then try to delete a tank instead
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                routes = all_routes[day,time]
                route = routes[rNum]

                statIndex=findall(x->x==station,route.Station)[1]
                if length(route.Tank[statIndex])==1
                    continue
                end

                tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]

                comps = route.Comps[statIndex][tankIndex]
                compsAmounts = route.CompsAmount[statIndex][tankIndex]
                
                min_before = minimum(curlvlstat[tank,day:end])

                obj_diff, fill_diff = Delete_Tank(routes,route,station,tank,curlvlstat,day)              

                if minimum(curlvlstat[tank,day:end]) >= minimum([0,min_before])
                    
                    total_obj_diff += obj_diff
                    total_amount_diff += fill_diff
                    #count[3]+=1
                    
                    @goto escape
                end

                _ , _ = Insert_Tank(route,station,tank,-fill_diff,comps,compsAmounts,curlvlstat,day)

            end
            #If that doesn't work then try to delete a compartment instead
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                routes = all_routes[day,time]
                route = routes[rNum]

                statIndex=findall(x->x==station,route.Station)[1]
                tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]
                if length(route.Comps[statIndex][tankIndex])==1
                    continue
                end

                for c in shuffle(1:length(route.Comps[statIndex][tankIndex]))
                    compartment = route.Comps[statIndex][tankIndex][c]
                    min_before = minimum(curlvlstat[tank,day:end])
                    
                    obj_diff, fill_diff = Delete_Compartment(routes,route,station,tank,compartment,curlvlstat,day)

                    if minimum(curlvlstat[tank,day:end]) >= minimum([0,min_before])

                        total_obj_diff += obj_diff
                        total_amount_diff += fill_diff
                        #count[4]+=1
                       
                        @goto escape
                    end
                    _ , _ = Insert_Compartment(route,station,tank,compartment,-fill_diff,curlvlstat,day)

                end
            end
            #If that doesn't work then try to remove exactly the violation amount from a compartment
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                route = all_routes[day,time][rNum]

                statIndex=findall(x->x==station,route.Station)[1]
                tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]

                for c in shuffle(1:length(route.Comps[statIndex][tankIndex]))
                    compartment = route.Comps[statIndex][tankIndex][c]
                    min_before = minimum(curlvlstat[tank,day:end])
                    
                    if route.CompsAmount[statIndex][tankIndex][c]>violation_amount

                        obj_diff, fill_diff = Insert_Compartment(route,station,tank,compartment,-violation_amount,curlvlstat,day)

                        if minimum(curlvlstat[tank,day:end]) >= minimum([0,min_before])
    
                            total_obj_diff += obj_diff
                            total_amount_diff += fill_diff
                            #count[5]+=1
                           
                            @goto escape
                        end
                        _ , _ = Insert_Compartment(route,station,tank,compartment,violation_amount,curlvlstat,day)
                    end                
                end
            end
            #If that doesn't work then delete a compartment at violation day anyways at random, making the solution infeasible by min
            for i in shuffle(1:length(tank_routes))
                day,time,rNum = tank_routes[i][2],tank_routes[i][3],tank_routes[i][4]
                route = all_routes[day,time][rNum]

                statIndex=findall(x->x==station,route.Station)[1]
                tankIndex=findall(x->x==tank,route.Tank[statIndex])[1]
    
                for c in shuffle(1:length(route.Comps[statIndex][tankIndex]))
                    compartment = route.Comps[statIndex][tankIndex][c]
                        
                    if day==violation_day 

                        obj_diff, fill_diff = Delete_Compartment(all_routes[day,time],route,station,tank,compartment,curlvlstat,day)
                        
                        total_obj_diff += obj_diff
                        total_amount_diff += fill_diff

                        #count[6]+=1
                       
                        @goto escape
                    end
                end
            end
            @label escape
            #
            max_tanks = findmax.(curlvl[max_stat_tank[i][1]][max_stat_tank[i][2],:].-StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].realCap.+StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].Consumption[1:nDays] for i=1:length(max_stat_tank))
            max_tanks_index = findall(x->x[1]>0.01,max_tanks)
            if length(max_tanks_index) == 0
                return 0,0
            end
            max_stat_tank=max_stat_tank[max_tanks_index]
            # println(max_tanks)
            # println(max_tanks_index)
            max_capacity_violation = findmax(max_tanks[max_tanks_index])

            violation_amount = max_capacity_violation[1][1] 
            violation_day = max_capacity_violation[1][2]
            station,tank = max_stat_tank[max_capacity_violation[2]]

            tank_routes = filter(x->x[2]<=violation_day,Support_All_Tank_Routes(solution,station,tank))
            
            counter2 = 0
            while length(tank_routes)==0 && violation_amount>0.001
                counter2+=1
                temp_curlvl = deepcopy(curlvl)
                max_capacity_change[station,tank,violation_day]=violation_amount

                for (k,v) in max_capacity_change
                    temp_curlvl[k[1]][k[2],k[3]]-=v
                end
                max_capacity_violation = findmax(findmax.(temp_curlvl[max_stat_tank[i][1]][max_stat_tank[i][2],:].-StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].realCap.+StationsArray[max_stat_tank[i][1]].Tanks[max_stat_tank[i][2]].Consumption[1:nDays] for i=1:length(max_stat_tank)))
                #max_capacity_violation = findmax(findmax.(temp_curlvl[i][a,:].-StationsArray[i].Tanks[a].realCap.+StationsArray[i].Tanks[a].Consumption[1:nDays] for i=2:nStation for a=1:StationsArray[i].nTank))

                violation_amount = max_capacity_violation[1][1] 
                violation_day = max_capacity_violation[1][2]
                station,tank = max_stat_tank[max_capacity_violation[2]]
                
                tank_routes = filter(x->x[2]<=violation_day,Support_All_Tank_Routes(solution,station,tank))
                #println(max_tanks)
                #println(max_tanks_index)
                # println(max_capacity_violation)
                # println(station)
                # println(tank)
                # println(tank_routes)
            end

        end
        
        solution.Routes = all_routes
        solution.CurrentLevel = curlvl
        solution.Objective += total_obj_diff

        return total_obj_diff,total_amount_diff
    end
    
    #Finds places in the solution where max time for a vehicle on a day,time is violated and fixes it by several methods    
    function Repair_Remove_Time_Violations(solution)
            
        routes = solution.Routes
        curlvl = solution.CurrentLevel
        total_obj_diff = 0
        total_amount_diff = 0

        for day=nDays:-1:1,time=Trips[3706].nTimePeriod[day]:-1:1
            @label start
            routes_dt = routes[day,time]
            remain,_ = Available_Vehicles(routes_dt,day,time)
            max_time_violation = sort(collect(remain),by = x->x.second)[1]
            # println("-------")
            # println("max_time_violation d,t $day $time")
            # println(max_time_violation)
            vehicle = max_time_violation[1]
            violation_amount = max_time_violation[2] 
            if violation_amount>0.0
                # println("No problemo B)")
                continue
            end
            vehicle_routes_index = findall(x->x==vehicle || x==vehicle*10+1 || x==(vehicle-1)/10,collect(routes_dt[i].Vehicle for i=1:length(routes_dt)))

            #First we try to move it to a different time period on the same day
            for time2 in setdiff(1:Trips[3706].nTimePeriod[day],time)
                remain_time2,_ = Available_Vehicles(routes[day,time2],day,time2)
                remain_time2_vehicle = remain_time2[vehicle]
                # println("remain_time2 $remain_time2_vehicle $time2")
                for r in shuffle(vehicle_routes_index)
                    route = routes_dt[r]

                    if remain_time2_vehicle > route.RouteTime > -violation_amount
                        obj_diff,amount_diff = Insert_Route(routes[day,time2],route.Station,route.Tank,route.Amount,route.Vehicle,route.Comps,route.CompsAmount,curlvl,day,0)
                        total_obj_diff+=obj_diff
                        total_amount_diff+=amount_diff

                        obj_diff,amount_diff = Delete_Route(routes_dt,r,curlvl,day)
                        total_obj_diff+=obj_diff
                        total_amount_diff+=amount_diff
                        
                        # println("Time period shift")
                        @goto escape
                    end
                end
            end
            #If that doesn't work then we try to push a route back 1 day with same amounts
            if day>1
                remain_max = 0
                time_max = 1
                for times in 1:Trips[3706].nTimePeriod[day-1]
                    remain_time2,_ = Available_Vehicles(routes[day-1,times],day-1,times)
                    # println("remain_time2[vehicle] time $times")
                    # println(remain_time2[vehicle])
                    if remain_time2[vehicle]>remain_max
                        remain_max = remain_time2[vehicle]
                        time_max   = times
                    end
                end
                
                for r in shuffle(vehicle_routes_index)
                    route = routes_dt[r]
                    # println("remain_max $remain_max")
                    # println("route.RouteTime ",route.RouteTime)
                    if remain_max > route.RouteTime && sum(collect(day-1 in StationsArray[route.Station[i]].Closed_Days for i=1:length(route.Station)))==0
                        # println(route)
                        obj_diff,amount_diff = Insert_Route(routes[day-1,time_max],route.Station,route.Tank,route.Amount,route.Vehicle,route.Comps,route.CompsAmount,curlvl,day-1,0)
                        total_obj_diff+=obj_diff
                        total_amount_diff+=amount_diff
                        obj_diff,amount_diff = Delete_Route(routes_dt,r,curlvl,day)
                        total_obj_diff+=obj_diff
                        total_amount_diff+=amount_diff
                        
                        # println("Pushback")
                        @goto escape
                    end
                end
                #If that doesn't work then we push the route with lowest time back anyways
                min_route_index = vehicle_routes_index[findmin(collect(routes_dt[i].RouteTime for i in vehicle_routes_index))[2]]
                # println("route")
                # println(route)
                # println("vehicle_routes_index")
                # println(vehicle_routes_index)
                # println("min_route_index")
                # println(min_route_index)

                route = routes_dt[min_route_index]
                itt=0
                while sum(collect(day-1 in StationsArray[route.Station[i]].Closed_Days for i=1:length(route.Station)))!=0
                    route = routes_dt[rand(vehicle_routes_index[1:length(vehicle_routes_index)])]
                    itt+=1
                    if itt==100
                        @goto delete
                    end
                end
                
                obj_diff,amount_diff = Insert_Route(routes[day-1,time_max],route.Station,route.Tank,route.Amount,route.Vehicle,route.Comps,route.CompsAmount,curlvl,day-1,0)
                total_obj_diff+=obj_diff
                total_amount_diff+=amount_diff
                obj_diff,amount_diff = Delete_Route(routes_dt,min_route_index,curlvl,day)
                total_obj_diff+=obj_diff
                total_amount_diff+=amount_diff

                # println("Last resort pushback")
                @goto escape
            
            end
            if day==1
                @label delete
                rNum = rand(vehicle_routes_index)
                route = routes_dt[rNum]
                # println("Time violation in day 1 panic slightly less aaaaaaaaaah")
                
                obj_diff,amount_diff = Delete_Route(routes_dt,rNum,curlvl,day)
                total_obj_diff+=obj_diff
                total_amount_diff+=amount_diff

                @goto escape
            end
            
            @label escape
            
            remain,_ = Available_Vehicles(routes_dt,day,time)

            max_time_violation = sort(collect(remain),by = x->x.second)[1]
            # println("max_time_violation 2 d,t $day, $time")
            # println(max_time_violation)
            # println("-------")
            if max_time_violation[2]<0.0
                @goto start
            end

        end
        solution.Objective+=total_obj_diff

        return total_obj_diff,total_amount_diff
    end
end

function Calc_CurLVL(solution)
    newCur = fill(zeros(1,1),nStation)
    for i = 2:nStation
        newCur[i] = zeros(StationsArray[i].nTank,nDays)
    end
    for d=1:nDays,t = 1:Trips[3706].nTimePeriod[d],r=1:length(solution.Routes[d,t])
        route = solution.Routes[d,t][r]
        for i=1:length(route.Station),a=1:length(route.Tank[i])
            stat=route.Station[i]
            tank=route.Tank[i][a]
            newCur[stat][tank,d:end].+=route.Amount[i][a]
        end
    end
    for d=1:nDays,i=2:nStation,a=1:StationsArray[i].nTank
        if d==1
            newCur[i][a,d:end].+=StationsArray[i].Tanks[a].StartLevel-StationsArray[i].Tanks[a].Min
        end
        newCur[i][a,d:end].-=StationsArray[i].Tanks[a].Consumption[d]
    end
    return newCur
end

function Remove_Consumption(currentLevel)
    newCur = deepcopy(currentLevel)
    for d=1:nDays,i=2:nStation,a=1:StationsArray[i].nTank
        newCur[i][a,d:end].+=StationsArray[i].Tanks[a].Consumption[d]
    end
    return newCur
end

# Random.seed!(1)
# include("Function_Library.jl")
# error_codes=0

# include("Constructor_Heuristics.jl")
# fuel_deliv = Constructor_Single_Stop(1.0)
# Sol=Fuel_To_Struct(fuel_deliv)
# # # RRR=deepcopy(Sol.Routes[1,1][1])
# # obj_diff=0

# # println("Objective Constructor heuristic:")
# # println(Sol.Objective/1000)

# for kk=1:30
#     obj_diff,amount_diff, dayTime,rNum = Destroy_Remove_Least_Filled_Route(Sol)
# end

# # # println("Objective after destroying routes:")
# # # println(Sol.Objective/1000)

# obj_diff,amount_diff = Repair_Closest_Tank(Sol)
# obj_diff,amount_diff = Repair_Remove_Max_Violations(Sol)
# Check_Feasibility(Sol)

# it=1
# while it<1000 && Check_Feasibility(Sol)!=(1,1,1)
#     # obj_diff,amount_diff = Repair_Closest_Tank(Sol)
#     obj_diff,amount_diff = Repair_Closest_Tank(Sol)
#     println(Check_Feasibility(Sol))
#     println(Sol.Objective/1000)

#     obj_diff,amount_diff = Repair_Remove_Max_Violations(Sol)
#     println(Check_Feasibility(Sol))
#     println(Sol.Objective/1000)

#     obj_diff,amount_diff = Repair_Remove_Time_Violations(Sol)
#     println(Check_Feasibility(Sol))
#     println(Sol.Objective/1000)

#     sleep(3)
#     it+=1
# end

# println("Objective before clean: ")
# println(Sol.Objective/1000)
# # Clean_Push_Routes(Sol)
# for d=1:nDays,t=1:Trips[3706].nTimePeriod[d],r=1:length(Sol.Routes[d,t])
#     # obj_diff,amount_diff = Clean_Fill_Route(Sol.Routes[d,t][r],Sol.CurrentLevel,d)
# end

# # obj_diff,amount_diff = Clean_Remove_Route(Sol)

# # obj_diff,amount_diff = Repair_Remove_Time_Violations(Sol)

# println("Objective after clean: ")
# println(Sol.Objective/1000)
# println("Feasibility: (max, min, time) ")
# println(Check_Feasibility(Sol))



# # # Solution_Checker(Struct_To_Fuel(Sol))
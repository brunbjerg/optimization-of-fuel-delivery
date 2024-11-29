function fill_n_run(fuel_deliv,sumpCur,I,A,K,C,P,D,T,R,IT,first)
    
    if first == 1
        for i in setdiff(StationIDs,2004)
            for a = 1:Stations[i].nTank
                sumpRatio = zeros(Float64,35)
                level = zeros(Float64,36)
                cur =  Stations[i].Tanks[a].StartLevel - Stations[i].Tanks[a].Min 
                level[1] = cur
                for d = 1:35           
                    sumpRatioTemp = Stations[i].Tanks[a].Consumption[d]/(cur )
                    if sumpRatioTemp<=0
                        sumpRatio[d] = 50 
                    else
                        sumpRatio[d] = sumpRatioTemp 
                    end
                    #cur = cur  - Stations[i].Tanks[a].Consumption[d]
                    level[d+1] = cur
                end
                sumpCur[i,a] = (1,1,Stations[i].Tanks[a].ProductNumber, sumpRatio,level) 
            end
        end
        return sumpCur
    elseif first == 0
        for d = D        
            sumpCur[I,A][5][d] = sumpCur[I,A][5][d] + fuel_deliv[I,A,K,C,P,D,T,R,IT]
            sumpRatioTemp = Stations[I].Tanks[A].Consumption[d]/(sumpCur[I,A][5][d] #=- Stations[I].Tanks[A].Consumption[d]=#)
            
            if sumpRatioTemp<=0
                sumpCur[I,A][4][d] = 50
            else
                sumpCur[I,A][4][d] = sumpRatioTemp 
            end
        end
        return sumpCur
    elseif first == -1 
        for d = D
            for i in setdiff(StationIDs,2004)
                for a = 1:Stations[i].nTank 
                    sumpCur[i,a][5][d] = sumpCur[i,a][5][d] - Stations[i].Tanks[a].Consumption[d]
                    sumpRatioTemp = Stations[i].Tanks[a].Consumption[d]/(sumpCur[i,a][5][d] #=- Stations[i].Tanks[a].Consumption[d]=#)
                    
                    if sumpRatioTemp<=0
                        sumpCur[i,a][4][d] = 50
                    else
                        sumpCur[i,a][4][d] = sumpRatioTemp 
                    end
                    sumpCur[i,a][5][d+1] = sumpCur[i,a][5][d]
                    
                    if Stations[i].Tanks[a].Consumption[d+1]/( sumpCur[i,a][5][d] #=- Stations[i].Tanks[a].Consumption[d+1]=#)<=0
                        sumpCur[i,a][4][d+1] = 50
                    else
                        sumpCur[i,a][4][d+1] = Stations[i].Tanks[a].Consumption[d+1]/( sumpCur[i,a][5][d] #=- Stations[i].Tanks[a].Consumption[d+1]=# ) 
                    end
                    #sumpCur[i,a][4][d+1] = Stations[i].Tanks[a].Consumption[d+1]/( sumpCur[i,a][5][d] - Stations[i].Tanks[a].Consumption[d+1] )
                end
            end
            
        end

        return sumpCur
    end
end

function alloc_tank(amount,k,used,p,excess,d,t,r)
    comp_vol=[]
    if !isempty(filter(x->x.first[2] == p , excess)) #&& 1==0
        usable_excesses = filter(x->x.first[2] == p , excess)
        excess_ind = collect(collect(keys(usable_excesses))[i][1] for i = 1:length(usable_excesses))
        ind  = collect(combinations(setdiff(collect(1:Vehicles[k].nCompartment), setdiff(used,excess_ind) )))
        usedComplementar = setdiff(collect(1:Vehicles[k].nCompartment), setdiff(used,ind))
        #we need to change the volume capacity of each compartment to reflect the amount that has already been used
        updated_comp_vol = deepcopy(Vehicles[k].VolCompartment)
        for (key,v) in usable_excesses
            updated_comp_vol[key[1]] = v 
            delete!(excess,key)
        end
        comp_vol = deepcopy(updated_comp_vol)
        comb = collect(combinations(updated_comp_vol[ usedComplementar] ))
    else
        ind  = collect(combinations(setdiff(collect(1:Vehicles[k].nCompartment),used )))
        usedComplementary = setdiff(collect(1:Vehicles[k].nCompartment), used)
        comb = collect(combinations(Vehicles[k].VolCompartment[ usedComplementary] ))
        comp_vol = deepcopy(Vehicles[k].VolCompartment)
    end

    if sum(comb[end]) < amount
        return comb[end], ind[end],comp_vol,excess
    end
    minIndex = 0 
    minDiff = 1000000
    for i =1 :length(comb)
        diff = sum(comb[i] ) - amount
        if diff < minDiff && diff >= 0
            minDiff = diff
            minIndex = i
        end 
    end

    return comb[minIndex], ind[minIndex], comp_vol, excess
end

function calculate_time_of_route(fuel_deliv,k,d,t,r)
    
    stops = []

    route = unique(x->(x.first[1],x.first[2] ),filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t && x.first[8]==r ,fuel_deliv))
    for it = 1:length(route)
        push!(stops,route[it][1][[1,9]])
    end
    stops = sort(stops,by = x->x[2])

    stops = [(2004,0); stops; (2004,length(stops)+1 )]

    route_time = 15*60 + 10*60*(length(stops)-2)

    for i = 1:length(stops)-1
        route_time += TimeDict[stops[i][1],stops[i+1][1]]
    end

    fill_time = sum(values(filter(x->x.first[3]== k &&
                                     x.first[6]== d && 
                                     x.first[7]== t &&
                                     x.first[8]== r,fuel_deliv)))/(2700/60)


    return route_time + fill_time
end

function find_station(sumpCur,c_sta, threshold,d,it,used_tanks,k)
    max_inter = 0
    max = 0
    sta = 0
    tank =0
    
    for i in setdiff(StationIDs,2004)
        for a  in setdiff(collect(1:Stations[i].nTank),  used_tanks[i] ) 
            if it==1  && Stations[i].Tanks[a].realCap -sumpCur[i,a][5][d] >1000 && !(d in Stations[i].Closed_Days) && (k in Stations[i].Allowed_Vehicles || k*10+1 in Stations[i].Allowed_Vehicles )
                if max_inter <=sumpCur[i,a][4][d]
                    max_inter = sumpCur[i,a][4][d]
                    sta = i
                    tank = a
                    max = max_inter
                end
            else 
                if max_inter <= sumpCur[i,a][4][d]/(DistDict[c_sta,i]+1) && Stations[i].Tanks[a].realCap -sumpCur[i,a][5][d]>1000 && !(d in Stations[i].Closed_Days) && (k in Stations[i].Allowed_Vehicles )
                    max_inter = sumpCur[i,a][4][d]/(DistDict[c_sta,i]+1)
                    sta = i
                    tank = a
                    max = max_inter*(DistDict[c_sta,i]+1)
                end
            end
        end
    end
    if sta != 0
        if k*10+1 in Stations[sta].Allowed_Vehicles && length(Stations[sta].Allowed_Vehicles)==2 && it ==1 # && k<10000
            k = k*10+1
        end
    end
    return max, sta, tank,k

end

function Constructor_Multiple_Stop(threshold)
    VehicleIDs = [3923, 3921, 3920, 3706]
    threshold = [0.5; 0.0*ones(10)]
    fuel_deliv = Dict(); sumpCur = Dict() ; sumpCur = fill_n_run(fuel_deliv,sumpCur,1,1,1,1,1,1,1,1,1,1)
    total_excess = 0
    comp_vol=[]
    for d = 1:30
        for k in VehicleIDs, t =1:Trips[k].nTimePeriod[d]
            k_ori = k
            r = 1
            time_of_routes = 0

            used_tanks = Dict()
            for l in StationIDs
                used_tanks[l] = []
            end

            while time_of_routes < Trips[k].TimePeriodLengths[d][t]  && find_station(sumpCur,2004,threshold,d,1,used_tanks,k)[1]>threshold[1]
                it = 1;used = []
                k = k_ori
                total_weight = 0
                c_sta = 2004
                excess = Dict()
                while length(used) < Vehicles[k].nCompartment && find_station(sumpCur,c_sta,threshold,d,it,used_tanks,k)[1]>threshold[it]
                    
                    max, sta, tank,k = find_station(sumpCur,c_sta,threshold,d,it,used_tanks,k)
                    append!(used_tanks[sta] , tank ) 
                    c_sta = sta

                    current  = sumpCur[sta,tank][5][d]
                    capacity = Stations[sta].Tanks[tank].realCap

                    comb_cap,comb_ind,comp_vol,excess = alloc_tank(capacity - current ,k, used,Stations[sta].Tanks[tank].ProductNumber,excess,d,t,r)

                    for c in comb_ind
                        compartment = deepcopy(comp_vol[c])
                        
                        fill_need = capacity - current        

                        if compartment <= fill_need
                            current_weight = total_weight
                            total_weight += compartment*Products[ProductIDs[Stations[sta].Tanks[tank].ProductNumber]].Density

                            if total_weight >= Vehicles[k].WeightCap
                                filling = (Vehicles[k].WeightCap - current_weight)/Products[ProductIDs[Stations[sta].Tanks[tank].ProductNumber]].Density
                                fuel_deliv[sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it] = filling
                                sumpCur = fill_n_run(fuel_deliv,sumpCur,sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it,0)
                                @goto label
                            end
                            fuel_deliv[sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it] = compartment
                            sumpCur = fill_n_run(fuel_deliv,sumpCur,sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it,0)
                            current = current + compartment      
                        else 
                            current_weight = total_weight
                            total_weight += fill_need*Products[ProductIDs[Stations[sta].Tanks[tank].ProductNumber]].Density

                            if total_weight >= Vehicles[k].WeightCap
                                filling = (Vehicles[k].WeightCap - current_weight)/Products[ProductIDs[Stations[sta].Tanks[tank].ProductNumber]].Density
                                fuel_deliv[sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it] = filling
                                sumpCur = fill_n_run(fuel_deliv,sumpCur,sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it,0)
                                @goto label
                            end
                            fuel_deliv[sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it] = fill_need
                            sumpCur = fill_n_run(fuel_deliv,sumpCur,sta,tank,k,c,Stations[sta].Tanks[tank].ProductNumber,d,t,r,it,0)
                            current = capacity
                            excess[c,Stations[sta].Tanks[tank].ProductNumber,it] = compartment - fill_need
                            total_excess += compartment - fill_need
                        end     
                    end
                    append!(used,comb_ind)
                    it += 1
                end
                @label label

                current_time = calculate_time_of_route(fuel_deliv,k,d,t,r)
                time_of_routes += calculate_time_of_route(fuel_deliv,k,d,t,r)
                
                if time_of_routes >= Trips[k].TimePeriodLengths[d][t]
                    fuel_deliv_minus = filter(x->x.first[8]==r && x.first[3]==k && x.first[6] == d && x.first[7] == t   ,  fuel_deliv )
                    for (key,value) in fuel_deliv_minus
                        fuel_deliv_minus[key] = -value
                        staa,tankk,kk,cc,pp,dd,tt,rr,itt = key
                        sumpCur = fill_n_run(fuel_deliv_minus,sumpCur,staa,tankk,kk,cc,pp,dd,tt,rr,itt,0)
                        delete!(fuel_deliv,key)
                    end
                    
                    time_of_routes -= current_time
                    current_time = 0
                    break
                end
                r += 1 
            end
        end
        sumpCur = fill_n_run(fuel_deliv,sumpCur,1,1,1,1,1,d,1,1,1,-1)
    end

    return fuel_deliv, sumpCur

end

#------------------------------------

function alloc_tank_single(k,comps,amount)
    ind = collect(combinations(collect(1:Vehicles[k].nCompartment)[comps]))
    comp = collect(combinations(Vehicles[k].VolCompartment[comps]))
    minIndex = 0 
    minDiff = 1000000
    for i =1 :length(comp)
        diff = sum(comp[i] ) - amount
        if diff < minDiff && diff >= 0
            minDiff = diff
            minIndex = i
        end 
    end

    return comp[minIndex], ind[minIndex] 

end

function Constructor_Single_Stop(threshold)
    sumpCurr = Dict()
    timeLeft = Dict()
    timeReq = Dict()
    capLeft = Dict()
    weightLeft = Dict()
    compsLeft = Dict()
    currentLevel = Dict{Tuple{Int64,Int64,Int64},Float64}()
    fuelAmount = Dict()
    routeNumber = Dict()

    fuel_deliv=Dict{Tuple{Int64,Int64,Int64,Int64,Int64,Int64,Int64,Int64,Int64},Float64}()
    visit=0
    Obj = 0
    totalFuel=0
    # threshold=1.0
    totalTimeUsed=0

    for k in All_Vehicles
        for d = 1:nDays
            for t = 1:Trips[k].nTimePeriod[d]
                timeLeft[k,d,t]=1.0*Trips[k].TimePeriodLengths[d][t]
                routeNumber[k,d,t]=1
            end
        end
    end

    for d = 1:nDays
        for i in setdiff(StationIDs,2004)
            for a = 1:Stations[i].nTank
                if d==1
                    currentLevel[i,a,1]=Stations[i].Tanks[a].StartLevel-Stations[i].Tanks[a].Min
                else
                    currentLevel[i,a,d]=currentLevel[i,a,d-1]
                end
                sump=Stations[i].Tanks[a].Consumption[d]
                sumpCurr[i,a,d] = sump/(currentLevel[i,a,d])
                
            end
        end
        
        sumpCurList=sort(collect(filter(x->x.first[3]==d,sumpCurr)), by=x->x.second)

        mustSumpCur=sumpCurList[end][2][1]

        if sumpCurList[1][2][1]<0.0
            mustSumpCur=sumpCurList[1][2][1]
        end
        
        it=0
        while (mustSumpCur>threshold && it<=30) || (mustSumpCur<0.0 && it<=30)
            it+=1
            if mustSumpCur<0.0
                mustStat=sumpCurList[1][1][1]
            else
                mustStat=sumpCurList[end][1][1]
            end

            for k in VehicleIDs, t = 1:Trips[k].nTimePeriod[d]
                timeReq[mustStat,k,d,t]=TimeDict[2004,mustStat]+TimeDict[mustStat,2004]+25*60
                if timeReq[mustStat,k,d,t]<=timeLeft[k,d,t] && !(d in Stations[mustStat].Closed_Days) && (k in Stations[mustStat].Allowed_Vehicles || k*10+1 in Stations[mustStat].Allowed_Vehicles)
                    if !(k in Stations[mustStat].Allowed_Vehicles) && k*10+1 in Stations[mustStat].Allowed_Vehicles
                        k=k*10+1
                        timeReq[mustStat,k,d,t]=TimeDict[2004,mustStat]+TimeDict[mustStat,2004]+25*60
                    end
                    nextMust=filter(x->x.first[1]==mustStat,sumpCurList)
                    capLeft[k,d,t,routeNumber[k,d,t]]=Vehicles[k].VolCap
                    weightLeft[k,d,t,routeNumber[k,d,t]]=Vehicles[k].WeightCap
                    compsLeft[k,d,t,routeNumber[k,d,t]]= collect(1:Vehicles[k].nCompartment)

                    for a = 1:Stations[mustStat].nTank
                        #println("\n -------------------")
                        #show(stdout, "text/plain", nextMust)
                        nextTank = nextMust[end][1][2]

                        fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]] = max(0,min(
                        capLeft[k,d,t,routeNumber[k,d,t]],
                        Stations[mustStat].Tanks[nextTank].realCap-currentLevel[mustStat,nextTank,d],
                        weightLeft[k,d,t,routeNumber[k,d,t]]/Products[ProductIDs[Stations[mustStat].Tanks[nextTank].ProductNumber]].Density))

                        volCompSum=0    
                        if fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]]>0.001
                            volComps,usedComps = alloc_tank_single(k,compsLeft[k,d,t,routeNumber[k,d,t]],fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]])
                            for c = 1:length(usedComps)
                                if c==length(usedComps)
                                    fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t],usedComps[c]]=fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]]-sum(volComps[1:end-1])
                                else
                                    fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t],usedComps[c]]=volComps[c]
                                end
                            end
                            volCompSum=sum(volComps)
                            compsLeft[k,d,t,routeNumber[k,d,t]] = setdiff(compsLeft[k,d,t,routeNumber[k,d,t]],usedComps)

                        else
                            for c = 1:Vehicles[k].nCompartment
                                fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t],c]=0
                            end
                        end

                        capLeft[k,d,t,routeNumber[k,d,t]]    -= volCompSum
                        weightLeft[k,d,t,routeNumber[k,d,t]] -= fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]]*Products[ProductIDs[Stations[mustStat].Tanks[nextTank].ProductNumber]].Density
                        timeReq[mustStat,k,d,t] += fuelAmount[mustStat,nextTank,k,d,t,routeNumber[k,d,t]]/((1800+900)/60)
                        pop!(nextMust)
                    end
                    if timeReq[mustStat,k,d,t]<=timeLeft[k,d,t] && sum(values(filter(x->x.first[1]==mustStat && x.first[3]==k && x.first[4]==d && x.first[5]==t,fuelAmount)))>0.01
                        
                        timeLeft[k,d,t]-=timeReq[mustStat,k,d,t]
                        if k in Trailer_Vehicles
                            timeLeft[k*10+1,d,t]-=timeReq[mustStat,k,d,t]
                        elseif k in Rigid_Vehicles
                            timeLeft[convert(Int64,(k-1)/10),d,t]-=timeReq[mustStat,k,d,t]
                        end

                        visit+=1
                        totalTimeUsed+=timeReq[mustStat,k,d,t]
                        Obj += DistDict[2004,mustStat]+DistDict[mustStat,2004]
                        itt=1
                        currTank=0
                        
                        for a=1:Stations[mustStat].nTank
                            for c=1:Vehicles[k].nCompartment
                                if haskey(fuelAmount,(mustStat,a,k,d,t,routeNumber[k,d,t],c))
                                    if fuelAmount[mustStat,a,k,d,t,routeNumber[k,d,t],c]>0.01
                                        if currTank==0
                                            currTank=a
                                        end
                                        if currTank!=a
                                            currTank=a
                                            itt+=1
                                        end
                                        fuel_deliv[mustStat,a,k,c,Stations[mustStat].Tanks[a].ProductNumber,d,t,routeNumber[k,d,t],itt]=fuelAmount[mustStat,a,k,d,t,routeNumber[k,d,t],c]
                                        totalFuel+=fuelAmount[mustStat,a,k,d,t,routeNumber[k,d,t],c]
                                    end
                                end
                            end
                            currentLevel[mustStat,a,d]+=fuelAmount[mustStat,a,k,d,t,routeNumber[k,d,t]]
                            sump=Stations[mustStat].Tanks[a].Consumption[d]
                            sumpCurr[mustStat,a,d] = sump/(currentLevel[mustStat,a,d])
                        end
                        routeNumber[k,d,t]+=1
                        if k in Trailer_Vehicles
                            routeNumber[k*10+1,d,t]+=1
                        elseif k in Rigid_Vehicles
                            routeNumber[convert(Int64,(k-1)/10),d,t]+=1
                        end
                        break
                    end
                end
            end
            sumpCurList=sort(collect(filter(x->x.first[3]==d,sumpCurr)), by=x->x.second)
            mustSumpCur=sumpCurList[end][2][1]
            if sumpCurList[1][2][1]<0.0
                mustSumpCur=sumpCurList[1][2][1]
            end
        end

        for i in StationIDs, a = 1:Stations[i].nTank
            currentLevel[i,a,d] -= Stations[i].Tanks[a].Consumption[d]
        end
    end

    return fuel_deliv
end
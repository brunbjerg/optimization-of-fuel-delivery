using JuMP, Statistics, Printf, DataFrames, XLSX, Combinatorics, Random, BenchmarkTools

mutable struct Tank
    ID::Int64
    TankID::String
    Product::String
    Capacity::Float64
    Max::Float64
    Min::Float64
    Empty::Float64
    MinRefill::Float64
    StartLevel::Float64
    Consumption::Vector{Float64}
    ProductType::Vector{Float64}
    ProductNumber::Int64
    averageSump::Float64
    sumpCapRatio::Vector{Float64}
    averageSumpCapRatio::Float64
    #If only 1 tank is filled at a time, when does each
    #tank become a must do order:
    realCap::Float64
    mustDoOrder::Vector{Int}
end

mutable struct Station
    Name::String
    ID::Int64
    City::String
    Longitude::Float64
    Latitude::Float64
    X::Float64
    Y::Float64
    OpenHours::String
    Closed_Days::Vector{Int64}
    nTank::Any
    Allowed_Vehicles::Any
    Tanks::Any
    StationCap::Float64
end

struct Vehicle
    Name::String
    ID::Int64
    Qualifications::Any
    VolCap::Int64
    WeightCap::Int64
    nCompartment::Int64
    Scale::String
    VolCompartment::Any
end

struct Product
    Name::String
    ID::Int64
    Density::Float64
end

mutable struct Trip
    ID::Int64
    TimePeriods::Vector{Any}
    TimePeriodLengths::Vector{Any}
    TimePeriodIndex::Vector{Any}
    nTimePeriod::Vector{Int64}
end

mutable struct Route
    Station::Vector{Int64}
    Tank::Vector{Vector{Int64}}
    Amount::Vector{Vector{Float64}}
    TotalAmount::Float64
    Vehicle::Int64
    Comps::Vector{Vector{Vector{Int64}}}
    CompsAmount::Vector{Vector{Vector{Float64}}}
    RouteTime::Float64
    RouteDist::Int64
end

mutable struct Solution
    Routes::Dict{Tuple{Int64,Int64},Vector{Route}} #[d,t]
    Objective::Float64
    CurrentLevel::Vector{Matrix{Float64}}
end

println("Initializing functions...")
println(" ")

function Data_Load()
    distXL = XLSX.readxlsx("Data\\DistMatrix.xlsx")
    TimeXL = XLSX.readxlsx("Data\\TimeMatrix.xlsx")
    dataXL = XLSX.readxlsx("Data\\Data Full.xlsx")

    DistMatrix=distXL["DistMatrix"]["A1:BI61"]
    TimeMatrix=TimeXL["TimeMatrix"]["A1:BI61"]
    DistMatrix[2,1] = DistMatrix[1,2] = 2004
    TimeMatrix[2,1] = TimeMatrix[1,2] = 2004
    TimeDict=Dict()
    DistDict=Dict()
    
    nStation=60
    nVehicle=4
    nProduct=6
    nDays=30
    
    for i = 1:nStation, j = 1:nStation
        TimeDict[TimeMatrix[1,i+1],TimeMatrix[j+1,1]] = TimeMatrix[i+1,j+1]
        DistDict[DistMatrix[1,i+1],DistMatrix[j+1,1]] = DistMatrix[i+1,j+1]

    end
    

    Stations=Dict()
    Vehicles=Dict()
    Products=Dict()
    Trips=   Dict()
    dataLocation=dataXL["Locations"]["A1:N112"]
    dataTanks = dataXL["Stations and Tanks"]["A1:BF256"]
    dataVehicle=dataXL["Vehicles"]["A1:K13"]
    dataTrips=dataXL["Trips"]["C2:AG5"]
    dataProducts=dataXL["Products"]["B2:D7"]
    
    #Tanks

    Tank_1 = zeros(1,58)
    for i =2:256
        if !(dataTanks[i,5] === missing)
            Tank_1 = [Tank_1 ; reshape(dataTanks[i,:], 1,58)]
        end 
    end

    Sta_ID = zeros(195)

    Tank_structs = Vector{Tank}(undef, 195)

    for i =2:196

        Product1=convert(String, Tank_1[i,7][12:end])
        ProductType1=zeros(nProduct)
        ProductNumber=0
        if Product1=="INGO 92"
            ProductType1[1]=1
            ProductNumber=1
        elseif Product1=="INGO 95"
            ProductType1[2]=1
            ProductNumber=2
        elseif Product1=="INGO Diesel"
            ProductType1[3]=1
            ProductNumber=3
        elseif Product1=="MILES 92"
            ProductType1[4]=1
            ProductNumber=4
        elseif Product1=="MILES 95"
            ProductType1[5]=1
            ProductNumber=5
        elseif Product1=="MILES DIESEL"
            ProductType1[6]=1
            ProductNumber=6
        end

        sump=convert(Vector{Any},Tank_1[i,19:53])
        
   
        
        Max=convert(Float64,Tank_1[i,9])
        Min=convert(Float64,Tank_1[i,10])
        StartLevel=convert(Float64,Tank_1[i,15])
        realCap=Max-Min
        mustDoOrder=[]
        currLevel=StartLevel
        for jj=1:length(sump)
            currLevel-=sump[jj]
            if currLevel>Min
                append!(mustDoOrder,0)
            else
                append!(mustDoOrder,1)
                currLevel=Max
            end
        end


        Tank_structs[i-1] = Tank(convert(Int64,  Tank_1[i,3]),
                            convert(String, Tank_1[i,6]),
                            Product1,
                            convert(Float64,Tank_1[i,8]),
                            Max,
                            Min,
                            convert(Float64,Tank_1[i,11]),
                            convert(Float64,Tank_1[i,13]),
                            StartLevel,
                            sump,
                            ProductType1,
                            ProductNumber,
                            #=averageSump=# mean(Tank_1[i,19:53])   ,
                            #=sumpCapRatio=# Tank_1[i,19:53]/realCap,
                            #=averageSumpCapRatio=# mean(Tank_1[i,19:53]/realCap),
                            realCap,
                            mustDoOrder)
                            
        Sta_ID[i-1] = Tank_1[i,3]
    end 
    Sta_ID = convert(Vector{Int64},Sta_ID)
    Tanks=Dict()

    dataLocation2 = zeros(1,10)
    for i =2:112
        if !(dataLocation[i,2] === missing) && i!=6
            dataLocation2 = [dataLocation2 ; reshape(dataLocation[i,2:11], 1,10)]
        end 
    end

    dataLocation2=dataLocation2[2:nStation+1,:]

    #Initialize terminal
    Stations[dataLocation2[1,2]]=Station(dataLocation2[1,1],dataLocation2[1,2],dataLocation2[1,3],dataLocation2[1,4],dataLocation2[1,5],parse(Float64,dataLocation2[1,6]),parse(Float64,dataLocation2[1,7]),dataLocation2[1,8],[0],0,dataLocation2[1,10],[],0)
    it=1
    for i=2:nStation
        
        Name=dataLocation2[i,1]
        ID=dataLocation2[i,2]
        City=dataLocation2[i,3]
        Longitude=dataLocation2[i,4]
        Latitude=dataLocation2[i,5]
        X=parse(Float64,dataLocation2[i,6])
        Y=parse(Float64,dataLocation2[i,7])
        OpenHours = dataLocation2[i,8]
        if dataLocation2[i,8] == "24/7"
            Closed_Days=[]
        else
            Closed_Days=[parse(Int64,split(split(dataLocation2[i,8])[2],"/")[1])]
        end

        nTank=dataLocation2[i,9]
        if dataLocation2[i,10] === missing
            Allowed_Vehicles = [3706, 3920, 3921, 3923, 39211, 39231]
        elseif dataLocation2[i,10] == "Rigid or drawbar"
            Allowed_Vehicles = [3921, 3923, 39211, 39231]
        elseif dataLocation2[i,10] == "Rigid"
            Allowed_Vehicles = [39211, 39231]
        elseif dataLocation2[i,10] == "Not Rigid (nor drawbar)"
            Allowed_Vehicles = [3706, 3920]
        end
        Tanks_v=[]
        for j=1:nTank
            push!(Tanks_v,Tank_structs[it])
            it=it+1
        end
        Tanks[ID]=Tanks_v
        StationCap=0
        for jj=1:length(Tanks[ID])
            StationCap+=Tanks[ID][jj].realCap
        end
        Stations[ID]=Station(Name,ID,City,Longitude,Latitude,X,Y,OpenHours,Closed_Days,nTank,Allowed_Vehicles,Tanks[ID],StationCap)
    end

    for i in collect(keys(Stations))
        for a in 1:Stations[i].nTank
            for d = 1:nDays
                if Stations[i].OpenHours != "24/7"
                    if parse(Int64,split(split(Stations[i].OpenHours)[2],"/")[1]) == d
                        Stations[i].Tanks[a].Consumption[d-1] = deepcopy(Stations[i].Tanks[a].Consumption[d-1]) + deepcopy(Stations[i].Tanks[a].Consumption[d])
                        Stations[i].Tanks[a].Consumption[d]   = 0 
                    end
                end
            end
        end
    end

    dataVehicle2 = zeros(1,10)
    for i =2:13
        if !(dataVehicle[i,2] === missing) && i!=9
            dataVehicle2 = [dataVehicle2 ; reshape(dataVehicle[i,2:11], 1,10)]
        end 
    end

    dataVehicle2=dataVehicle2[1:end .!=1,setdiff(1:end,(3,5))]
    
    for i=1:nVehicle

        
        
        if i== 3 || i == 4
            Name=dataVehicle2[i,1] * " & Drawbar"
            ID=dataVehicle2[i,2]
            Qualifications=dataVehicle2[i,3]
            VolCap=dataVehicle2[i,4] + dataVehicle2[i+4,4]
            WeightCap=dataVehicle2[i,5] + dataVehicle2[i+4,5]
            nCompartment=dataVehicle2[i,6] + dataVehicle2[i+4,6]
            Scale=dataVehicle2[i,7]
            VolCompartment=[parse(Float64,x) for x in split(dataVehicle2[i,8]*","*dataVehicle2[i+4,8],",")]
        else
            Name=dataVehicle2[i,1]
            ID=dataVehicle2[i,2]
            Qualifications=dataVehicle2[i,3]
            VolCap= dataVehicle2[i+4,4]
            WeightCap=dataVehicle2[i+4,5]
            nCompartment=dataVehicle2[i+4,6] 
            Scale=dataVehicle2[i,7]
            VolCompartment=[parse(Float64,x) for x in split(dataVehicle2[i+4,8],",")]
        end
        Vehicles[ID]=Vehicle(Name,ID,Qualifications,VolCap,WeightCap,nCompartment,Scale,VolCompartment)
    end

    #Rigid vehicles without trailer
    for i = 3:4    
        Name=dataVehicle2[i,1]
        ID=convert(Int64,dataVehicle2[i,2]*10+1)
        Qualifications=dataVehicle2[i,3]
        Scale=dataVehicle2[i,7]


        VolCap=dataVehicle2[i,4] 
        WeightCap=dataVehicle2[i,5] 
        nCompartment=dataVehicle2[i,6] 
        VolCompartment=[parse(Float64,x) for x in split(dataVehicle2[i,8],",")]
        Vehicles[ID]=Vehicle(Name,ID,Qualifications,VolCap,WeightCap,nCompartment,Scale,VolCompartment)
    end





    for i=1:nProduct
            
        Name=dataProducts[i,1][12:end]
        ID=dataProducts[i,2]
        Density=dataProducts[i,3]

        Products[ID]=Product(Name,ID,Density)
    end



    StationIDs=dataLocation2[1:nStation,2]
    VehicleIDs=dataVehicle2[1:nVehicle,2]
    ProductIDs=dataProducts[1:nProduct,2]

    #     ID::Int64
    #     TimePeriods::Vector{Any}
    #     TimePeriodLengths::Vector{Any}
    #     nTimePeriod::Vector{Int64}

    for j=1:nVehicle
        TimePeriods=fill([],nDays)
        TimePeriodLengths=fill([],nDays)
        TimePeriodIndex=fill([],nDays)
        itt=0
        nTimePeriod=[]
        for i=1:nDays
            temp2=split(filter(x -> !isspace(x), dataTrips[j,i+1]),"+")
            append!(nTimePeriod,length(temp2))
            if length(temp2)==2
                TimePeriodIndex[i]=[itt+1,itt+2]
                itt=itt+2
            else
                TimePeriodIndex[i]=[itt+1]
                itt=itt+1
            end
            TimePeriods[i]=temp2
            temp3=[]
            for k=1:length(temp2)
                temp4=split.(split(temp2[k],"-"),":")
                temp1=[parse(Int64,x) for x in vcat(temp4[1],temp4[2])]
                time1=temp1[1]*60+temp1[2]
                time2=temp1[3]*60+temp1[4]
                if time1>time2
                    time2=time2+24*60
                end
                append!(temp3,60*(time2-time1))
            end
            TimePeriodLengths[i]=temp3
        end
        ID=dataTrips[j,1]
        Trips[ID]=Trip(ID,TimePeriods,TimePeriodLengths,TimePeriodIndex,nTimePeriod)
    end
    Trips[39211] = Trips[3921]
    Trips[39231] = Trips[3923]
    Trailer_Vehicles = [3921,3923]
    Rigid_Vehicles = [39211,39231]
    All_Vehicles = union(VehicleIDs,Rigid_Vehicles)
    ProductDict = Dict()

    ProductDict[1030841]=1
    ProductDict[1030842]=2
    ProductDict[1030900]=3
    ProductDict[1030919]=4
    ProductDict[1030921]=5
    ProductDict[1030928]=6

    StatID_To_Index = Dict()
    Index_To_StatID = Dict()
    sorted_stations = sort(StationIDs)
    for i = 1:nStation
        StatID_To_Index[sorted_stations[i]] = i
        Index_To_StatID[i] = sorted_stations[i]
    end
    DistMatrix = DistMatrix[2:nStation+1,2:nStation+1]
    TimeMatrix = TimeMatrix[2:nStation+1,2:nStation+1]
    

    StationsArray = []
    for i = 1:60
        push!(StationsArray, Stations[Index_To_StatID[i]])
    end
    
    Tank_To_Station = Dict()
    count=1
    for i=2:nStation, a=1:StationsArray[i].nTank
        Tank_To_Station[count]=(i,a)
        count+=1
    end

    return DistMatrix,TimeMatrix,DistDict,TimeDict,Stations,Vehicles,Products,Trips,StationIDs,ProductIDs,ProductDict,VehicleIDs,Trailer_Vehicles,Rigid_Vehicles,All_Vehicles,StatID_To_Index,Index_To_StatID,StationsArray,Tank_To_Station
end

function Convert_Time_To_Indices(d,t)
    if d == 1 && t == 1
        return 1
    elseif  d == 1 && t == 2
        return 2
    else
        return sum(Trips[3923].nTimePeriod[i] for i = 1:(d-1)) + t
    end
    
end

function Convert_Indices_To_Time(I)
    D = 1
    t = 0
    while I > sum(Trips[3923].nTimePeriod[i] for i = 1:D)
        D += 1 
    end
    if I == sum(Trips[3923].nTimePeriod[i] for i = 1:D) && Trips[3923].nTimePeriod[D]== 1
        t = 1
    elseif I == sum(Trips[3923].nTimePeriod[i] for i = 1:D)
        t = 2
    elseif I < sum(Trips[3923].nTimePeriod[i] for i = 1:D)
        t = 1
    end 
    return D,t
end

#=
function Fuel_To_Struct(fuel_deliv)
    Routes=Dict()
    Obj=0
    currentLevel = Dict()
    for i in setdiff(StationIDs,2004),a = 1:Stations[i].nTank
        currentLevel[i,a]=[]
        #Calculate Current Levels through the days
        for d=1:nDays
            if d==1
                push!(currentLevel[i,a],Stations[i].Tanks[a].StartLevel-Stations[i].Tanks[a].Min)
            else
                push!(currentLevel[i,a],currentLevel[i,a][d-1])
            end
            sump=Stations[i].Tanks[a].Consumption[d]

            fuelDelivered=values(filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
            if length(fuelDelivered)>0
                currentLevel[i,a][d]+=sum(fuelDelivered)
            end

            currentLevel[i,a][d]-=sump          
        end
    end

    for d=1:nDays, t = 1:Trips[3706].nTimePeriod[d]
        Routes[d,t]=[]
        for k in VehicleIDs
            if k in Trailer_Vehicles
                volUsedPeriod=filter(x->(x.first[3]==k || x.first[3]==k*10+1) && x.first[6]==d && x.first[7]==t,fuel_deliv)
            else
                volUsedPeriod=filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t,fuel_deliv)
            end
        
            if length(volUsedPeriod)!=0
                colVol=collect(volUsedPeriod)
                # println("  ")
                # println("  ")
                # show(stdout, "text/plain", sort(colVol, by=x->x.first[8]))
                # println("  ")
                # println("  ")
                for r=1:sort(colVol, by=x->x.first[8])[end].first[8]
                    Vehicle=colVol[r][1][3]
                    Tank=Dict()
                    Comps=Dict()
                    CompsAmount=Dict()
                    Amount=Dict()
                    RouteTime=0
                    RouteDist=0
                    totalAmount=0

                    #fuel_deliv[i,a,k,c,p,d,t,r,it]

                    route=sort(collect(filter(x->x[8]==r,keys(volUsedPeriod))), by=x->x[9])
                    routeAmount=sort(collect(filter(x->x.first[8]==r,volUsedPeriod)), by=x->(x.first[9],x.first[4]))
                    Stat=unique(x->x[1],first.(route))

                    for ki = 1:length(Stat)
                        Tank[Stat[ki]]=[x[2] for x in unique(x->x[2],filter(x->x[1]==Stat[ki],route))]

                        for ka = 1:length(Tank[Stat[ki]])
                            tankDeliv=filter(x->x.first[1]==Stat[ki] && x.first[2]==Tank[Stat[ki]][ka],routeAmount)

                            Comps[Stat[ki],Tank[Stat[ki]][ka]]=[x.first[4] for x in tankDeliv]

                            for ke = 1:length(Comps[Stat[ki],Tank[Stat[ki]][ka]])
                                CompsAmount[Stat[ki],Tank[Stat[ki]][ka],Comps[Stat[ki],Tank[Stat[ki]][ka]][ke]]=[x.second for x in tankDeliv][ke]
                            end
     
                            # println(" ")
                            # println(" ")
                            # show(stdout, "text/plain", Tank[Stat[ki]][ka])
                            # println(" ")
                            # println(" ")
                            Amount[Stat[ki],Tank[Stat[ki]][ka]]=sum([x.second for x in tankDeliv])
                            totalAmount+=Amount[Stat[ki],Tank[Stat[ki]][ka]]
                        end

                    end
                    
                    RouteTime+=TimeDict[2004,route[1][1]]
                    RouteTime+=TimeDict[route[end][1],2004]
                    
                    RouteDist+=DistDict[2004,route[1][1]]
                    RouteDist+=DistDict[route[end][1],2004]

                    for l=1:(length(route)-1)
                        RouteTime+=TimeDict[route[l][1],route[l+1][1]]
                        RouteDist+=DistDict[route[l][1],route[l+1][1]]
                    end
                    RouteTime+=15*60+10*60*length(Stat)+totalAmount/((1800+900)/60)
                    
                    push!(Routes[d,t],Route(Stat,Tank,Amount,totalAmount,Vehicle,Comps,CompsAmount,RouteTime,RouteDist))
                end
            end
        end       
    end
    for d=1:nDays, t=1:Trips[3706].nTimePeriod[d]
        Dists = getfield.(Routes[d,t],:RouteDist)
        if length(Dists)>0
            Obj+=sum(getfield.(Routes[d,t],:RouteDist))
        end
    end
    sol=Solution(Routes,Obj,currentLevel)
    return sol
end
=#



#! OLD_VERSION. REVERT IF ERROR
function Fuel_To_Struct(fuel_deliv)
    Routes=Dict()
    Obj=0
    SN=StatID_To_Index
    currentLevel = fill(zeros(1,1),nStation)
    for i in setdiff(StationIDs,2004)
        currentLevel[SN[i]]=zeros(Stations[i].nTank,nDays)
        for a = 1:Stations[i].nTank
            #Calculate Current Levels through the days
            
            for d=1:nDays
                if d==1
                    currentLevel[SN[i]][a,1]=Stations[i].Tanks[a].StartLevel-Stations[i].Tanks[a].Min
                else
                    currentLevel[SN[i]][a,d]=currentLevel[SN[i]][a,d-1]
                end
                sump=Stations[i].Tanks[a].Consumption[d]

                fuelDelivered=values(filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
                if length(fuelDelivered)>0
                    currentLevel[SN[i]][a,d]+=sum(fuelDelivered)
                end

                currentLevel[SN[i]][a,d]-=sump          
            end
        end
    end
    for d=1:nDays, t = 1:Trips[3706].nTimePeriod[d]
        Routes[d,t]=[]
        for k in VehicleIDs
            if k in Trailer_Vehicles
                volUsedPeriod=filter(x->(x.first[3]==k || x.first[3]==k*10+1) && x.first[6]==d && x.first[7]==t,fuel_deliv)
            else
                volUsedPeriod=filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t,fuel_deliv)
            end
        
            if length(volUsedPeriod)!=0
                colVol=collect(volUsedPeriod)
                # println("  ")
                # println("  ")
                # show(stdout, "text/plain", sort(colVol, by=x->x.first[8]))
                # println("  ")
                # println("  ")
                for r=1:sort(colVol, by=x->x.first[8])[end].first[8]
                    Tank=[]
                    Comps=[]
                    CompsAmount=[]
                    Amount=[]
                    Stats=[]
                    RouteTime=0
                    RouteDist=0

                    #fuel_deliv[i,a,k,c,p,d,t,r,it]

                    route=sort(collect(filter(x->x[8]==r,keys(volUsedPeriod))), by=x->x[9])
                    routeAmount=sort(collect(filter(x->x.first[8]==r,volUsedPeriod)), by=x->(x.first[9],x.first[4]))
                    Stat=unique(x->x[1],first.(route))

                    Vehicle=route[1][3]

                    for ki = 1:length(Stat)
                        push!(Stats,SN[Stat[ki]])
                        push!(Tank,[x[2] for x in unique(x->x[2],filter(x->x[1]==Stat[ki],route))])
                        CompsTemp=[]
                        CompsAmountTemp=[]
                        AmountTemp=[]
                        for ka = 1:length(Tank[ki])
                            tankDeliv=filter(x->x.first[1]==Stat[ki] && x.first[2]==Tank[ki][ka],routeAmount)
                            push!(CompsTemp,[x.first[4] for x in tankDeliv])
                            push!(CompsAmountTemp,[x.second for x in tankDeliv])                  
                            push!(AmountTemp,sum(CompsAmountTemp[ka]))
                        end
                        push!(Comps,CompsTemp)
                        push!(CompsAmount,CompsAmountTemp)                  
                        push!(Amount,AmountTemp)
                    end
                    
                    RouteTime+=TimeDict[2004,route[1][1]]
                    RouteTime+=TimeDict[route[end][1],2004]
                    
                    RouteDist+=DistDict[2004,route[1][1]]
                    RouteDist+=DistDict[route[end][1],2004]

                    for l=1:(length(route)-1)
                        RouteTime+=TimeDict[route[l][1],route[l+1][1]]
                        RouteDist+=DistDict[route[l][1],route[l+1][1]]
                    end
                    totalAmount=sum(sum(Amount[i]) for i=1:length(Amount))
                    RouteTime+=15*60+10*60*length(Stat)+totalAmount/((1800+900)/60)
                    
                    push!(Routes[d,t],Route(Stats,Tank,Amount,totalAmount,Vehicle,Comps,CompsAmount,RouteTime,RouteDist))
                end
            end
        end       
    end
    for d=1:nDays, t=1:Trips[3706].nTimePeriod[d]
        Dists = getfield.(Routes[d,t],:RouteDist)
        if length(Dists)>0
            Obj+=sum(getfield.(Routes[d,t],:RouteDist))
        end
    end
    sol=Solution(Routes,Obj,currentLevel)
    return sol
end

function Struct_To_Fuel(Solution)
    fuel_deliv=Dict()
    #fuel_deliv[i,a,k,c,p,d,t,r,it]
    it=Dict()
    rr=Dict()
    for k in All_Vehicles
        it[k]=1
        rr[k]=1
    end
    for d=1:nDays, t = 1:Trips[3706].nTimePeriod[d]
        for k in All_Vehicles
            rr[k]=1
        end
        for r = 1:length(Solution.Routes[d,t])
            kk=Solution.Routes[d,t][r].Vehicle
            it[kk]=1
            for i = 1:length(Solution.Routes[d,t][r].Station), a = 1:length(Solution.Routes[d,t][r].Tank[i])
                for c = 1:length(Solution.Routes[d,t][r].Comps[i][a])
                    ii=Index_To_StatID[Solution.Routes[d,t][r].Station[i]]
                    aa=Solution.Routes[d,t][r].Tank[i][a]
                    cc=Solution.Routes[d,t][r].Comps[i][a][c]
                    pp=Stations[ii].Tanks[aa].ProductNumber
                    am=Solution.Routes[d,t][r].CompsAmount[i][a][c]
                    
                    fuel_deliv[ii,aa,kk,cc,pp,d,t,rr[kk],it[kk]]=am
                end
                it[kk]+=1
            end
            if kk in Trailer_Vehicles
                rr[kk*10+1]+=1
            elseif kk in Rigid_Vehicles
                rr[convert(Int64,(kk-1)/10)]+=1
            end
            rr[kk]+=1
        end
    end

    return fuel_deliv
end

function Objective_Calculator(fuel_deliv)
    Obj=0
    for k in VehicleIDs,d=1:nDays, t = 1:Trips[k].nTimePeriod[d]
        if k in Trailer_Vehicles
            volUsedPeriod=filter(x->(x.first[3]==k || x.first[3]==k*10+1) && x.first[6]==d && x.first[7]==t,fuel_deliv)
        else
            volUsedPeriod=filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t,fuel_deliv)
        end
        if length(volUsedPeriod)!=0
            distUsedDrive=0
            for r=1:sort(collect(volUsedPeriod), by=x->x.first[8])[end].first[8]
                route=sort(collect(filter(x->x[8]==r,keys(volUsedPeriod))), by=x->x[9])
         
                distUsedDrive+=DistDict[2004,route[1][1]]
                distUsedDrive+=DistDict[route[end][1],2004]

                for l=1:(length(route)-1)
                    distUsedDrive+=DistDict[route[l][1],route[l+1][1]]
                end
            end

            Obj+=distUsedDrive
        end
    end
    return Obj
end

function Data_Simulation(nStation,Dist_Time_Bounds,bounds_seed,shuffle_seed)

    DistMatrix,TimeMatrix,DistDict,TimeDict,Stations,Vehicles,Products,Trips,StationIDs,ProductIDs,ProductDict,VehicleIDs,Trailer_Vehicles,Rigid_Vehicles,All_Vehicles,StatID_To_Index,Index_To_StatID = Data_Load()
    
    #shuffle_seed = 13
    #Dist_Time_Bounds = [0.9,1.1]
    SimDist = Dict()
    SimTime = Dict()


    for (k,v) in DistDict
        Ran = sqrt(Dist_Time_Bounds[1] + (Dist_Time_Bounds[2] - Dist_Time_Bounds[1])*rand(MersenneTwister(bounds_seed+v)))
        SimDist[k[1],k[2]] = round(Int64,Ran*DistDict[k[1],k[2]])
        SimDist[k[2],k[1]] = round(Int64,Ran*DistDict[k[2],k[1]])

        SimTime[k[1],k[2]] = round(Int64,Ran*TimeDict[k[1],k[2]])
        SimTime[k[2],k[1]] = round(Int64,Ran*TimeDict[k[2],k[1]])
    end

    
    #Shuffle 
    old_station_order = StationIDs[2:end]
    new_station_order = shuffle(MersenneTwister(shuffle_seed),StationIDs[2:end])
    Transformation = Dict()
    for i = 1:length(old_station_order)
        Transformation[old_station_order[i]] = new_station_order[i]
    end
    Transformation[2004] = 2004

    TempSimDist = Dict()
    TempSimTime = Dict()

    for (k,v) in SimDist
        TempSimDist[Transformation[k[1]] , Transformation[k[2]]  ] = v
    end
    for (k,v) in SimTime
        TempSimTime[Transformation[k[1]] , Transformation[k[2]]  ] = v
    end


    SimDist = TempSimDist
    SimTime = TempSimTime


    Reduced_Station_List = [2004;new_station_order][1:nStation]

    SimDist = filter(x->x.first[1] in Reduced_Station_List && x.first[2] in Reduced_Station_List , SimDist)
    SimTime = filter(x->x.first[1] in Reduced_Station_List && x.first[2] in Reduced_Station_List , SimTime)

    Dist_Matrix = zeros(nStation,nStation)
    for (k,v) in SimDist
        Dist_Matrix[StatID_To_Index[k[1]],StatID_To_Index[k[2]]] = v
    end
    Time_Matrix = zeros(nStation,nStation)
    for (k,v) in SimTime
        Time_Matrix[StatID_To_Index[k[1]],StatID_To_Index[k[2]]] = v
    end



    return Dist_Matrix, Time_Matrix,SimDist,SimTime,Reduced_Station_List
end

function Solution_Checker(fuel_deliv)
    currentLevel = Dict()
    Obj=0
    timeUsedTotal=0
    rNum=0
    rStop=0
    compsTotal=0
    compsLeft=0
    posDryRuns=0
    dryRunIndices=[]

    if length(filter(x->x.second<0.01,fuel_deliv))>0
        println(filter(x->x.second<0.01,fuel_deliv))
        return "Infeasible - Delivering 0 or negative fuel amount"
    end


    for i in setdiff(StationIDs,2004),a = 1:Stations[i].nTank
        #Calculate Current Levels through the days
        for d=1:nDays
            if d==1
                currentLevel[i,a,1]=Stations[i].Tanks[a].StartLevel-Stations[i].Tanks[a].Min
            else
                currentLevel[i,a,d]=currentLevel[i,a,d-1]
            end
            sump=Stations[i].Tanks[a].Consumption[d]

            if currentLevel[i,a,d]-sump<0.0
                posDryRuns+=1
                push!(dryRunIndices,[i,a,d])
            end

            fuelDelivered=values(filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
            if length(fuelDelivered)>0
                currentLevel[i,a,d]+=sum(fuelDelivered)
                #Closed days
                if d in Stations[i].Closed_Days
                    show(stdout, "text/plain", filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
                    return "Infeasible - Day $d is closed for station $i"
                end
                #Maximum level in tanks
                if currentLevel[i,a,d]>0.001+Stations[i].Tanks[a].realCap
                    show(stdout, "text/plain", filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
                    println(" ")
                    println(" ")
                    println("currentLevel i a d: $i $a $d :",currentLevel[i,a,d])
                    println("maxLevel i a: $i $a: ",Stations[i].Tanks[a].realCap)
                    return "Infeasible - Maximum Level not respected"
                end
                #Qualification demands
                Veh=unique([x.first[3] for x in filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv)])

                if length(setdiff(Veh,Stations[i].Allowed_Vehicles))>0
                    show(stdout, "text/plain", filter(x->x.first[1]==i && x.first[2]==a && x.first[6]==d,fuel_deliv))
                    println(" ")
                    println(" ")
                    show(stdout, "text/plain", Stations[i].Allowed_Vehicles)
                    println(" ")
                    println(" ")
                    return "Infeasible - Qualification demands not met"
                end
            end

            currentLevel[i,a,d]-=sump
            #Minimum Level in tanks
            if currentLevel[i,a,d]<0.00
                println("currentLevel i a d: $i $a $d :",currentLevel[i,a,d])
                return "Infeasible - Minimum Level not upheld"
            end
            
        end
        #Correct products delivered to all tanks
        productDelivered=length(filter(x->x.first[1]==i && x.first[2]==a && x.first[5]!=Stations[i].Tanks[a].ProductNumber,fuel_deliv))
        if productDelivered>0
            println("Problem is: ",filter(x->x.first[1]==i && x.first[2]==a && x.first[5]!=Stations[i].Tanks[a].ProductNumber,fuel_deliv))
            return "Infeasible - Wrong products delivered to tanks"
        end
    end
    
    #fuel_deliv[i,a,k,c,p,d,t,r,it]
    #Vol Cap, Weight Cap, 1 prod per compartment
    for k in VehicleIDs, d=1:nDays, t = 1:Trips[k].nTimePeriod[d]
        #Amount of time upheld for each time period
        if k in Trailer_Vehicles
            volUsedPeriod=filter(x->(x.first[3]==k || x.first[3]==k*10+1) && x.first[6]==d && x.first[7]==t,fuel_deliv)
        else
            volUsedPeriod=filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t,fuel_deliv)
        end
        #show(stdout, "text/plain", volUsedPeriod)

        if length(volUsedPeriod)!=0
            timeUsedFuel=sum(values(volUsedPeriod))/((1800+900)/60)
            numberStops=length(unique(x->(x.first[1],x.first[8]),volUsedPeriod))
            numberRoutes=length(unique(x->x.first[8],volUsedPeriod))
            rNum+=numberRoutes
            rStop+=numberStops
            timeUsedStops=15*60*numberRoutes+10*60*numberStops
            timeUsedDrive=0
            distUsedDrive=0
            for r=1:sort(collect(volUsedPeriod), by=x->x.first[8])[end].first[8]
                route=sort(collect(filter(x->x[8]==r,keys(volUsedPeriod))), by=x->x[9])
                #show(stdout, "text/plain", route)
                for rr=1:route[end][9]
                    flow=unique(x->(x[1],x[2]),filter(x->x[9]==rr,route))
                    #show(stdout, "text/plain", flow)
                    if length(flow)>1
                        println("Problem is: ",flow)
                        return "Infeasible - Two stations/tanks in same iteration"
                    end
                end
                timeUsedDrive+=TimeDict[2004,route[1][1]]
                timeUsedDrive+=TimeDict[route[end][1],2004]
                
                distUsedDrive+=DistDict[2004,route[1][1]]
                distUsedDrive+=DistDict[route[end][1],2004]

                for l=1:(length(route)-1)
                    timeUsedDrive+=TimeDict[route[l][1],route[l+1][1]]
                    distUsedDrive+=DistDict[route[l][1],route[l+1][1]]
                end
            end

            timeUsed=timeUsedFuel+timeUsedStops+timeUsedDrive
            timeUsedTotal+=timeUsed
            Obj+=distUsedDrive
            if timeUsed>Trips[k].TimePeriodLengths[d][t]
                tcap=Trips[k].TimePeriodLengths[d][t]
                #println("Problem is at k,d,t: $k, $d, $t")
                #println("time: $timeUsed, time cap $tcap ")
                #return "Infeasible - Time period lengths not respected"
            end
        end

        for r=1:sort(collect(fuel_deliv), by=x->x.first[8])[end].first[8]
            #Volume Capacity
            volUsed = filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t && x.first[8]==r,fuel_deliv)
            #rNum+=1
            if length(volUsed)!=0
                volSum=sum(values(volUsed))
                compsLeft += Vehicles[k].nCompartment-length(unique(x->x.first[4],volUsed))
                compsTotal += Vehicles[k].nCompartment
                #println(volUsed,"    ",Vehicles[k].VolCap)
                if volSum>0.1+Vehicles[k].VolCap
                    println("Problem is: k,d,t,r: $k, $d, $t, $r. Filled: $volSum. Capacity: $(Vehicles[k].VolCap)")
                    return "Infeasible - Volume capacity for each truck not respected"
                end
            end
            #Weight Capacity
            weightUsed=filter(x->x.first[3]==k && x.first[6]==d && x.first[7]==t && x.first[8]==r,fuel_deliv)
            if length(weightUsed)!=0
                weightSum=sum(v*Products[ProductIDs[k[5]]].Density for (k,v) in weightUsed)
                #println(weightSum,"    ",Vehicles[k].WeightCap)
                if weightSum>0.001+Vehicles[k].WeightCap
                    println("Problem is: k,d,t,r: $k, $d, $t, $r")
                    println("sum of weights ",weightSum,"  weight cap",Vehicles[k].WeightCap)
                    return "Infeasible - Weight capacity for trucks not respected"
                end
            end
            
            for c=1:Vehicles[k].nCompartment
                #Volume capacity for compartments
                compVolUsed=filter(x->x.first[3]==k && x.first[4]==c && x.first[6]==d && x.first[7]==t && x.first[8]==r,fuel_deliv)
                if length(compVolUsed)!=0
                    compVolSum=sum(values(compVolUsed))
                    #println(compVolUsed,"    ",Vehicles[k].VolCompartment[c])
                    if compVolSum>Vehicles[k].VolCompartment[c]+0.01
                        println("Problem is: $compVolUsed")
                        return "Infeasible - Volume capacity for compartments not respected"
                    end
                end
                
                #Only one product in each compartment
                prodComp=unique(x->x.first[5],filter(x->x.second>0.01 && x.first[3]==k && x.first[4]==c && x.first[6]==d && x.first[7]==t && x.first[8]==r,fuel_deliv))
                if length(prodComp)>1
                    println(prodComp)
                    return "Infeasible - More than 1 product in compartment"
                end
            end
        end
    end

    timeUsedTotal2=round(100*(timeUsedTotal/sum(Trips[k].TimePeriodLengths[d][t] for k in VehicleIDs for d=1:nDays for t=1:Trips[k].nTimePeriod[d])),digits=2)
    ObjKM=round(Int,Obj/1000)
    solution = Fuel_To_Struct(fuel_deliv)
    average_volume_per_route= mean(solution.Routes[d,t][r].TotalAmount for d = 1:nDays for t=1:Trips[3920].nTimePeriod[d] for r = 1:length(solution.Routes[d,t])) 

    Dict_of_fill_ratio = Dict()
    Dict_of_Vol_Weight = Dict()
    for d = 1:30
        for t = 1:Trips[3706].nTimePeriod[d]
            for r = 1:length(solution.Routes[d,t])
                Vol = sum(Vehicles[solution.Routes[d,t][r].Vehicle].VolCompartment) \ solution.Routes[d,t][r].TotalAmount
                Weight = Vehicles[solution.Routes[d,t][r].Vehicle].WeightCap \ sum(solution.Routes[d,t][r].Amount[i][a]*Products[ProductIDs[StationsArray[solution.Routes[d,t][r].Station[i]].Tanks[solution.Routes[d,t][r].Tank[i][a]].ProductNumber]].Density for i = 1:length(solution.Routes[d,t][r].Station) for a = 1:length(solution.Routes[d,t][r].Tank[i])  )  
                Dict_of_fill_ratio[d,t,r] = max(Vol,Weight) 
                Dict_of_Vol_Weight[d,t,r] = (Vol,Weight)
            end
        end
    end
    #sum(collect(values(Dict_of_Vol_Weight))[i][2] for i =1:341)/341

    nRoutes = length(collect(solution.Routes[d,t][r] for d = 1:nDays for t = 1:Trips[3920].nTimePeriod[d]  for r = 1:length(solution.Routes[d,t])))
    volumeFilled=round(100*sum(collect(values(Dict_of_Vol_Weight))[i][1] for i =1:nRoutes)/nRoutes  ,digits=2)
    weightFilled=round(100*sum(collect(values(Dict_of_Vol_Weight))[i][2] for i =1:nRoutes)/nRoutes  ,digits=2)
    totalFilled =round(100*sum(collect(values(Dict_of_fill_ratio))[i]    for i =1:nRoutes)/nRoutes  ,digits=2)

    compRatio = round(100*(compsLeft/compsTotal),digits=2)
    routeStopRatio=round(rStop/rNum,digits=2)

    # show(stdout, "text/plain", dryRunIndices)
    # show(stdout, "text/plain", sum(values(fuel_deliv)))

    average_station_volume = mean(sum(solution.Routes[d,t][r].Amount[i]) for d=1:nDays for t = 1:Trips[3920].nTimePeriod[d] for r = 1:length(solution.Routes[d,t]) for i = 1:length(solution.Routes[d,t][r].Station))


    return ["Feasible with objective: $ObjKM km, $Obj m.", 
    "The vehicles are active $timeUsedTotal2 % of the time.", 
    "The vehicles are on average $volumeFilled % filled wrt. volume.",
    "The vehicles are on average $weightFilled % filled wrt. weight.",
    "The vehicles are on average $totalFilled % filled wrt. volume and weight.",
    "$compsLeft out of $compsTotal ($compRatio %) of the compartments are empty.",
    "Total of $rNum routes and $rStop stops, average of $routeStopRatio stops per route",
    "Total volumen delivered $(round(sum(values(fuel_deliv)),digits = 2 )) ",
    "Average volume per route $average_volume_per_route",
    "Average kilometer per volume  $(round(1000*ObjKM/sum(values(fuel_deliv)),digits = 2 ) )",
    "Average volume at Stations $(average_station_volume)",
    "$posDryRuns possible dry runs",
    "$((round(timeUsedTotal))) seconds. $((round(timeUsedTotal/3600,digits = 2))) hours."]
end

println("Loading data...")
println(" ")

nStation=60
nVehicle=4
nProduct=6
nDays=30
fuel_flow_rate = 1/45

#fuel_deliv[i,a,k,c,p,d,t,r,it]

DistMatrix,TimeMatrix,DistDict,TimeDict,Stations,Vehicles,Products,Trips,StationIDs,ProductIDs,ProductDict,VehicleIDs,Trailer_Vehicles,Rigid_Vehicles,All_Vehicles,StatID_To_Index,Index_To_StatID,StationsArray,Tank_To_Station = Data_Load()

nTimePeriods=Trips[3706].TimePeriodIndex[end][1]


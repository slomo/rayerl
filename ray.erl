-module(ray).
-compile(export_all).
%-export([getRays/4,test/0]).

-record(color,{r=0,g=0,b=0}).
-record(vector,{x=0,y=0,z=0}).
-record(ball,{m,r}).
-record(plane,{v1,v2,base}).
-record(ray,{v1,base}).


-define(eye,#vector{x=300,y=200,z=-400}).
-define(image,#plane{
        v1 = #vector{x=1},
        v2 = #vector{y=1},
        base = #vector{}}).



addVector(V1,V2) ->
    #vector{
        x = V1#vector.x + V2#vector.x,
        y = V1#vector.y + V2#vector.y,
        z = V1#vector.z + V2#vector.z
    }.

mulVectorScalar(A,#vector{x=X,y=Y,z=Z}) ->
    #vector{
        x = A * X,
        y = A * Y,
        z = A * Z
    }.

mulVector(#vector{x=A1,y=A2,z=A3},#vector{x=B1,y=B2,z=B3}) ->
    A1*B1 + A2*B2 + A3*B3.

normVector(v) ->
    math:sqrt(mulVector(v,v)).

getRays(Source,Plane,NoX,NoY) ->
    PlanePoints = [ getPointInPlane(Plane,X,Y) ||
                    Y <- lists:seq(1,NoX),
                    X <- lists:seq(1,NoY)],
    lists:map( fun
            (Point) ->
                createRay(Source,Point)
        end,PlanePoints).


getPointInPlane(#plane{v1=V1,v2=V2,base=Base},DV1,DV2) ->
    InPlane1 = mulVectorScalar(DV1,V1),
    InPlane2 = mulVectorScalar(DV2,V2),
    addVector(InPlane1,addVector(InPlane2,Base)).

getPointInRay(#ray{v1=V,base=B},DV) ->
    addVector(mulVectorScalar(DV,V),B).

createRay(V1,V2) ->
    #ray{
        v1 = addVector(V2,mulVectorScalar(-1,V1)),
        base = V1
    }.

intersect(#ray{v1=V,base=B}=Ray,#ball{m=M,r=R}) ->
    Z = addVector(mulVectorScalar(-1,M),B),
    A = mulVector(V,V),
    P = 2*mulVector(V,Z) / A,
    Q = (mulVector(Z,Z) - math:pow(R,2)) / A,
    Rad = math:pow(P/2,2) - Q,
    if
        Rad < 0 ->
            {};
        Rad == 0 ->
            {getPointInRay(Ray,P/2)};
        true ->
            {
                getPointInRay(Ray,-P/2+math:sqrt(Rad)),
                getPointInRay(Ray,-P/2-math:sqrt(Rad))
            }
    end
%    ;
%intersect(#ray{v1=V1,base=B1}=Ray,#ray{v1=V2,v2=V3,base=B2}) ->
    .


solve([Last]) ->
    Last;
solve([Row|Remaining]) ->
    NewRemaining = lists:map(
        fun
            (Remains) ->
                mergeRows(Row,Remains)
        end,
        Remaining),
    solve(NewRemaining).
    
    

mergeRows([H1|_]=List1,[H2|_]=List2) ->
    Fak = H1/H2,
    List2Norm = lists:map(
        fun
            (Ele) ->
                Ele * Fak
        end,
        List2),
    io:format("~p~n~p~n~n",[List1,List2Norm]),
    NewList = lists:zipwith(
        fun 
            (Ele1,Ele2) ->
                Ele2 - Ele1
        end,
        List1,List2Norm),
    lists:dropwhile(
        fun 
            (Ele) ->
                0 == Ele
        end,
        NewList).



parmap(F, L) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end || Pid <- [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L]].

setColorIfIntersects(Ray,Object) ->
    case intersect(Ray,Object) of
        {} ->
            false;
        _ ->
            #color{r=255}
    end.

intersectsWithOne(Ray,Objects) ->
    lists:foldr(
        fun
            (Object,Acc) ->
                case setColorIfIntersects(Ray,Object) of
                    false ->
                        Acc;
                    Color ->
                        Color
                end
        end,#color{},Objects).

writeToFile(Points,{DimX,DimY}) ->
    {ok,Fd} = file:open("my.pbm",[write]),
    ok = io:format(Fd,"P3~n~w ~w~n255~n",[DimY,DimX]),
    lists:foreach(
        fun
            (#color{r=R,g=G,b=B}) -> io:format(Fd,"~w ~w ~w~n",[R,G,B]) end,
        Points),
    file:close(Fd).

    %% ----


test() ->
    Objects = [#ball{m=#vector{x=200,y=200,z=100},r=100}, #ball{m=#vector{x=900,y=300,z=2000},r=100}],
    Rays = getRays(?eye,?image,400,600),
    
    lists:map(fun(Ray) -> intersectsWithOne(Ray,Objects) end,Rays).



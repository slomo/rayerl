-module(ray).
-compile(export_all).
%-export([getRays/4,test/0]).

-record(color,{r=0,g=0,b=0}).
-record(vector,{x=0,y=0,z=0}).
-record(ball,{m,r}).
-record(plane,{v1,v2,base}).
-record(ray,{v1,base}).


-define(eye,#vector{x=200,y=200,z=-400}).
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

normVector(#vector{x=X,y=Y,z=Z}) ->
    math:sqrt(X*X+Y*Y+Z*Z).

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
    Z = addVector(B,M),
    A = mulVector(V,V),
    P = mulVector(V,Z) / A,
    Q = mulVector(Z,Z) + math:pow(R,2) / A,
    Rad = math:pow(P/2,2) - Q,
    if
        Rad < 0 ->
            {};
        Rad == 0 ->
            {getPointInRay(Ray,P/2)};
        true ->
            {
                getPointInRay(Ray,P/2+math:sqrt(Rad)),
                getPointInRay(Ray,P/2-math:sqrt(Rad))
            }
    end.

setColorIfIntersects(Ray,Object) ->
    case intersect(Ray,Object) of
        {} ->
            #color{};
        _ ->
            #color{r=255}
    end.

writeToFile(Points,{DimX,DimY}) ->
    {ok,Fd} = file:open("my.pbm",[write]),
    ok = io:format(Fd,"P3~n~w ~w~n",[DimX,DimY]),
    lists:foreach(
        fun
            (#color{r=R,g=G,b=B}) -> io:format(Fd,"~w ~w ~w~n",[R,G,B]) end,
        Points),
    file:close(Fd).

    %% ----


test() ->
    Object = #ball{m=#vector{x=200,y=200,z=100},r=200},
    Rays = getRays(?eye,?image,400,600),
    lists:map(fun(Ray) -> setColorIfIntersects(Ray,Object) end,Rays).



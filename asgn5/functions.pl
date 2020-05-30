% Jonathan Xu
% Zain Shafique

% prolog version of not 
not( X ) :- X, !, fail.
not( _ ).

% formatting for printing times
pad2( Num, Padded) :- Num < 10, string_concat( '0', Num, Padded).
pad2( Num, Num).

% Outputs distance in minutes
haversine_distance( Start, End, Distance) :-
    airport( Start, _, degmin( Start_LatD, Start_LatM ), 
            degmin( Start_LonD, Start_LonM )),
    airport( End, _, degmin( End_LatD, End_LatM ), 
            degmin( End_LonD, End_LonM )),

    % converting degrees/minutes to radians
    Lat1 is (Start_LatD + (Start_LatM / 60)) * (pi / 180),

    Lat2 is (End_LatD + (End_LatM / 60)) * (pi / 180),

    Lon1 is (Start_LonD + (Start_LonM / 60)) * (pi / 180),

    Lon2 is (End_LonD + (End_LonM / 60)) * (pi / 180),

    % converting radians to minutes
    Dlon is Lon2 - Lon1,
    Dlat is Lat2 - Lat1,
    Temp is sin( Dlat / 2 ) ** 2
        + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
    Dist is 2 * atan2( sqrt( Temp ), sqrt( 1 - Temp )),
    Distance is ((Dist * 3961)/500) * 60.

% determining remainder
remainder( Float, Modulus, Quotient, Remainder) :-
   Quotient is truncate( Float / Modulus),
   Remainder is round(Float - Quotient * Modulus).

% converting characters to uppercase
to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( upcase_atom, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

% formatting for printing trip information
print_trip( Activity, Code, Name, time( Hour, Min)) :-
    write( Activity), write( '  '),
    string_upper( Code, Upper),
    write( Upper), write( '  '), write( Name), write( ' '),
    pad2( Hour, Hour2), write( Hour2), write( ':'),
    pad2( Min, Min2), write( Min2), nl.

%list of nodes is empty from listpath
writepath( [] ) :-
   nl.

%recursively print all the paths found by listpath
writepath( [flight(Start, End, time(Depart_Hrs, Depart_Mins))|Tail] ) :-

   % names of airports
   airport(Start, StartName, _, _),
   airport(End, EndName, _, _),

   % using haversine formula to calculate arrival time in minutes
   haversine_distance(Start, End, Duration),
   ArrivalMins is Duration + (Depart_Hrs * 60) + Depart_Mins,

   remainder(ArrivalMins, 60, Arrive_Hrs, Arrive_Mins),
  
   % printing trips
   print_trip('depart', Start, StartName, 
            time(Depart_Hrs, Depart_Mins)),
   print_trip('arrive', End, EndName, 
            time(Arrive_Hrs, Arrive_Mins)),

   %recursively call writepath on rest of list
   writepath( Tail ).

writepath(_).

% base case
listpath( Node, End, Outlist ) :-
   listpath( Node, End, [Node], 0, Outlist ).

% case: found correct path
listpath( Node, Node, _, _, [Node] ).

% recursively finding path from arrival to finish
listpath( Node, End, Tried, PrevArrivalTime,
            [flight(Node, Next, time(Hrs,Min))|List] ) :-

   % get info for current flight
   flight(Node, Next, time(Hrs,Min)),

   % use haversine formula to calculate arrival time for next flight
   haversine_distance(Node, Next, Duration),
   ArrivalTime is ((Hrs * 60) + Min) + Duration,

   % calculate arrival time of previous flight 
   TimeAvailable is PrevArrivalTime + 30,

   % calculate departure time of current flight
   CurrDepartTime is (Hrs * 60) + Min,

   % check if destination already visited
   not( member( Next, Tried )),

   % check arrival time of the prev flight to this flights departure
   (TimeAvailable < CurrDepartTime),

   % recursively call listpath
   listpath(Next, End, [Next|Tried], ArrivalTime, List ).

% airport codes are uppercase
fly(Start, End) :-
   var(Start),
   var(End),
   write(' Error: Airport codes must be lowercase'), nl, !, fail.

% airports are same
fly(Airport, Airport) :-
   write(' Error: Same Airport '), nl, !, fail.

fly(Start, End) :-

   % check if airports are in database
   airport(Start, _, _, _),
   airport(End, _, _, _),

   % determine paths between airports
   listpath( Start, End, List ), !, nl,

   %print out paths between airports
   writepath( List ), true.

% no path between airports
fly(Start, End) :-

   % check if airports are in database
   airport(Start, _, _, _),
   airport(End, _, _, _),

   % if no path and airports exist
   write(' Error: No Path '), nl, !, fail.

% airports don't exist
fly(_, _) :-
   write(' Error: Non-existent Airports '), nl, !, fail.




-module(game_of_life).
-compile(export_all).

evolve(InitialState, Max) ->
  evolve(InitialState, 0, Max).
evolve(FinalState, Cycles, Max) when Cycles == Max ->
  FinalState;
evolve(InitialState, Cycles, Max) ->
  create_image:draw_points(InitialState, integer_to_list(Cycles)),
  evolve(world_tick(InitialState), Cycles + 1, Max).

world_tick(LivingCells) ->
  Candidates = lists:flatmap(fun emit_cells/1, LivingCells),
  CountedCandiates = lists:foldl(fun combine_cells/2, dict:new(), Candidates),
  TickedCells = dict:fold(fun key_value_to_ticked_cell/3, [], CountedCandiates),
  lists:flatmap(fun drop_dead_cells/1, TickedCells).

timed_world_tick(Width, Height) ->
  LivingCells = generate_square_world(Width, Height),
  {Time, Value} = timer:tc(game_of_life, world_tick, [LivingCells]),
  Description = lists:flatten(io_lib:format("Ticked ~p cells in ~p milliseconds.",
                [length(LivingCells), Time/1000])),
  {Value, Description}.

generate_square_world(Width, Height) ->
  [{X, Y} || X <- lists:seq(1,Width), Y <- lists:seq(1,Height)].


emit_cells({X, Y}) ->
  [ {X-1, Y-1, 1, false}, {X-1, Y  , 1, false}, {X-1, Y+1, 1, false},
    {X  , Y-1, 1, false}, {X  , Y  , 0, true }, {X  , Y+1, 1, false},
    {X+1, Y-1, 1, false}, {X+1, Y  , 1, false}, {X+1, Y+1, 1, false} ].

combine_cells({X,Y,Count,Alive}, CellDict) ->
  IncrementCount = fun ({DictCount, DictAlive}) ->
    {DictCount + Count, DictAlive or Alive}
  end,
  dict:update({X, Y}, IncrementCount, {Count, Alive}, CellDict).

key_value_to_ticked_cell({X, Y}, {Count, Alive}, Acc) ->
  [{X, Y, tick(Alive, Count)} | Acc].

tick(true, 2) -> true;
tick(_, 3) -> true;
tick(_, _) -> false.

drop_dead_cells({X, Y, true})  -> [{X, Y}];
drop_dead_cells({_X, _Y, false}) -> [].

glider_gun() ->
  [{3,  6}, {4,  6}, {3,  7},  {4, 7},  {13, 6},  {13, 7}, {13, 8}, {14, 5},
   {14, 9}, {15, 4}, {15, 10}, {16, 4}, {16, 10}, {17, 7}, {18, 5},
   {18, 9}, {19, 6}, {19, 7},  {19, 8}, {20, 7},  {23, 4}, {23, 5},
   {23, 6}, {24, 4}, {24, 5},  {24, 6}, {25, 3},  {25, 7}, {27, 2},
   {27, 3}, {27, 7}, {27, 8},  {37, 4}, {37, 5},  {38, 4}, {38, 5}].

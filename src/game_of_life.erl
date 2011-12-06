-module(game_of_life).
-compile(export_all).

world_tick(LivingCells) ->
  Candidates = lists:flatmap(fun emit_cells/1, LivingCells),
  CountedCandiates = lists:foldl(fun combine_cells/2, dict:new(), Candidates),
  TickedCells = dict:fold(fun key_value_to_ticked_cell/3, [], CountedCandiates),
  lists:flatmap(fun drop_dead_cells/1, TickedCells).

timed_world_tick(Width, Heigh) ->
  LivingCells = [{X, Y} || X <- lists:seq(1,Width), Y <- lists:seq(1,Heigh)],
  {Time, _} = timer:tc(game_of_life, world_tick, [LivingCells]),
  lists:flatten(io_lib:format("Ticked ~p cells in ~p milliseconds.",
                [length(LivingCells), Time/1000])).

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


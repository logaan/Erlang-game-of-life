-module(game_of_life_test).

-compile(export_all).
-import(game_of_life, [tick/2, emit_cells/1, world_tick/1]).

-include_lib("eunit/include/eunit.hrl").

live_cell_tick_test() ->
  false = tick(true, 0),
  false = tick(true, 1),
  true  = tick(true, 2),
  true  = tick(true, 3),
  false = tick(true, 4),
  false = tick(true, 5),
  false = tick(true, 6),
  false = tick(true, 7),
  false = tick(true, 8).

dead_cell_tick_test() ->
  false = tick(false, 0),
  false = tick(false, 1),
  false = tick(false, 2),
  true  = tick(false, 3),
  false = tick(false, 4),
  false = tick(false, 5),
  false = tick(false, 6),
  false = tick(false, 7),
  false = tick(false, 8).

world_tick_test() ->
  [] = world_tick([{0,0}]),
  [] = world_tick([{0,0}, {1,0}]),
  [{0,0}, {0,-1}, {0,1}] = world_tick([{-1,0}, {0,0}, {1,0}]).

emit_cells_test() ->
  [ {-1,-1,1,false},
    {-1,0,1,false},
    {-1,1,1,false},
    {0,-1,1,false},
    {0,0,0,true},
    {0,1,1,false},
    {1,-1,1,false},
    {1,0,1,false},
    {1,1,1,false}] = emit_cells({0,0}).


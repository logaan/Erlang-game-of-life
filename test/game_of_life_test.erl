-module(game_of_life_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

live_cell_tick_test() ->
  false = game_of_life:tick(true, 0),
  false = game_of_life:tick(true, 1),
  true  = game_of_life:tick(true, 2),
  true  = game_of_life:tick(true, 3),
  false = game_of_life:tick(true, 4),
  false = game_of_life:tick(true, 5),
  false = game_of_life:tick(true, 6),
  false = game_of_life:tick(true, 7),
  false = game_of_life:tick(true, 8).

dead_cell_tick_test() ->
  false = game_of_life:tick(false, 0),
  false = game_of_life:tick(false, 1),
  false = game_of_life:tick(false, 2),
  true  = game_of_life:tick(false, 3),
  false = game_of_life:tick(false, 4),
  false = game_of_life:tick(false, 5),
  false = game_of_life:tick(false, 6),
  false = game_of_life:tick(false, 7),
  false = game_of_life:tick(false, 8).

world_tick_test() ->
  [] = game_of_life:world_tick([{0,0}]),
  [] = game_of_life:world_tick([{0,0}, {1,0}]),
  [{0,0}, {0,-1}, {0,1}] = game_of_life:world_tick([{0,0}, {-1,0}, {1,0}]),
  [{0,0}, {-1,0}, {1,0}] = game_of_life:world_tick([{0,0}, {0,-1}, {0,1}]).

drawing_world_tick_test() ->
  StartWorld = [{0,0}, {0,-1}, {0,1}],
  EndWorld = game_of_life:world_tick(StartWorld),
  create_image:draw_points(StartWorld, "start"),
  create_image:draw_points(EndWorld, "end").

evolve_test() ->
  StartWorld = game_of_life:generate_square_world(100,100),
  game_of_life:evolve(StartWorld, 1).

glider_gun_test() ->
  game_of_life:evolve(game_of_life:glider_gun(), 100).

timed_world_tick_test() ->
  {Value, Description} = game_of_life:timed_world_tick(100, 100),
  create_image:draw_points(Value, "10000"),
  erlang:display(Description).

emit_cells_test() ->
  [ {-1,-1,1,false},
    {-1,0,1,false},
    {-1,1,1,false},
    {0,-1,1,false},
    {0,0,0,true},
    {0,1,1,false},
    {1,-1,1,false},
    {1,0,1,false},
    {1,1,1,false}] = game_of_life:emit_cells({0,0}).


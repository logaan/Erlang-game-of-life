-module(create_image_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-import(create_image, [draw_points/2, find_canvas_edges/1]).

from_world_test() ->
  draw_points([{10, 10}, {5, 5}], "two"),
  draw_points([{10, 10}, {-5, -5}], "negative"),
  draw_points([{0, -1}, {0, 0}, {0, 1}], "line").

find_canvas_edges_test() ->
  {{1, 1}, {10, 10}} = find_canvas_edges([{1, 1}, {10, 10}]),
  {{1, 1}, {10, 10}} = find_canvas_edges([{5, 5}, {1, 1}, {10, 10}]),
  {{0, 0}, {10, 10}} = find_canvas_edges([{0, 10}, {10, 0}, {5, 5}]),
  {{0, 0}, {10, 10}} = find_canvas_edges([{0, 0}, {0, 0}, {10, 10}]).


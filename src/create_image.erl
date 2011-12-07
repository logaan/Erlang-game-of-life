-module(create_image).
-compile(export_all).
-define(cell_size, 5).

draw_points(Points, FileName) ->
   {{MinX, MinY}, {MaxX, MaxY}} = find_canvas_edges(Points),
   CenteredPoints = lists:map(fun({X, Y}) -> {X - MinX, Y - MinY} end, Points),
   Canvas = egd:create(max_edge(MinX, MaxX), max_edge(MinY, MaxY)),
   lists:map(fun(Point) -> draw_cell(Point, Canvas) end, CenteredPoints),
   ImageBinary = egd:render(Canvas),
   egd:save(ImageBinary, FileName ++ ".png").

max_edge(MinPoint, MaxPoint) ->
  (MaxPoint - MinPoint + 1) * ?cell_size.

find_canvas_edges([{X, Y} | TailPoints]) ->
  find_canvas_edges(X, Y, X, Y, TailPoints).

find_canvas_edges(MinX, MinY, MaxX, MaxY, []) ->
  {{MinX, MinY}, {MaxX, MaxY}};
find_canvas_edges(MinX, MinY, MaxX, MaxY, [{X,Y} | TailPoints]) ->
  find_canvas_edges(smallest(MinX, X), smallest(MinY, Y),
                    largest(MaxX, X), largest(MaxY, Y),
                    TailPoints).

largest(Val1, Val2) when Val1 > Val2 -> Val1;
largest(Val1, Val2) when Val1 < Val2 -> Val2;
largest(Val1, Val2) when Val1 == Val2 -> Val1.

smallest(Val1, Val2) when Val1 < Val2 -> Val1;
smallest(Val1, Val2) when Val1 > Val2 -> Val2;
smallest(Val1, Val2) when Val1 == Val2 -> Val1.

draw_cell(CoOrds, Image) ->
   Red = egd:color({255, 0, 0}),
   {TopLeft, BottomRight} = point_to_rectangle(CoOrds),
   egd:filledRectangle(Image, TopLeft, BottomRight, Red).

point_to_rectangle({X,Y}) ->
  NewX = X * 5,
  NewY = Y * 5,
  {{NewX, NewY}, {NewX + 4, NewY + 4}}.


defmodule Vindinium.Bots.RainerZufall do

  require Logger

  # Magic numbers
  @hero_life_max 100
  @hero_fight_lose_life 20
  @mine_lose_life 20
  @tavern_gold 2
  @tavern_life 50

  # --- DIRECTIONS ---
  @directions [
    stay: "Stay",
    north: "North",
    south: "South",
    east: "East",
    west: "West",
  ]
  @direction_keys Keyword.keys(@directions)
  Enum.each(@directions, fn({atom, string}) ->
    defp direction(unquote(atom)), do: unquote(string)
  end)

  @coords_around [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]

  defp coord_to_direction({ 0, -1}), do: :west
  defp coord_to_direction({ 0,  1}), do: :east
  defp coord_to_direction({ 1,  0}), do: :south
  defp coord_to_direction({-1,  0}), do: :north
  defp coord_to_direction({ 0,  0}), do: :stay
  defp coord_to_direction(nil),      do: :stay

  # --- TILES ---
  # ## Impassable wood
  # @1 Hero number 1
  # [] Tavern
  # $- Gold mine (neutral)
  # $1 Gold mine (belonging to hero 1)
  @tiles [
    {'  ', :empty},
    {'##', :wood},
    {'[]', :tavern},
    {'$-', :mine},
    {'$1', {:mine, 1}},
    {'$2', {:mine, 2}},
    {'$3', {:mine, 3}},
    {'$4', {:mine, 4}},
    {'@1', {:hero, 1}},
    {'@2', {:hero, 2}},
    {'@3', {:hero, 3}},
    {'@4', {:hero, 4}},
  ]
  Enum.each(@tiles, fn({string, type}) ->
    defp tile_type(unquote(string)), do: unquote(type)
  end)
  Enum.each(@tiles, fn({string, type}) ->
    defp tile_string(unquote(type)), do: unquote(string)
  end)

  def move(state) do

    # Factors
    # * Health (1..100)
    # * Gold (>= 2) for buying beer
    # * Distance Tavern for drinking beer
    # * Distance free/other mine
    #
    # Questions
    # Is there any mine to conquer?
    #   No -> Walk to next tavern and wait
    #   Yes ->
    #     Can i reach and conquer mine with my current health?
    #       No -> Find tavern nearest to mine and drink there first
    #       Yes -> Walk to mine and conquer it
    # Do i have enough energy to fight?
    #   No -> Flee in direction away from hero.
    #   Yes ->
    #     Would i win some mines?
    #       No -> Flee in direction away from hero.
    #       Yes -> Fight.

    # TODO: Try to detect and avoid loops in decisions. Choose a random action then. Maybe forwardtracking?
    # TODO: Create "Heatmaps" around other heroes to avoid them earlier.
    # TODO: Do not step on spawn points except my own.
    # DONE: When standing directly next to a tavern, but life is not 100 and gold > 2, drink till life is 100.
    # DONE: Avoid a path where a hero is currently standing next to so the hero will flee from.

    IO.puts ""

    # Enrich state map with some needed infos.
    state = state
      |> add_map
      |> add_heroes_map
      |> add_routes

    {:ok, dir} =
    with :no_direction <- drain_tavern(state),
         :no_direction <- flee_or_fight(state),
         :no_direction <- conquer_mine(state) do
      state |> random_direction |> ok
    end

    IO.puts(hero_stats_line(state["hero"]))

    :timer.sleep(10)
    print_map(state)

    direction(dir)
  end

  # Drink as much as usefull when standing next to a tavern.
  defp drain_tavern(%{"hero" => %{"life" => life}}) when life >= 90, do: :no_direction
  defp drain_tavern(%{map: map}) do
    @coords_around
    |> Enum.filter(fn({a, b}) ->
      Map.get(map, {a, b}) in [:tavern]
    end)
    |> case do
      [coord|_] ->
        # Found a tavern, lets drink!
        IO.puts("MEMO: Drinking all the beer!")
        coord |> coord_to_direction |> ok
      [] ->
        # No tavern, no drinking.
        :no_direction
    end
  end

  defp random_direction(state) do
    {0, 0}
    |> next_empty_coords(state)
    |> Enum.take_random(1)
    |> List.first
    |> coord_to_direction
  end

  defp conquer_mine(%{route_mines: route_mines, route_taverns: route_taverns} = state) do
    case route_mines do
        [] ->
          # Walk to next tavern and wait
          case route_taverns do
            [{_dist, coords, [next_step|_]}|_] ->
              if {0, 0} in next_empty_coords(coords, state) do
                # Already reached empty tile next to tavern
                IO.puts("MEMO: Standing next to tavern because nothing to do")
                :stay |> ok
              else
                # Walk to tavern
                IO.puts("MEMO: Drink at next tavern")
                next_step |> coord_to_direction |> ok
              end
            [] ->
              IO.puts("MEMO: No tavern reachable")
              state |> random_direction |> ok
          end
        [{dist, _coords, [next_step|_]}|_] ->
          # Can i reach and conquer mine with my current health?
          life_needed = @mine_lose_life + @hero_fight_lose_life + dist
          life_current = hero_life(state)
          case life_needed <= life_current do
            false ->
              # TODO: Find tavern NEAREST to me AND mine and drink there first
              IO.puts("MEMO: Drink at next tavern before going to mine")
              case route_taverns do
                [{_dist, _coords, [next_step|_]}|_] ->
                  next_step |> coord_to_direction |> ok
                [] ->
                  state |> random_direction |> ok
              end
            true ->
              # Walk to mine and conquer it
              IO.puts("MEMO: Walk to mine and conquer it")
              next_step |> coord_to_direction |> ok
          end
      end
  end

  defp flee_or_fight(%{route_heroes: route_heroes, route_taverns: route_taverns} = state) do
    case route_heroes do
      [{dist, _coords, [next_step|_], {:hero, other_hero_id}} | _] when dist < 3 ->
        # There is a hero around, should we fight him or flee?
        other_hero_life = hero_life(state, other_hero_id)
        case hero_life(state) > other_hero_life + @hero_fight_lose_life do
          true ->
            # TODO: Does other hero have any mines we could win?
            # FIGHT!
            IO.puts("MEMO: FIGHT hero #{other_hero_id}")
            next_step |> coord_to_direction |> ok
          false ->
            case route_taverns do
              [{_dist, _coords, [next_step|_]}|_] ->
                IO.puts("MEMO: Fleeing to next tavern from hero #{other_hero_id}")
                next_step |> coord_to_direction |> ok
              [] ->
                state |> random_direction |> ok
            end
        end
      _ ->
        :no_direction
    end
  end

  defp next_empty_coords({x, y}, %{map: map}) do
    @coords_around
    |> Enum.filter(fn({a, b}) ->
      Map.get(map, {x+a, y+b}) in [:empty]
    end)
  end

  defp route_taverns(state) do

    # TODO: Do not step on other heros spawning points.

    hero_pos = other_hero_positions(state)
    edge_cost_fun = fn(_from, to) ->
      case to in hero_pos do
        true ->
          100 # try to avoid any path that would come next to a other hero
        false ->
          1
      end
    end

    state
    |> find_coords([:tavern])
    |> Enum.map(fn({_, _} = coords) ->
      path = find_path_to(state, {0, 0}, coords, [:empty, :tavern], [edge_cost: edge_cost_fun])
      {length(path), coords, path}
    end)
    |> Enum.filter(fn({length, _, _}) ->
      0 != length
    end)
    |> Enum.sort
  end

  defp route_mines(state) do
    tile_types = [:mine] ++ Enum.map(other_hero_ids(state), &({:mine, &1}))

    # TODO: Use edge_cost function here too, for avoiding other heroes.

    state
    |> find_coords(tile_types)
    |> Enum.map(fn({_, _} = coords) ->
      path = find_path_to(state, {0, 0}, coords, [:empty] ++ tile_types)
      {length(path), coords, path}
    end)
    |> Enum.filter(fn({length, _, _}) ->
      0 != length
    end)
    |> Enum.sort
  end

  defp route_heroes(state) do
    tile_types = Enum.map(other_hero_ids(state), &({:hero, &1}))

    # Not using a edge cost function here, so we will get fastes way to other heroes.

    state
    |> find_coords(tile_types, true)
    |> Enum.map(fn({coords, type}) ->
      path = find_path_to(state, {0, 0}, coords, [:empty] ++ tile_types)
      {length(path), coords, path, type}
    end)
    |> Enum.filter(fn({distance, _coords, _path, _type}) ->
      0 != distance
    end)
    |> Enum.sort
  end

  defp find_coords(%{map: map}, types, with_type \\ false) do
    tiles = map
    |> Enum.filter(fn({_coords, type}) ->
      type in types
    end)

    case with_type do
      true -> tiles
      false -> Keyword.keys(tiles)
    end
  end

  defp find_path_to(%{map: map}, from = {_, _}, to = {_, _}, tile_types, opts \\ []) do

    opts = Keyword.merge([
      edge_cost: fn(_, _) -> 1 end,
    ], opts)

    Astar.astar(
      {
        # All reachable
        fn({x, y}) ->
          # Create a list of empty nodes around current position.
          [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
          |> Enum.map(fn({a, b}) ->
            {x+a, y+b}
          end)
          |> Enum.filter(fn({a, b}) ->
            Map.get(map, {a, b}) in tile_types
          end)
          #|> IO.inspect
        end,

        # Edge cost between two nodes.
        # Hier kommt eine Funktion hin, welche sagt wie viel ein Weg kostet.
        opts[:edge_cost],

        # Estimated cost between two nodes.
        # Hier kommt eine Funktion hin, welche die anzahl Züge zwischen zwei Koordinaten berechnet.
        fn({_fromX, _fromY} = x, {_toX, _toY} = y) ->
          # It does not matter if the function is completely correct, because its only a relative value.
          distance_coords(x, y)
          #|> IO.inspect
        end,
      },
      from,
      to
    )
    #|> IO.inspect
  end

  #--- Helpers ---

  # Will print human readable map from tiles.
  defp print_map(%{"game" => %{"board" => %{"size" => size, "tiles" => tiles}}}) do
    IO.puts("+" <> String.duplicate("-", size*2) <> "+")
    tiles
    |> String.to_charlist
    |> Enum.chunk(size*2)
    |> Enum.each(fn(line) -> IO.puts("|#{line}|") end)
    IO.puts("+" <> String.duplicate("-", size*2) <> "+")
  end

  # Will return a line showing all info about hero.
  defp hero_stats_line(%{"crashed" => crashed, "elo" => elo, "gold" => gold, "id" => id,
                          "life" => life, "mineCount" => mines, "name" => name,
                          "pos" => _, "spawnPos" => _, "userId" => _}) do
    "#{name} (@#{id}): #{life}/#{@hero_life_max} mines:#{mines} gold:#{gold} elo:#{elo} crashed:#{inspect crashed}"
  end

  # Will build a map from tiles using hero position as 0,0
  defp build_map(%{"game" => %{"board" => %{"size" => size, "tiles" => tiles}}} = state) do
    {hero_x, hero_y} = hero_position(state)
    tiles
    |> String.to_charlist
    |> Enum.chunk(size*2)
    |> Enum.with_index
    |> Enum.flat_map(fn({line, x}) ->
      line
      |> Enum.chunk(2)
      |> Enum.with_index
      |> Enum.map(fn({string, y}) ->
        {
          # New center of map is current hero position.
          {x-hero_x, y-hero_y},
          tile_type(string)
        }
      end)
    end)
    |> Enum.into(%{})
  end

  # Hero helpers
  defp hero_position(%{"hero" => %{"pos" => %{"x" => x, "y" => y}}}), do: {x, y}
  defp hero_id(%{"hero" => %{"id" => id}}), do: id
  defp hero_life(%{"hero" => %{"life" => life}}), do: life
  defp hero_life(%{heroes_map: %{1 => %{"life" => life}}}, 1), do: life
  defp hero_life(%{heroes_map: %{2 => %{"life" => life}}}, 2), do: life
  defp hero_life(%{heroes_map: %{3 => %{"life" => life}}}, 3), do: life
  defp hero_life(%{heroes_map: %{4 => %{"life" => life}}}, 4), do: life

  defp other_hero_ids(state), do: [1,2,3,4] -- [hero_id(state)]
  defp other_hero_positions(%{heroes_map: heroes_map} = state) do
    state
    |> other_hero_ids
    |> Enum.map(fn(hero_id) ->
      %{"x" => x, "y" => y} = get_in(heroes_map, [hero_id, "pos"])
      {x, y}
    end)
  end

  defp add_map(state) do
    Map.put(state, :map, build_map(state))
  end

  defp add_heroes_map(%{"game" => %{"heroes" => heroes}} = state) do
    Map.put(state, :heroes_map, Enum.into(heroes, %{}, fn(%{"id" => id} = hero) -> {id, hero} end))
  end

  defp add_routes(state) do
    task_taverns = Task.async(fn -> route_taverns(state) end)
    task_heroes = Task.async(fn -> route_heroes(state) end)
    task_mines = Task.async(fn -> route_mines(state) end)

    state
    |> Map.put(:route_taverns, Task.await(task_taverns))
    |> Map.put(:route_heroes, Task.await(task_heroes))
    |> Map.put(:route_mines, Task.await(task_mines))
  end

  defp ok(val), do: {:ok, val}

  defp distance_coords({fromX, fromY}, {toX, toY}) do
    :math.sqrt(:math.pow(abs(fromX-toX), 2) + :math.pow(abs(fromY-toY), 2))
  end

end

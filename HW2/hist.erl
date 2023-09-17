-module(hist).
-export([new/1, update/3]).

new(Name) ->
%%  all numbers are less than inf
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false -> {new, [{Node, N} | History]};
    {_, Counter} ->
      if Counter < N ->
        {new, [{Node, N} |
          lists:keydelete(Node, 1, History)]};
        true -> old
      end
  end.
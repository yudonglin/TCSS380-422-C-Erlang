%%%-------------------------------------------------------------------
%%% @author Yudong Lin & Wei Wei Chien & Michael Theisen
%%%-------------------------------------------------------------------

-module(bst).

-export([insert/2, countLeaves/1, nodes/1]).

-type bst() :: empty
| {integer(), bst(), bst()}.

insert(N, Tree) ->
  case Tree of
    empty -> {N, empty, empty};
    {Value, Left, Right} ->
      if
        N < Value -> {Value, insert(N, Left), Right};
        N > Value -> {Value, Left, insert(N, Right)}
      end
  end.

countLeaves(Tree) ->
  case Tree of
    empty -> 0;
    {_, empty, empty} -> 1;
    {_, Left, Right} -> countLeaves(Left) + countLeaves(Right)
  end.

nodes(Tree) ->
  FindNodes = fun F(TheTree) ->
    case TheTree of
      empty -> 0;
      {_, empty, empty} -> 1;
      {_, Left, Right} -> 1 + F(Left) + F(Right)
    end
              end,
  Size = FindNodes(Tree),
  Leaves = countLeaves(Tree),
  {Size, Leaves, Size - Leaves}.
defmodule Util do
  # This is how the if macro is implemented (i.e the macro just calls a function which
  # does the actual transformation)
  defmacro aif(condition, clauses) do
    build_aif(condition, clauses)
  end
  defp build_aif(condition, do: do_clause) do
    build_aif(condition, do: do_clause, else: nil)
  end
  defp build_aif(condition, do: do_clause, else: else_clause) do  
    quote do
      var!(it) = unquote(condition)
      case var!(it) do
        x when x in [false, nil] -> unquote(else_clause)
        _ -> unquote(do_clause)
      end
    end
  end
  defmacro let(bindings, body) do
    build_let(bindings, body)
  end
  defp build_let(bindings, do: body) do
    vars = for {name, val} <- bindings do
      quote do unquote(Macro.var(name, nil)) = unquote(val) end
    end
    quote do
      with unquote_splicing(vars), do: unquote(body)
    end
  end
  # Just as a reference for how lists work
  def cons(x,y) do [x | y] end
  def car(x) do hd(x) end
  def cdr(x) do tl(x) end
end

# defmodule UtilTest do
#   def aif_test(expr) do
#     require Util
#     aif expr do
#       it
#     else
#       []
#     end
#   end
# end
  

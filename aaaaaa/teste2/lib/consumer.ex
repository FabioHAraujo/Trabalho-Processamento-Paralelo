defmodule Consumer do

  def start(manager_pid) do
    spawn(fn -> loop(manager_pid) end)
  end

  def loop(manager_pid) do
    receive do
      {:run, work} ->
        :timer.sleep(work * 1000)
        send manager_pid, {:done, self}
        loop(manager_pid)
    end
  end

end

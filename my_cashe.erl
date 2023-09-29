-module(my_cashe).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

-record(cache_entry, {value, expire_time}).

 
create(TableName) ->
    ets:new(TableName, [set, public]).

 
insert(TableName, Key, Value) ->
    ets:insert(TableName, {Key, #cache_entry{value=Value, expire_time=undefined}}).

 
insert(TableName, Key, Value, ExpireTime) when is_integer(ExpireTime) ->
    CurrentTime = erlang:system_time(seconds),
    ExpireAt = add_seconds(CurrentTime, ExpireTime),
    erlang:display(ExpireAt),
    ets:insert(TableName, {Key, #cache_entry{value=Value, expire_time=ExpireAt}}).

 
 lookup(TableName, Key) ->
    CurrentTime = erlang:system_time(seconds),
    case ets:lookup(TableName, Key) of
        [{Key, #cache_entry{value=Value, expire_time=undefined}}] ->
            {ok, Value}; 
        [{Key, #cache_entry{value=Value, expire_time=ExpireTime}}] when ExpireTime == undefined ->
            {ok, Value}; 
        [{Key, #cache_entry{value=Value, expire_time=ExpireTime}}] when is_integer(ExpireTime), ExpireTime > CurrentTime ->
            {ok, Value}; 
        _ ->
            undefined
    end.
 
delete_obsolete(TableName) ->
    CurrentTime = erlang:system_time(seconds),
    F = fun({_Key, #cache_entry{expire_time=undefined}}) -> false;
           ({_Key, #cache_entry{expire_time=ExpireTime}}) when ExpireTime =< CurrentTime -> true;
           (_) -> false
       end,
    ets:delete_all_objects(TableName, F).

 
add_seconds( Secs, Seconds) ->
    Secs + Seconds.

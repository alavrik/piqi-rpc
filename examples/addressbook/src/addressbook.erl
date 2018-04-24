-module(addressbook).
-compile([export_all, nowarn_export_all]).


-include("addressbook_piqi.hrl").
-behaviour(addressbook_piqi).


-define(DB_FILE, "addressbook").


read_addressbook() ->
    case file:read_file(?DB_FILE) of
        {ok, Bin} -> binary_to_term(Bin);
        {error, enoent} -> []
    end.


write_addressbook(Book) ->
    file:write_file(?DB_FILE, term_to_binary(Book)).



rpc_handle_add_person(Person) ->
    io:format("add_person: ~p~n", [Person]),

    Book = read_addressbook(),

    case lists:keymember(Person#person.id, #person.id, Book) of
        true ->
            {error, "person already added"};
        false ->
            write_addressbook([Person | Book]),
            ok
    end.


rpc_handle_get_person(Input) ->
    #get_person_input{ id = Id } = Input,
    io:format("get_person: ~p~n", [Id]),

    Book = read_addressbook(),

    case lists:keyfind(Id, #person.id, Book) of
        false ->
            {error, "no such person"};
        Person ->
            {ok, Person}
    end.


rpc_handle_list_people(_) ->
    Book = read_addressbook(),
    {ok, Book}.


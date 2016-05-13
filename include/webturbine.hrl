-define(ACCEPT(T), {T, accept_content}).
-define(PROVIDE(T), {T, provide_content}).

-type wtb_route_name() :: atom().
-type wtb_route_path() :: [string() | atom()].
-type wtb_type() :: json | binary | text | html | requested | any | none | string().
-type wtb_method() :: 'GET' | 'PUT' | 'POST' | 'DELETE'.

-record(wtb_route, {name :: wtb_route_name(),
                    path :: [wtb_route_path()],
                    methods = ['GET'] :: [wtb_method()],
                    provides = [any] :: [wtb_type()],
                    accepts = [any] :: [wtb_type()],
                    routes = [] :: [wtb_route()],
                    prefix = [] :: [wtb_route_path()],                    
                    handlers = [] :: [{atom(), function()}],
                    resource :: module(),
                    state :: any(),
                    request :: any()}).
-type wtb_route() :: #wtb_route{}.

-type wtb_json_resp() :: [{atom() | binary(), term()}].
-type wtb_text_resp() :: string().
-type wtb_binary_resp() :: binary().
-type wtb_content() :: wtb_json_resp() | 
                       wtb_text_resp() | 
                       wtb_binary_resp().
-type wtb_resp() :: {halt, non_neg_integer()} | 
                    {{halt, non_neg_integer()}, wtb_content()} |
                    {error, not_found} |
                    {error, term()} | 
                    wtb_content().

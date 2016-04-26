-include_lib("webmachine/include/webmachine.hrl").

-define(ACCEPT(T), {T, accept_content}).
-define(PROVIDE(T), {T, provide_content}).
-define(JSON_TYPE, "application/json").
-define(TEXT_TYPE, "plain/text").
-define(HTML_TYPE, "text/html").
-define(OCTET_TYPE, "application/octet-stream").
-define(FORM_TYPE, "application/x-www-form-urlencoded").
-define(PROVIDE_TEXT, [{?TEXT_TYPE, provide_text_content}]).
-define(ACCEPT_TEXT, [?ACCEPT(?FORM_TYPE),
                      ?ACCEPT(?OCTET_TYPE),
                      ?ACCEPT(?TEXT_TYPE),
                      ?ACCEPT(?JSON_TYPE)]).

-type wtb_type() :: json | binary | text | html | requested | any | none | string().

-type wtb_req() :: #wm_reqdata{}.

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

-type wtb_callback_def() :: {wtb_route_name(), wtb_type(), wtb_req()}.

-type wtb_route_name() :: atom().
-type wtb_route_path() :: [string() | atom()].
-record(wtb_route, {name :: wtb_route_name(),
                    prefix = [] :: [wtb_route_path()],
                    path :: [wtb_route_path()],
                    methods = ['GET'] :: ['GET' | 'PUT' | 'POST' | 'DELETE'],
                    accepts = [any] :: [wtb_type()],
                    provides = [any] :: [wtb_type()]}).
-type wtb_route() :: #wtb_route{}.

-module(nova_controller).
-export([
         status_code/1
        ]).

status_code(#{status := 500, stacktrace := Stacktrace}) ->
    {ok, #{status => 500,
           message => get_text_from_status(500),
           stacktrace => Stacktrace},
     #{view => "nova_error"}};
status_code(#{status := StatusCode}) ->
    {ok,
     #{
       status => StatusCode,
       message => get_text_from_status(StatusCode)
      },
     #{view => "nova_error"}}.

get_text_from_status(400) -> <<"Bad Request">>;
get_text_from_status(401) -> <<"Unauthorized">>;
get_text_from_status(402) -> <<"Payment Required">>;
get_text_from_status(403) -> <<"Forbidden">>;
get_text_from_status(404) -> <<"Not Found">>;
get_text_from_status(405) -> <<"Method Not Allowed">>;
get_text_from_status(406) -> <<"Not Acceptable">>;
get_text_from_status(407) -> <<"Proxy Authentication Required">>;
get_text_from_status(408) -> <<"Request Timeout">>;
get_text_from_status(409) -> <<"Conflict">>;
get_text_from_status(410) -> <<"Gone">>;
get_text_from_status(411) -> <<"Length Required">>;
get_text_from_status(412) -> <<"Precondition Failed">>;
get_text_from_status(413) -> <<"Payload Too Large">>;
get_text_from_status(414) -> <<"URI Too Long">>;
get_text_from_status(415) -> <<"Unsupported Media Type">>;
get_text_from_status(416) -> <<"Range Not Satisfiable">>;
get_text_from_status(417) -> <<"Expection Failed">>;
get_text_from_status(418) -> <<"I'm a teapot">>;
get_text_from_status(421) -> <<"Misdirected Request">>;
get_text_from_status(422) -> <<"Unprocessable Entity">>;
get_text_from_status(423) -> <<"Locked">>;
get_text_from_status(424) -> <<"Failed Dependency">>;
get_text_from_status(425) -> <<"Too Early">>;
get_text_from_status(426) -> <<"Upgrade Required">>;
get_text_from_status(428) -> <<"Precondition Required">>;
get_text_from_status(429) -> <<"Too Many Requests">>;
get_text_from_status(431) -> <<"Request Header Fields Too Large">>;
get_text_from_status(451) -> <<"Unavailable For Legal Reasons">>;
get_text_from_status(500) -> <<"Internal Server Error">>;
get_text_from_status(501) -> <<"Not Implemented">>;
get_text_from_status(502) -> <<"Bad Gateway">>;
get_text_from_status(503) -> <<"Service Unavailable">>;
get_text_from_status(504) -> <<"Gateway Timeout">>;
get_text_from_status(505) -> <<"HTTP Version Not Supported">>;
get_text_from_status(506) -> <<"Variant Also Negotiates">>;
get_text_from_status(507) -> <<"Insufficient Storage">>;
get_text_from_status(508) -> <<"Loop Detected">>;
get_text_from_status(510) -> <<"Not Extended">>;
get_text_from_status(511) -> <<"Network Authentication Required">>.

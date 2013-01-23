%% @author maciek
%% @doc @todo Add description to url_download_server.


-module(url_download_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([pull/1,report/5]).



%% ====================================================================
%% Internal functions
%% ====================================================================

pull(_Number)->
	Url = link_server:get_link(),
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	[{random:uniform(2000000),Url}].

report(Url_id, Url, Redirected_url, Source, Status)->
	case Status of
		ok -> 
			Urls = processing_handler:process_data(Url_id, Url, Source),
			%%file:write_file(string:join(string:tokens(Url,":/_?.="),""), Urls),
			lager:log(debug,self(),[string:concat("Pobrano ",Url)]);
		error ->
			lager:log(warning,self(),[string:concat("Byl problem z ",Url)])
	end.

%% @author maciek
%% @doc @todo Add description to downloader.


-module(downloader).

-import(ibrowse).

%% ====================================================================
%% API functions
%% ====================================================================
-export([download/4,forward/1,forward_headers/5]).


%% ====================================================================
%% Internal functions
%% ====================================================================

download(Url,Url_id,Redirect_counter,{_,_,ConnectionTimeout, DownloadTimeout, RedirectLimit}) when RedirectLimit == Redirect_counter ->  
	url_download_server:report(Url,Url_id,<<>>,error);

download(Url,Url_id,Redirects_counter,{_,_,ConnectionTimeout, DownloadTimeout, RedirectLimit}=ConnCfg)->
	try
		case ibrowse:send_req(Url, [], get,[],[{connect_timeout, ConnectionTimeout}], DownloadTimeout) of  
			{ok, "200", _Headers, WebPageContent} ->
				url_download_server:report(Url_id,Url,WebPageContent,ok);
			{ok, "301", Headers, _B} ->
				forward_headers(Url_id,Url, Headers, Redirects_counter,ConnCfg);
			{ok, "302", Headers, _B} ->
				forward_headers(Url_id,Url, Headers, Redirects_counter,ConnCfg);
			_ ->
				url_download_server:report(Url_id,Url,<<>>,error)				
		end
	catch
		error:Error ->
			url_download_server:report(Url_id,Url,<<>>,error);
		exit:_ ->
			url_download_server:report(Url,Url_id,<<>>,error)
	end.

forward_headers(Url,Url_id,Headers, Redirects_no,ConnCfg) ->
	case forward(Headers) of
		none -> url_download_server:report(Url,Url_id,<<>>,fail);
		NewUrl -> download(NewUrl,Url_id,Redirects_no+1,ConnCfg)
	end.

forward([])->
	none;
forward([{"location",Url} | _T])->
	Url;
forward([{"Location",Url} | _T])->
	Url;
forward([_H|T])->
	forward(T);
forward(_)->
	none.
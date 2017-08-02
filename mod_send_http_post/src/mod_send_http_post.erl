%%%-------------------------------------------------------------------
%%% @author Hamdullah Shah & Milos Pajic
%%% ©copyright (C) 2017-2018, Affinitas.de
%%% @doc
%%%
%%%
%%% @end
%%% Create: 27 June 2017 
%%%-------------------------------------------------------------------
-module(mod_send_http_post).
-author('H.Shah & Milos P.').
%%-behavior(gen_mod).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("xmpp.hrl").
-export([start/2,
	 stop/1,
	%% mod_opt_type/1,	
	%% depends/2,		
	 user_send_packet/1]).
-define(DEFAULT_DOMAIN_URL, "http://10.98.65.166:8080/").
-define(DEFAULT_FILTER_PATH, "/connectionApproval").
-define(DENIED, <<"denied">>).
-record(conversationmessage, {from = <<>> :: binary(),
                              to = <<>> :: binary(),
                              message = <<>> :: binary()}).
start(_Host,_Opts) ->
        ejabberd_hooks:add(user_send_packet, _Host, ?MODULE, user_send_packet, 100),
        ok.
stop(_Host)->
        ejabberd_hooks:delete(user_send_packet, _Host, ?MODULE, user_send_packet, 100),
        ok.
%%mod_opt_type(_) ->
%%        [].
%%depends(_, _) ->
%%        [].

user_send_packet({#message{type = T, body = B} = Packet,C2SState}) 
        when T == chat ->               %%If type of the packet is Chat then...
	case xmpp:get_text(B) of
	<<"">> -> {Packet, C2SState};
	_ ->        
		NewPacket = check_for_permission(Packet),
        	{NewPacket, C2SState}
	end;

user_send_packet(Acc) ->
        Acc.

check_for_permission(Packet) ->
        ConvMsg = parse_xmpp_message(Packet),
        Response = request_filter_service(ConvMsg),
        ?INFO_MSG("response: ~p",[Response]),
        case Response of
                {error,Reason} ->
                        ?INFO_MSG("Error:~p",[Reason]),
                        Packet;
                XML ->
                        case is_user_allowed(XML) of
                                ?DENIED ->
                                        ?INFO_MSG("conversation message denied",[]),    
                                        xmpp:make_error(Packet, xmpp:err_forbidden(<<"You are not allowed to send message">>,<<"">>));
                                _ -> 
                                        ?INFO_MSG("conversation message allowed",[]),
                                        Packet
                        end
        end.

request_filter_service(#conversationmessage{} = ConvMsg) ->
        RequestType = post,
        URL = create_url("http://jidfilter:8080","/connectionApproval", ConvMsg),
        ?INFO_MSG("Chat: ~p",[URL]),
        Encoding = "application/x-www-form-urlencoded",
        Request = {URL,[],Encoding, ""},
        Response  = httpc:request(RequestType, Request,[], []),
        filter_service_response(Response).

filter_service_response({error, Response}) ->
        {error, Response};

filter_service_response({ok, {{_, 200, _}, _, Body}}) ->
        valid_XML(Body).
valid_XML(XML) ->       
        ParsedXML = fxml_stream:parse_element(list_to_binary(XML)),
        case ParsedXML of
                {error, Error} -> {erorr,Error};
                Result -> Result
        end.
is_user_allowed(XML) ->
        fxml:get_subtag_cdata(XML,<<"status">>).

create_url(Domain, Path, #conversationmessage{} = ConvMsg) ->
          Domain ++
                Path ++ 
                "?from=" ++ 
                ConvMsg#conversationmessage.from ++ 
                "&to=" ++ 
                ConvMsg#conversationmessage.to.

parse_xmpp_message(#message{body = Body} = Msg) ->
        ToJID = xmpp:get_to(Msg),
        FromJID = xmpp:get_from(Msg),
        ChatText = xmpp:get_text(Body),
        To = binary:bin_to_list(jlib:jid_to_string(ToJID)),
        From = binary:bin_to_list(jlib:jid_to_string(FromJID)),
        #conversationmessage{from = From,
                                to = To,
                                message = ChatText}.

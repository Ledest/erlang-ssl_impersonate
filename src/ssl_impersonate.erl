-module(ssl_impersonate).

-include("ssl_impersonate.hrl"). 

-export([ssl_opts/1]).

encoded_supported_groups() -> <<?UINT16(?GREASE_SG), ?UINT16(?X25519), ?UINT16(?SECP256R1), ?UINT16(?SECP384R1)>>.

encoded_signature_algorithms() ->
    <<?UINT16(?ECDSA_SECP256R1_SHA256),
      ?UINT16(?RSA_PSS_RSAE_SHA256),
      ?UINT16(?RSA_PKCS1_SHA256),
      ?UINT16(?ECDSA_SECP384R1_SHA384),
      ?UINT16(?RSA_PSS_RSAE_SHA384),
      ?UINT16(?RSA_PKCS1_SHA384),
      ?UINT16(?RSA_PSS_RSAE_SHA512),
      ?UINT16(?RSA_PKCS1_SHA512)>>.

encoded_supported_versions() -> <<?UINT16(?GREASE_SV), ?UINT16(?TLSV13), ?UINT16(?TLSV12)>>.

encode_extension(Type, Len) -> <<?UINT16(Type), ?UINT16(Len)>>.

encode_extension(Type, Len, ExtLen) -> <<?UINT16(Type), ?UINT16(Len), ExtLen>>.
encode_extension(?SUPPORTED_VERSIONS, Len, ExtLen, ExtData) ->
    <<?UINT16(?SUPPORTED_VERSIONS), ?UINT16(Len), ExtLen, ExtData/binary>>;
encode_extension(?COMPRESS_CERTIFICATE, Len, ExtLen, ExtData) ->
    <<?UINT16(?COMPRESS_CERTIFICATE), ?UINT16(Len), ExtLen, ExtData/binary>>;
encode_extension(Type, Len, ExtLen, ExtData) when is_binary(ExtData) ->
    <<?UINT16(Type), ?UINT16(Len), ?UINT16(ExtLen), ExtData/binary>>;
encode_extension(Type, Len, ExtLen, ExtData) -> <<?UINT16(Type), ?UINT16(Len), ExtLen, ExtData>>.

encode_extension(?APPLICATION_SETTINGS, Len, StatusType, ListLen, ExtLen) ->
    <<?UINT16(?APPLICATION_SETTINGS), ?UINT16(Len), ?UINT16(StatusType), ListLen, ?UINT16(ExtLen)>>;
encode_extension(Type, Len, StatusType, ListLen, ExtLen) ->
    <<?UINT16(Type), ?UINT16(Len), StatusType, ?UINT16(ListLen), ?UINT16(ExtLen)>>.

ssl_opts(Host) ->
  [{versions, ['tlsv1.3', 'tlsv1.2']},
   {verify, verify_none},
   {server_name_indication, Host},
   {alpn_advertised_protocols, [<<"h2">>, <<"http/1.1">>]},
   {key_share_groups, [grease, x25519]},
   {substitute_cipher_suites,
    [?GREASE,
     ?TLS_AES_128_GCM_SHA256,
     ?TLS_AES_256_GCM_SHA384,
     ?TLS_CHACHA20_POLY1305_SHA256, 
     ?TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384, 
     ?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256, 
     ?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256, 
     ?TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA, 
     ?TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA, 
     ?TLS_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_RSA_WITH_AES_256_GCM_SHA384, 
     ?TLS_RSA_WITH_AES_128_CBC_SHA, 
     ?TLS_RSA_WITH_AES_256_CBC_SHA]},
   {substitute_client_extensions,
    [sni,
     encode_extension(?EXTENDED_MASTER_SECRET_EXT, 0),
     encode_extension(?RENEGOTIATION_INFO, 1, 0),
     encode_extension(?SUPPORTED_GROUPS, 10, 8, encoded_supported_groups()),
     encode_extension(?EC_POINT_FORMATS, 2, 1, 0),
     encode_extension(?SESSION_TICKET, 0),
     alpn,
     encode_extension(?STATUS_REQUEST, 5, 1, 0, 0), 
     encode_extension(?SIGNATURE_ALGORITHMS, 18, 16, encoded_signature_algorithms()),
     encode_extension(?SIGNATURE_CERTIFICATE_TIMESTAMP, 0),
     key_share,
     encode_extension(?PSK_KEY_EXCHANGE_MODES, 2, 1, 1),
     encode_extension(?SUPPORTED_VERSIONS, 7, 6, encoded_supported_versions()),
     encode_extension(?COMPRESS_CERTIFICATE, 3, 2, <<?UINT16(2)>>),
     encode_extension(?APPLICATION_SETTINGS, 5, 3, 2, ?H2)]}].

%
% This file is part of uProtoBuf.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0
%

-module(uprotobuf_decoder_test).

-include_lib("eunit/include/eunit.hrl").

decode_varint_test() ->
    Schema = #{
        a => {1, int32}
    },
    DecoderSchema = uprotobuf_decoder:transform_schema(Schema),
    ?assertEqual(#{a => 150}, uprotobuf_decoder:parse(<<16#08, 16#96, 16#01>>, DecoderSchema)).

decode_string_test() ->
    Schema = #{
        b => {2, string}
    },
    DecoderSchema = uprotobuf_decoder:transform_schema(Schema),
    ?assertEqual(
        #{b => <<"testing">>},
        uprotobuf_decoder:parse(
            <<16#12, 16#07, 16#74, 16#65, 16#73, 16#74, 16#69, 16#6E, 16#67>>, DecoderSchema
        )
    ).

decode_submsg_test() ->
    Schema = #{
        c => {3, #{a => {1, int32}}}
    },
    DecoderSchema = uprotobuf_decoder:transform_schema(Schema),
    ?assertEqual(
        #{c => #{a => 150}},
        uprotobuf_decoder:parse(<<16#1A, 16#03, 16#08, 16#96, 16#01>>, DecoderSchema)
    ).

decode_int32_string_int32_message_test() ->
    Encoded =
        <<16#10, 16#05, 16#1A, 16#0B, 16#48, 16#65, 16#6C, 16#6C, 16#6F, 16#20, 16#57, 16#6F, 16#72,
            16#6C, 16#64, 16#20, 16#C5, 16#0F>>,
    Schema = #{
        b => {2, int32},
        c => {3, string},
        d => {4, int32}
    },
    DecoderSchema = uprotobuf_decoder:transform_schema(Schema),
    ?assertEqual(
        #{b => 5, c => <<"Hello World">>, d => 1989},
        uprotobuf_decoder:parse(Encoded, DecoderSchema)
    ).

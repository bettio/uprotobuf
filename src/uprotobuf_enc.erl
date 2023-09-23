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

-module(uprotobuf_enc).
-export([encode/2]).

-define(LEN_TAG, 2).
-define(FIXED32_TAG, 5).

encode(Map, Schema) ->
    Iterator = maps:iterator(Map),
    encode(maps:next(Iterator), Schema, [<<>>]).

encode(none, _Schema, Acc) ->
    Acc;
encode({K, V, I}, Schema, Acc) ->
    {FieldNum, Type} = maps:get(K, Schema),
    NewAcc = [encode_field(FieldNum, V, Type) | Acc],
    encode(maps:next(I), Schema, NewAcc).

encode_field(FieldNum, V, bytes) ->
    Len = erlang:iolist_size(V),
    [encode_varint((FieldNum bsl 3) bor ?LEN_TAG), encode_varint(Len), V];
encode_field(FieldNum, V, {enum, LabelsToInt}) ->
    IntVal = maps:get(V, LabelsToInt),
    [encode_varint(FieldNum bsl 3), encode_varint(IntVal)];
encode_field(FieldNum, V, int32) ->
    [encode_varint(FieldNum bsl 3), encode_varint(V)];
encode_field(FieldNum, V, sfixed32) ->
    [encode_varint((FieldNum bsl 3) bor ?FIXED32_TAG), encode_sfixed32(V)];
encode_field(FieldNum, V, MapSchema) when is_map(MapSchema) ->
    Encoded = encode(V, MapSchema),
    Len = erlang:iolist_size(Encoded),
    [encode_varint((FieldNum bsl 3) bor ?LEN_TAG), encode_varint(Len), Encoded];
encode_field(_FiledNum, _V, _Type) ->
    [].

encode_sfixed32(Int) ->
    <<Int:32/little-signed-integer>>.

% TODO: right now we focus to positive ints up to UINT32_MAX and something
encode_varint(Int) when Int < 0 ->
    error(badarg);
% 2^7 - 1
encode_varint(Int) when Int =< 127 ->
    [Int];
% 2^14 - 1
encode_varint(Int) when Int =< 16383 ->
    Int0 = Int band 16#7F,
    Int1 = Int bsr 7,
    <<1:1, Int0:7, 0:1, Int1:7>>;
% 2^21 - 1
encode_varint(Int) when Int =< 2097151 ->
    Int0 = Int band 16#7F,
    Int1 = (Int bsr 7) band 16#7F,
    Int2 = Int bsr 14,
    <<1:1, Int0:7, 1:1, Int1:7, 0:1, Int2:7>>;
% 2^28 - 1
encode_varint(Int) when Int =< 268435456 ->
    Int0 = Int band 16#7F,
    Int1 = (Int bsr 7) band 16#7F,
    Int2 = (Int bsr 14) band 16#7F,
    Int3 = Int bsr 21,
    <<1:1, Int0:7, 0:1, Int1:7, 0:1, Int2:7, 0:1, Int3:7>>;
% 2^35 - 1
encode_varint(Int) when Int =< 34359738367 ->
    Int0 = Int band 16#7F,
    Int1 = (Int bsr 7) band 16#7F,
    Int2 = (Int bsr 14) band 16#7F,
    Int3 = (Int bsr 21) band 16#7F,
    Int4 = Int bsr 28,
    <<1:1, Int0:7, 1:1, Int1:7, 1:1, Int2:7, 1:1, Int3:7, 0:1, Int4:7>>;
encode_varint(_Int) ->
    error(badarg).

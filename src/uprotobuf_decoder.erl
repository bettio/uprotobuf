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

-module(uprotobuf_decoder).
-export([parse/2, transform_schema/1]).

transform_schema(Schema) ->
    Iterator = maps:iterator(Schema),
    decode_schema(maps:next(Iterator), #{}).

decode_schema(none, Acc) ->
    Acc;
decode_schema({K, {FieldNum, Type}, I}, Acc) when
    is_atom(Type) and is_integer(FieldNum) and (FieldNum >= 0)
->
    decode_schema(maps:next(I), Acc#{FieldNum => {K, Type}});
decode_schema({K, {FieldNum, Type}, I}, Acc) when
    is_map(Type) and is_integer(FieldNum) and (FieldNum >= 0)
->
    decode_schema(maps:next(I), Acc#{FieldNum => {K, transform_schema(Type)}});
decode_schema({K, {FieldNum, {enum, LabelsToInts}}, I}, Acc) when
    is_map(LabelsToInts) and is_integer(FieldNum) and (FieldNum >= 0)
->
    decode_schema(maps:next(I), Acc#{FieldNum => {K, {enum, transform_enum_map(LabelsToInts)}}});
decode_schema({K, T, _I}, _Acc) ->
    error({badarg, K, T}).

transform_enum_map(Map) ->
    Iterator = maps:iterator(Map),
    transform_enum_map(maps:next(Iterator), #{}).

transform_enum_map(none, Acc) ->
    Acc;
transform_enum_map({K, V, I}, Acc) ->
    transform_enum_map(maps:next(I), Acc#{V => K}).

parse(Bin, Schema) ->
    parse(Bin, Schema, tag, #{}).

parse(<<>>, _Schema, tag, Acc) ->
    Acc;
parse(Bin, Schema, What, Acc) ->
    case What of
        tag ->
            parse_varint(Bin, 0, 0, value, Schema, Acc);
        value ->
            [Tag | _Built] = Acc,
            WireType = Tag band 7,
            case WireType of
                0 -> parse_varint(Bin, 0, 0, end_of_field, Schema, Acc);
                1 -> parse_fixed64(Bin, end_of_field, Schema, Acc);
                2 -> parse_varint(Bin, 0, 0, len_field_value, Schema, Acc);
                3 -> {error, unsupported_group};
                4 -> {error, invalid};
                5 -> parse_fixed32(Bin, end_of_field, Schema, Acc);
                6 -> {error, unsupported_feature};
                7 -> {error, unsupported_feature}
            end;
        end_of_field ->
            [Value, Tag | Built] = Acc,
            FieldNum = Tag bsr 3,
            {Key, Type} = maps:get(FieldNum, Schema, {x, undefined}),
            NewAcc = maps:put(Key, cast(Value, Type), Built),
            parse(Bin, Schema, tag, NewAcc);
        len_field_value ->
            [Len, Tag | Built] = Acc,
            <<SubBin:Len/binary, Rest/binary>> = Bin,
            FieldNum = Tag bsr 3,
            {Key, Type} = maps:get(FieldNum, Schema, {x, undefined}),
            NewAcc = maps:put(Key, cast(SubBin, Type), Built),
            parse(Rest, Schema, tag, NewAcc)
    end.

parse_varint(<<0:1, IntValue:7, Rest/binary>>, IntAcc, Bytes, Next, Schema, Acc) when Bytes =< 10 ->
    VarInt = (IntValue bsl 7 * Bytes) bor IntAcc,
    parse(Rest, Schema, Next, [VarInt | Acc]);
parse_varint(<<1:1, IntValue:7, Rest/binary>>, IntAcc, Bytes, Next, Schema, Acc) when Bytes =< 10 ->
    VarInt = (IntValue bsl 7 * Bytes) bor IntAcc,
    parse_varint(Rest, VarInt, Bytes + 1, Next, Schema, Acc);
parse_varint(Bin, _IntAcc, _Bytes, _Next, _Schema, Acc) ->
    {invalid, Bin, Acc}.

parse_fixed32(<<Fixed32:4/binary, Rest/binary>>, Next, Schema, Acc) ->
    parse(Rest, Schema, Next, [Fixed32 | Acc]).

parse_fixed64(<<Fixed64:8/binary, Rest/binary>>, Next, Schema, Acc) ->
    parse(Rest, Schema, Next, [Fixed64 | Acc]).

cast(Value, int32) ->
    Value;
cast(Value, {enum, IntToLabels}) ->
    case maps:find(Value, IntToLabels) of
        {ok, Label} -> Label;
        error -> Value
    end;
cast(<<Value:32/integer-little-unsigned>>, fixed32) ->
    Value;
cast(<<Value:32/integer-little-signed>>, sfixed32) ->
    Value;
cast(Value, undefined) ->
    Value;
cast(Value, bytes) ->
    Value;
cast(Value, string) ->
    Value;
cast(Value, Proto) when is_map(Proto) ->
    parse(Value, Proto).

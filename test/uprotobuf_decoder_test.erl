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
    ?assertEqual(uprotobuf_decoder:parse(<<16#08, 16#96, 16#01>>, DecoderSchema), #{a => 150}).

/*
 * Copyright 2022 Greenfossil Pte Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.greenfossil.data.mapping

case class AuthorizationRequest(
                                 id: Long = 0,
                                 responseType: String,
                                 clientId: String,
                                 subjectId: Long,
                                 scope: String,
                                 redirectURI: String,
                                 stateOpt: Option[String] = None,
                                 nonce: Option[String] = None,
                                 display: Option[String] = None,
                                 prompt: Option[String] = None,
                                 maxAge: Option[Long] = None,
                                 uiLocales: Option[String] = None,
                                 claimsLocales: Option[String] = None,
                                 idTokenHint: Option[String] = None,
                                 loginHint: Option[String] = None,
                                 responseMode: Option[String] = None,
                                 acrValues: Option[String] = None,
                                 claims: Option[String] = None,
                                 sectorIdentifierURI: Option[String] = None,
                                 authCode: Option[String] = None,
//                                 sessionState: Option[String] = None,
//                                 created: Option[LocalDateTime] = None,
//                                 codeChallenge: Option[String] = None,
//                                 codeChallengeMethod: Option[String] = None,
//                                 method: String,
//                                 userAgent: String,
//                                 authorization: Option[String],
//                                 ipAddress: String
                               )

import com.greenfossil.data.mapping.Mapping.*
import scala.annotation.unused

class LargeCaseClassMappingSuite extends munit.FunSuite {

  test("large case class"){

    @unused val largeCaseClassForm = Mapping.mapping[AuthorizationRequest](
      "id" -> longNumber,
      "responseType" -> text,
      "clientId" -> text,
      "subjectId" -> longNumber,
      "scope" -> text,
      "redirectURI" -> text,
      "stateOpt" -> optional(text),
      "nonce" -> optional(text),
      "display" -> optional(text),
      "prompt" -> optional(text),
      "maxAge" -> optional(longNumber),
      "uiLocales" -> optional(text),
      "claimsLocales" -> optional(text),
      "idTokenHint" -> optional(text),
      "loginHint" -> optional(text),
      "responseMode" -> optional(text),
      "acrValues" -> optional(text),
      "claims" -> optional(text),
      "sectorIdentifierURI" -> optional(text),
      "authCode" -> optional(text),
//      "sessionState" -> optional(text),
      //  "created" -> optional(localDateTime),
      //  "codeChallenge" -> optional(text),
      //  "codeChallengeMethod" -> optional(text),
      //  "method" -> text,
      //  "userAgent" -> text,
      //  "authorization" -> optional(text),
      //  "ipAddress" -> text
    )
  }


}

package com.github.andyglow.jsonschema

import com.github.andyglow.jsonschema.model._
import zio.json._

object UserProfileJson {

  implicit val CredentialsW: JsonEncoder[Credentials] = DeriveJsonEncoder.gen[Credentials]

  implicit val NotesW: JsonEncoder[Notes] = DeriveJsonEncoder.gen[Notes]

  implicit val BetaFeatureW: JsonEncoder[BetaFeature] =
    JsonEncoder[String] contramap { betaFeature =>
      betaFeature match {
        case F0 => "feature-0-name"
        case F1 => "feature-1-name"
        case F2 => "feature-2-name"
      }
    }

  implicit val RoleW: JsonEncoder[Role] =
    JsonEncoder[String] contramap { role =>
      role match {
        case Role.User    => "e-user"
        case Role.Manager => "e-manager"
        case Role.Admin   => "e-admin"
      }
    }

}

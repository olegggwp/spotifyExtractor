package ru.belyaev.config

import scala.annotation.unused
import cats.effect.Sync
import pureconfig.generic.ProductHint
import pureconfig.generic.semiauto.deriveReader
import pureconfig.module.catseffect.syntax.CatsEffectConfigSource
import pureconfig.{ConfigReader, ConfigSource}


case class ApplicationConfig (
                               spotifyClientConfig : SpotifyClientConfig
                             )

object ApplicationConfig {
  @unused private implicit def hint[T]: ProductHint[T] = ProductHint[T](allowUnknownKeys = false)

  implicit val configReader: ConfigReader[ApplicationConfig] = {
    @unused implicit val spotifyClientReader: ConfigReader[SpotifyClientConfig] =
      deriveReader[SpotifyClientConfig]


    deriveReader[ApplicationConfig]
  }

  def usafeLoad[F[_] : Sync](
                              config: ConfigSource = ConfigSource.default
                            ): F[ApplicationConfig] =
    config.at("ru.belyaev").loadF[F, ApplicationConfig]()


}

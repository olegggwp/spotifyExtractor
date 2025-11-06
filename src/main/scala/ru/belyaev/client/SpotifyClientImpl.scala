package ru.belyaev.client

import cats.Monad
import cats.data.EitherT
import cats.syntax.functor.*
import ru.belyaev.config.SpotifyClientConfig
import sttp.client4.{Backend, UriContext, basicRequest}

import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import java.util.Base64
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.parser.*
import cats.syntax.all.*

private class SpotifyClientImpl[F[_] : Monad](
                                                 config: SpotifyClientConfig,
                                                 backend: Backend[F]
                                               ) extends SpotifyClient[F] {

  case class Song(id: String, name: String)

  case class Track(track: Song)

  case class Playlist(items: List[Track])

  implicit val songDecoder: Decoder[Song] = deriveDecoder[Song]
  implicit val songEncoder: Encoder[Song] = deriveEncoder[Song]
  implicit val trackDecoder: Decoder[Track] = deriveDecoder
  implicit val trackEncoder: Encoder[Track] = deriveEncoder
  implicit val playlistDecoder: Decoder[Playlist] = deriveDecoder[Playlist]
  implicit val playlistEncoder: Encoder[Playlist] = deriveEncoder[Playlist]
  implicit val fooDecoder: Decoder[TokensResponse] = deriveDecoder[TokensResponse]
  implicit val fooEncoder: Encoder[TokensResponse] = deriveEncoder[TokensResponse]
  implicit val userProfileDecoder: Decoder[UserProfile] = deriveDecoder[UserProfile]
  implicit val userProfileEncoder: Encoder[UserProfile] = deriveEncoder[UserProfile]


  override def step1(): String = {
    val mainURL = "https://accounts.spotify.com/authorize"
    val redirect_uri = config.redirectURL
    val scope = "user-read-private user-read-email playlist-modify-public playlist-modify-private"
    val resStr = uri"$mainURL"
      .addParam("response_type", "code")
      .addParam("client_id", config.clientId)
      .addParam("scope", scope)
      .addParam("redirect_uri", redirect_uri)
    resStr.toString
  }


  override def step2(code: String): F[Either[String, TokensResponse]] = {

    val sp = "https://accounts.spotify.com/api/token"
    val grant_type = "authorization_code"
    val redirect_uri = config.redirectURL
    val hashed = ""
    val authorization = "Basic " + Base64.getEncoder.encodeToString(
      (config.clientId + ":" + config.clientSecret)
        .getBytes(StandardCharsets.UTF_8))
    val txt = basicRequest
      .headers(Map("content-type" -> "application/x-www-form-urlencoded", "Authorization" -> authorization))
      .post(uri"$sp"
        .addParam("grant_type", grant_type)
        .addParam("redirect_uri", redirect_uri)
        .addParam("code", code)
      )
      .send(backend)
      .map(_.body)
    toTokensResponse(txt)
  }

  private def toTokensResponse(str: F[Either[String, String]]): F[Either[String, TokensResponse]] = {
    str.map(x =>
        x.flatMap(jsonstr =>
          decode[TokensResponse](jsonstr)
            .leftMap(err => s"error while parsing tokens response: $err"))
      )
  }


  override def step2parse(callbackUrlWithCode: String): Either[String, String] = {
    val basePattern = Regex.quote(config.redirectURL)
    val CodePattern = s"""^$basePattern\\?code=([^&]+).*$$""".r

    callbackUrlWithCode match {
      case CodePattern(code) => Right(code)
      case _ => Left("URL не соответствует ожидаемому формату")
    }
  }
  

  override def getUserId(token: String): F[Either[String, String]] = {
    basicRequest
      .headers(Map("Authorization" -> ("Bearer " + token)))
      .get(uri"https://api.spotify.com/v1/me")
      .send(backend)
      .map(_.body)
      .map(_.flatMap(
          decode[UserProfile](_)
            .leftMap(err => s"error while parsing user profile response: $err")
            .map(_.id)
        )
      )
  }


  private def getPlaylist(token: String, playlist_id: String): F[Either[String, Playlist]] = {
    basicRequest
      .headers(Map("Authorization" -> ("Bearer " + token)))
      .get(uri"https://api.spotify.com/v1/playlists/$playlist_id/tracks"
        .addParam("fields", "items(track(name,id))")
      )
      .send(backend)
      .map(_.body)
      .map(_.flatMap(
        decode[Playlist](_)
          .leftMap(err => s"error while parsing playlist response: $err")
      )
      )
  }

  override def getPlaylistStr(token: String, playlist_id: String): F[Either[String, String]] = {
    getPlaylist(token, playlist_id).map(_.map(
      _.items.map(_.track.name).fold("playlist:\n")((a, b) => a + "\n" + b)))
  }

  override def unionPlaylists(token: String, playlist1_id: String, playlist2_id: String): F[Either[String, String]] = {
    def findUniqueSongs(p1: Playlist, p2: Playlist): List[String] = {
      val p2SongIds: Set[String] = p2.items.map(_.track.id).toSet
      p1.items
        .map(_.track.id)
        .filter(!p2SongIds.contains(_))
    }

    (for {
      a <- EitherT(getPlaylist(token, playlist1_id))
      b <- EitherT(getPlaylist(token, playlist2_id))
      addToB = findUniqueSongs(a, b)
      res <- EitherT(addSongs(token, playlist2_id, addToB))
    } yield res).value
  }

  def addSongs(token : String, playlist_id : String, toAdd: List[String]): F[Either[String, String]] = {
    basicRequest
      .headers(Map("Authorization" -> ("Bearer " + token)))
      .post(uri"https://api.spotify.com/v1/playlists/$playlist_id/tracks"
        .addParam("position", "0")
        .addParam("uris", toAdd.map("spotify:track:" + _).mkString(","))
      )
      .send(backend)
      .map(_.body)
  }

}



object SpotifyClientImpl {

  def apply[F[_] : Monad](
                             config: SpotifyClientConfig,
                             backend: Backend[F]
                           ): SpotifyClient[F] = new SpotifyClientImpl(config, backend)

}

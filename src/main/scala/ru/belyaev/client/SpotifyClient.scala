package ru.belyaev.client
//import ru.belyaev.client.TokensResponse

trait SpotifyClient[F[_]] {

  def step1(): String

  def step2parse(callbackUrlWithCode: String): Either[String, String]

  def step2(code: String): F[Either[String, TokensResponse]]

  def getUserId(token: String): F[Either[String, String]]

  def getPlaylistStr(token: String, playlist_id: String): F[Either[String, String]]

  def unionPlaylists(token: String, playlist1_id: String, playlist2_id: String): F[Either[String, String]]

  case class UserProfile
  (
    country: String,
    display_name: String,
    email: String,
    href: String,
    id: String
  )

  case class TokensResponse
  (
    access_token: String,
    token_type: String,
    scope: String,
    expires_in: Int,
    refresh_token: String
  )

  
}

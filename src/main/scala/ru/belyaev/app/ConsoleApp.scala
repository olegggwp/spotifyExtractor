package ru.belyaev.app

import cats.effect.std.Console
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Applicative, Monad, MonadThrow}
import cats.syntax.all._

import ru.belyaev.client.SpotifyClient

import scala.util.matching.Regex

private class ConsoleApp[F[_] : MonadThrow : Console](
                                                       client: SpotifyClient[F]
                                                     ) {

  import ConsoleApp.*

  def loop: F[Unit] =
    Monad[F].iterateWhile {
      Console[F].println("Привет! Доступные команды:\n" +
        "start : начать процесс авторизации\n" +
        "exit : выйти\n" +
        "Самые интересные вещи начнутся как только Вы авторизируетесь!") *>
        Console[F].readLine.flatTap {
          case step1Regex() => authStep1()
          case debugReg(token) => Console[F].println("Вы вошли в авторизованный режим.") *> mainLoop(token)
          case exit() => Applicative[F].unit
          case cmd => Console[F].println(s"Неизвестная команда: '$cmd' . Доступные команды : start , exit")
        }
    } {
      case exit() => false
      case _ => true
    }.void

  private def authStep1(): F[Unit] = {
    val url = client.step1()
    for {
      _ <- Console[F].println(s"Перейди сюда: $url")
      _ <- Console[F].println("и когда тебя перекинет на страницу, впиши сюда то, что находится в строке браузера:")
      url <- Console[F].readLine
      _ <- client.step2parse(url) match {
        case Left(error) => Console[F].println(s"ошибка : $error")
        case Right(code) =>
          //          Console[F].println(s"code : $code") *>
          step2cmdcode(code)
      }
    } yield ()
  }

  private def step2cmdcode(code: String): F[Unit] = {
    client.step2(code).attempt.flatMap {
      case Left(throwable) => Console[F].println(s"Exception was thrown при попытке получить токен: ${throwable.getMessage}")
      case Right(response) =>
        response match {
          case Left(error) => Console[F].println(s"Сервер выдал ошибку при попытке получить токен: $error")
          case Right(tokensResponse: client.TokensResponse) =>
            Console[F].println(s"Ура, вы авторизировались! \n Запрашиваю ваш профиль...")
              *> printProfile(tokensResponse.access_token)
              *> Console[F].println("Вы вошли в авторизованный режим.")
              *> mainLoop(tokensResponse.access_token)
        }
    }
  }

  private def printProfile(token: String): F[Unit] = {
    client.getUserId(token).attempt.flatMap {
      case Left(throwable) => Console[F].println(s"Exception was thrown при попытке получить профиль пользователя: ${throwable.getMessage}")
      case Right(response) =>
        response match {
          case Left(error) => Console[F].println(s"Сервер выдал ошибку при попытке получить профиль пользователя: $error")
          case Right(userId) =>
            Console[F].println(s"Ваш id : $userId")
        }
    }
  }

  private def mainLoop(token: String): F[Unit] = Monad[F].iterateWhile {
    Console[F].println("\nДоступные комманды:\n" +
      "user : вывести информацию о пользователе\n" +
      "play <id> : вывести плейлист по его id \n" +
      "(чтобы узнать какой у плейлиста id, " +
      "посмотрите на ссылку по которой он доступен, " +
      "она должна выглядеть примерно так:  https://open.spotify.com/playlist/<id>)\n" +
      "union <id> <id> : единственная полезная команда, объединяет песни из двух плейлистов в один. " +
      "(Т. е. Все песни, которые есть в плейлисте-1 но которых нет в плейлисте-2 будут добавлены в плейлист-2)\n" +
      "exit : выйти из авторизированного режима") *>
      Console[F].readLine.flatTap {
        case userReg() => printProfile(token)
        case playReg(playlist_id) => printPlaylist(token, playlist_id)
        case unionReg(playlist1_id, playlist2_id) => unionPlaylists(token, playlist1_id, playlist2_id)
        case exit() => Applicative[F].unit
        case cmd => Console[F].println(s"Неизвестная команда: '$cmd' . ")
      }
  } {
    case exit() => false
    case _ => true
  }.void

  private def printPlaylist(token: String, playlist_id: String): F[Unit] = {
    Console[F].println(s"Запрашиваю данные плейлиста...") *>
      client.getPlaylistStr(token, playlist_id).attempt.flatMap {
        case Left(throwable) => Console[F].println(s"Exception was thrown при попытке получить плейлист: ${throwable.getMessage}")
        case Right(response) =>
          response match {
            case Left(error) => Console[F].println(s"Сервер выдал ошибку при попытке получить плейлист: $error")
            case Right(playlistStr) =>
              Console[F].println(s"Ваш плейлист : $playlistStr")
          }
      }
  }

  private def unionPlaylists(token: String, playlist1_id: String, playlist2_id: String): F[Unit] = {
    Console[F].println(s"Соединяю плейлисты...") *>
      client.unionPlaylists(token, playlist1_id, playlist2_id).attempt.flatMap {
        case Left(throwable) => Console[F].println(s"Exception was thrown при попытке соединить плейлисты: ${throwable.getMessage}")
        case Right(response) =>
          response match {
            case Left(error) => Console[F].println(s"Сервер выдал ошибку при попытке соединить плейлисты: $error")
            case Right(str) =>
              Console[F].println(s"Получилось! Вот что сервер сказал напоследок : $str")
          }
      }
  }

}


object ConsoleApp {

  def start[F[_] : MonadThrow : Console](
                                          client: SpotifyClient[F]
                                        ): F[Unit] = new ConsoleApp[F](client).loop

  private val step1Regex: Regex = """^start\s*$""".r
  private val getPlaylistsRegex: Regex = """^playlists\s*$""".r
  private val userReg: Regex = """^user\s*$""".r
  private val playReg: Regex = """^play\s+(.*)$""".r
  private val unionReg: Regex = """^union\s+(\S+)\s+(\S+)$""".r
  private val debugReg: Regex = """^debug\s+(.*)$""".r

  private val exit: Regex = """^exit\s*$""".r

}

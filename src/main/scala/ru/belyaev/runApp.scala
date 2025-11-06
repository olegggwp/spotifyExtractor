package ru.belyaev

import cats.effect.Resource
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.effect.std.Console
import ru.belyaev.app.ConsoleApp
import ru.belyaev.client.{SpotifyClient, SpotifyClientImpl}
import ru.belyaev.config.ApplicationConfig
import sttp.client4.httpclient.cats.HttpClientCatsBackend


object runApp extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =

    application[IO]
      .attempt
      .flatMap {
        case Left(th) =>
          IO.println(s"Exit with error:${th.getMessage}")
            .as(ExitCode.Error)
        case Right(_) =>
          IO.println("Exit 'Ok'")
            .as(ExitCode.Success)
      }
      .onCancel(IO.println("Exit 'Cancel'"))


  private def application[F[_] : Async : Console]: F[Unit] = (
    for {
      _ <- Resource.make(Console[F].println("Starting application"))(_ =>
        Console[F].println("Application closed")
      )
      config <- Resource.eval(ApplicationConfig.usafeLoad())
      backend <- HttpClientCatsBackend.resource[F]()
      searchClient: SpotifyClient[F] <- Resource.pure(SpotifyClientImpl(config.spotifyClientConfig, backend))
      _ <- Resource.make(Console[F].println("Application started"))(_ =>
        Console[F].println("Start closing application")
      )
    } yield searchClient
    ).use(ConsoleApp.start(_))


}

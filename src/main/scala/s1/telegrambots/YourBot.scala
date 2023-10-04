package s1.telegrambots
import s1.telegrambots.BasicBot
import scala.util.Using
import scala.io.Source.fromURL


object YourBot extends App:
    object Bot extends BasicBot:
        
        /**
         * TODO: Luokaa bottinne tähän metodeineen ja reagoijineen.
         */

        this.onUserCommand("ruokalista", ruokalista)

        def haeJSON() =
          val json = fromURL(s"https://kitchen.kanttiinit.fi/menus?lang=fi&restaurants=&days=2023-10-4").mkString
          json

        def ruokalista(message: Message): String = ???

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot

    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot

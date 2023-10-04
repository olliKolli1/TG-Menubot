package s1.telegrambots
import s1.telegrambots.BasicBot
import scala.util.Using
import scala.io.Source
import scala.util.Random



object YourBot extends App:
    object Bot extends BasicBot:

        private val pics: Vector[String] = Vector("1.png", "19.png", "40.png", "46.png", "92.png", "104.png", "105.png")
        private val words: Vector[String] = Vector("Ronaldo", "CR7", "Cristiano Ronaldo", "Penaldo")

        def ronaldo(msg: Message) =
          val id = getChatId(msg)
          val m = getString(msg)
          if words.contains(m) then
            sendPhoto(pics(Random.between(0,6)), id)
            writeMessage("SIIUUU", id)

        /**
         * TODO: Luokaa bottinne tähän metodeineen ja reagoijineen.
         */

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot

    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot

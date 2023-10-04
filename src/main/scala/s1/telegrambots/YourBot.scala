package s1.telegrambots
import s1.telegrambots.BasicBot
import scala.io.Source.fromURL
import scala.util.Random

object YourBot extends App:
    object Bot extends BasicBot:

        //Ronaldoittamiseen käytetyt kuvat ja aputekstin pätkät
        private val pics: Vector[String] = Vector("kuvat/1.png", "kuvat/19.png", "kuvat/40.png", "kuvat/46.png", "kuvat/92.png", "kuvat/104.png", "kuvat/105.png")
        private val words: Vector[String] = Vector("ronaldo", "cr7", "cristiano ronaldo", "penaldo")

        //testataan sisältääkö lähetetty viesti jonkin avainsanoista, mikäli sisältää käytetään getRonaldoed metodia
        def doesMsgContainCR7(message: Message): Unit =
          val ms = getString(message).toLowerCase()
          println(ms)
          if this.words.contains(ms) then
            getRonaldoed(message)

        //metodi, joka lähettää ryhmään viestin "SIIUUU" ja hassunhauskan kuvan
        def getRonaldoed(message: Message) =
          val id = getChatId(message)
          sendPhoto(pics(Random.between(0,6)), id)
          writeMessage("SIIUUU", id)

        //tapahtumankuuntelija ronaldoittamiselle, kutsutaan metodia, joka tarkistaa onko viestissä avainsanoja
        this.onUserMessage(doesMsgContainCR7)

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot
    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot

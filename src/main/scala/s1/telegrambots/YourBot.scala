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
          if ms.contains(this.words(0)) || ms.contains(this.words(1)) || ms.contains(this.words(2)) || ms.contains(this.words(3)) then
            getRonaldoed(message)

        //metodi, joka lähettää ryhmään viestin "SIIUUU" ja hassunhauskan kuvan
        def getRonaldoed(message: Message) =
          val id = getChatId(message)
          sendPhoto(pics(Random.between(0,6)), id)
          writeMessage("SIIUUU", id)

        //tapahtumankuuntelija ronaldoittamiselle, kutsutaan metodia, joka tarkistaa onko viestissä avainsanoja
        this.onUserMessage(doesMsgContainCR7)

        def getJSON: String =
          val date = "2023-10-04"
          val json = fromURL(s"https://kitchen.kanttiinit.fi/menus?lang=fi&restaurants=&days=${date}").mkString
          val tTalo = "Tietotekniikantalo\n" + tTalonRuokalista(json)
          val täffä = "Täffä\n" + täffänRuokalista(json)
          val alvari = "Alvari\n" + alvarinRuokalista(json)
          val tuas = "TUAS\n" + tuasinRuokalista(json)
          val dipoli = "Dipoli Ravintolat\n" + dipolinRuokalista(json)
          val kipsariVäre = "KipsariVäre\n" + kipsarinVäreenRuokalista(json)
          val studioKipsari = "Studio Kipsari\n" + studioKipsarinRuokalista(json)
          val aBloc = "A Bloc\n" + aBlocinRuokalista(json)
          val arvo = "Arvo\n" + arvonRuokalista(json)
          val kvarkki = "Kvarkki\n" + tTalonRuokalista(json)



        //T-talon ja kvarkin ruokalistat
        def tTalonRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"1\"")
          val endIndex = json.indexOfSlice("}]}")
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Täffän ruokalista
        def täffänRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"3\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def alvarinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"5\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Tuasin Ruokalista ruokalista
        def tuasinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"7\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def dipolinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"45\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def kipsarinVäreenRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"50\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def studioKipsarinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"51\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def aBlocinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"52\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //Alvarin ruokalista
        def arvonRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"59\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 19), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot
    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot

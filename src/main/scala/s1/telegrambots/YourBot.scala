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


        //tapahtumankuuntelija Help-komennolle ja toiminnallisuus alla
        this.onUserCommand("help", help)

        def help(message: Message): String =
          "/ruokalista[päivämäärä (muodossa: vvvvkkpp)]: Tiedot päivän ruokatarjonnasta \n" +
          "/ravintolat: Opiskelijaravintolat ja niiden ID:t \n" +
          "/lempiravintola[ravintolan ID] muuttaa lempiravintolaasi antamasi ID:n mukaisesti \n" +
          "Oikeilla avainsanoilla saa kuvia \"vuohesta\""


        //näillä voi muuttaa lempiravintolaa
        private var fave: Option[String] = None

        this.onUserCommand("lempiravintola1", to1)
        def to1(message: Message): String =
          this.fave = Some("1")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola3", to3)
        def to3(message: Message): String =
          this.fave = Some("3")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola5", to5)
        def to5(message: Message): String =
          this.fave = Some("5")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola7", to7)
        def to7(message: Message): String =
          this.fave = Some("7")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola45", to45)
        def to45(message: Message): String =
          this.fave = Some("45")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola50", to50)
        def to50(message: Message): String =
          this.fave = Some("50")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola51", to51)
        def to51(message: Message): String =
          this.fave = Some("51")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola52", to52)
        def to52(message: Message): String =
          this.fave = Some("52")
          "Suosikkia muutettu."

        this.onUserCommand("lempiravintola59", to59)
        def to59(message: Message): String =
          this.fave = Some("59")
          "Suosikkia muutettu."

        //"tarpeettomat" testausmetodit
        this.onUserCommand("fave", favo)

        def favo(message: Message): String =
          this.fave match
            case None => "ei ole"
            case Some(value) => value

        //tällä saa listan ravintoloiden id:istä
        this.onUserCommand("ravintolat", teksti)

        def teksti(message: Message): String =
          "T-talo tai Kvarkki: 1, \nTäffä: 3, \nAlvari: 5, \nTuas: 7, \nDipoli: 45, \nKipsari väre: 50, \nStudio Kipsari: 51, \nA bLoc: 52, \nArvo: 59"

        def convertDate(date: Seq[String]): String =
          val dateInRightFormat = date.slice(0, 4).mkString + "-" + date.slice(4, 6).mkString + "-" + date.slice(6, 8).mkString
          ruokalista(dateInRightFormat)

        //ruokalistan hakumetodi
        def ruokalista(date: String): String =
          val json = fromURL(s"https://kitchen.kanttiinit.fi/menus?lang=fi&restaurants=&days=${date}").mkString
          val tTalo = "Tietotekniikantalo ja Kvarkki\n" + tTalonRuokalista(json)
          val täffä = "Täffä\n" + täffänRuokalista(json)
          val alvari = "Alvari\n" + alvarinRuokalista(json)
          val tuas = "TUAS\n" + tuasinRuokalista(json)
          val dipoli = "Dipoli Ravintolat\n" + dipolinRuokalista(json)
          val kipsariVäre = "KipsariVäre\n" + kipsarinVäreenRuokalista(json)
          val studioKipsari = "Studio Kipsari\n" + studioKipsarinRuokalista(json)
          val aBloc = "A Bloc\n" + aBlocinRuokalista(json)
          val arvo = "Arvo\n" + arvonRuokalista(json)
          s"Ruokalistat:\n$tTalo\n\n$täffä\n\n$alvari\n\n$tuas\n\n$dipoli\n\n$kipsariVäre\n\n$studioKipsari\n\n$aBloc\n\n$arvo"

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
          var sliced = json.slice( (startIndex + 20), endIndex)
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
          var sliced = json.slice( (startIndex + 20), endIndex)
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
          var sliced = json.slice( (startIndex + 20), endIndex)
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
          var sliced = json.slice( (startIndex + 20), endIndex)
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
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          sliced

        //tapahtumankuuntelija ruokalistoille
        this.onUserCommandWithArguments("ruokalista", convertDate)

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot
    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot





//private var lemppari: Option[String] = None
//
//        this.onUserCommand("lempiravintola", kysy)
//
//        def kysy(message: Message): String =
//          this.lisaaLemppari
//
//          "Lista ravintoloista, valitse 1 ja kirjoita id:\nKvarkki tai T-talo: 1, \nTäffä: 3, \nAlvari: 5, \nTuas: 7, \nDipoli: 45, \nKipsari väre: 50, \nStudio Kipsari: 51, \nA Bloc: 52"
//
//        def lisaaLemppari =
//          this.onUserMessageReply(metodi(String))
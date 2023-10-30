package s1.telegrambots
import s1.telegrambots.BasicBot
import scala.io.Source.fromURL


object YourBot extends App:
    object Bot extends BasicBot:

        //tapahtumankuuntelija Help-komennolle ja toiminnallisuus alla
        this.onUserCommand("help", help)
        def help(message: Message): String =
          "/ruokalista [päivämäärä (muodossa: vvvv-kk-pp)]: Tiedot päivän ruokatarjonnasta \n/ravintolat: Opiskelijaravintolat ja niiden ID:t \n/lempiravintola [ravintolan ID]: muuttaa lempiravintolaasi antamasi ID:n mukaisesti \n/lemppari: palauttaa lemppariravintolasi\n/täffä [pvm]: täffän ruokalista\n/tietotalo [pvm]: T-talon ja kvarkin ruokalista\n/tuas [pvm]: Tuasin ruokalista\n/alvari [pvm]: alvarin ruokalista\n/abloc [pvm]: A blocin ruokalista\n/dipoli [pvm]: Dipolin ruokalista"


        //lempiravintolan muuttuja
        private var fave: Option[String] = None

        //tapahtumankuuntelija ja toteutus lempiravintolan muuttamiselle
        this.onUserCommandWithArguments("lempiravintola", muutaRavintolaa)

        def muutaRavintolaa(index: Seq[String]): String =
          val indexAsString = index.mkString
          if indexAsString.toInt > 9 || indexAsString.toInt < 1 then
            "Väärä Indeksi !!!!!"
          else
            this.fave = Some(indexAsString)
            "Lemppari muutettu :)"

        //lemppari komennon tapahtumankuuntelija ja toteutus
        this.onUserCommand("lemppari", lempiravintola)

        def lempiravintola(message: Message): String =
          var id = ""
          this.fave match
            case None => id = "0"
            case Some(value) => id = value
          var palautus = ""
          if id.toInt == 1 then
            palautus = "T-talo ja Kvarkki (1)"
          else if id.toInt == 2 then
            palautus = "Täffä (2)"
          else if id.toInt == 3 then
            palautus = "Alvari (3)"
          else if id.toInt == 4 then
            palautus = "Tuas (4)"
          else if id.toInt == 5 then
            palautus = "Dipoli (5)"
          else if id.toInt == 6 then
            palautus = "Kipsari Väre (6)"
          else if id.toInt == 7 then
            palautus = "Studio Kipsari (7)"
          else if id.toInt == 8 then
            palautus = "A Bloc (8)"
          else if id.toInt == 9 then
            palautus = "Arvo (9)"
          else if id.toInt == 0 then
            palautus = "Et ole asettanut lempiravintolaa :("
          palautus

        //tällä saa listan ravintoloiden id:istä
        this.onUserCommand("ravintolat", teksti)

        def teksti(message: Message): String =
          "T-talo ja Kvarkki: 1, \nTäffä: 2, \nAlvari: 3, \nTuas: 4, \nDipoli: 5, \nKipsari väre: 6, \nStudio Kipsari: 7, \nA BLoc: 8, \nArvo: 9"

        def getJson(date: String): String =
          val json = fromURL(s"https://kitchen.kanttiinit.fi/menus?lang=fi&restaurants=&days=${date}").mkString
          json

        //ruokalistan hakumetodi
        def ruokalista(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          val tTalo = tTalonRuokalista(json)
          val täffä = täffänRuokalista(json)
          val alvari = alvarinRuokalista(json)
          val tuas = tuasinRuokalista(json)
          val dipoli = dipolinRuokalista(json)
          val kipsariVäre = kipsarinVäreenRuokalista(json)
          val studioKipsari = studioKipsarinRuokalista(json)
          val aBloc = aBlocinRuokalista(json)
          val arvo = arvonRuokalista(json)
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
          var nimi = "Tietotekniikantalo ja Kvarkki"
          if this.fave.getOrElse("0") == "1" then
            nimi = "** Tietotekniikantalo ja Kvarkki **"
          nimi + "\n" + sliced

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
          var nimi = "Täffä"
          if this.fave.getOrElse("0") == "2" then
            nimi = "** Täffä **"
          nimi + "\n" + sliced

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
          var nimi = "Alvari"
          if this.fave.getOrElse("0") == "3" then
            nimi = "** Alvari **"
          nimi + "\n" + sliced

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
          var nimi = "Tuas"
          if this.fave.getOrElse("0") == "4" then
            nimi = "** Tuas **"
          nimi + "\n" + sliced

        //Dipolin ruokalista
        def dipolinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"45\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          var nimi = "Dipoli"
          if this.fave.getOrElse("0") == "5" then
            nimi = "** Dipoli **"
          nimi + "\n" + sliced

        //Kipsari Väreen ruokalista
        def kipsarinVäreenRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"50\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          var nimi = "Kipsari Väre"
          if this.fave.getOrElse("0") == "6" then
            nimi = "** Kipsari Väre **"
          nimi + "\n" + sliced

        //Studio Kipsarin ruokalista
        def studioKipsarinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"51\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          var nimi = "Studio Kipsari"
          if this.fave.getOrElse("0") == "7" then
            nimi = "** Studio Kipsari **"
          nimi + "\n" + sliced

        //A Blocin ruokalista
        def aBlocinRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"52\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          var nimi = "A Bloc"
          if this.fave.getOrElse("0") == "8" then
            nimi = "** A Bloc **"
          nimi + "\n" + sliced

        //Arvon ruokalista
        def arvonRuokalista(json: String): String =
          val startIndex = json.lastIndexOfSlice("\"59\"")
          val endIndex = json.indexOfSlice("}]}", startIndex)
          var sliced = json.slice( (startIndex + 20), endIndex)
          sliced = sliced.replaceAll("\"title\"", "")
          sliced = sliced.replaceAll("},", "\n")
          sliced = sliced.replaceAll("\"", "")
          sliced = sliced.replaceAll("properties:", " allergeenitiedot: ")
          sliced = sliced.replaceAll("\\{:", "")
          var nimi = "Arvo"
          if this.fave.getOrElse(0) == "9" then
            nimi = "** Arvo **"
          nimi + "\n" + sliced

        //tapahtumankuuntelija ruokalistoille
        this.onUserCommandWithArguments("ruokalista", ruokalista)

        //täffän ruokalistan haku
        this.onUserCommandWithArguments("täffä", täffä)
        def täffä(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          täffänRuokalista(json)

        //A blocin ruokalistan haku
        this.onUserCommandWithArguments("abloc", abloc)
        def abloc(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          aBlocinRuokalista(json)

        //dipolin ruokalistan haku
        this.onUserCommandWithArguments("dipoli", dipoli)
        def dipoli(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          dipolinRuokalista(json)

        //tuasin ruokalistan haku
        this.onUserCommandWithArguments("tuas", tuas)
        def tuas(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          tuasinRuokalista(json)

        //alvarin ruokalistan haku
        this.onUserCommandWithArguments("alvari", alvari)
        def alvari(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          alvarinRuokalista(json)

        //t-talonruokalistan haku
        this.onUserCommandWithArguments("tietotalo", tietotalo)
        def tietotalo(date: Seq[String]): String =
          val dateAsString = date.mkString
          val json = getJson(dateAsString)
          tTalonRuokalista(json)

        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot
    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end YourBot




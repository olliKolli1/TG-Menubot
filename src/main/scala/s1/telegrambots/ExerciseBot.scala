package s1.telegrambots
import s1.telegrambots.BasicBot
import s1.telegrambots.ExerciseBot.Bot


object ExerciseBot extends App:
    object Bot extends BasicBot:

        def viestinTiedot(msg: Message): String =
            
            val len = getString(msg).length.toString

            /**
             * TODO: Hae viestistä lähettäjän nimi ja tallenna se tähän
             * Vinkki
            */
            val name: String = this.getUserFirstName(msg)
            
            //Funktio palauttaa Stringin, joka lähetetään vastauksena viestiin
            s"Message from $name was $len characters long"


        /*
         *TODO: Luo tähän alle reagoijat eli tapahtumankuuntelijan, joka reagoi käyttäjän viestiin.
         */
        this.onUserMessageReply(viestinTiedot)


        this.run()
        // Tarkistetaan, että lähti käyntiin
        println("Started the bot")

    end Bot

    // Tämä rivi pyytää ja ajaa täten yllä olevan botin
    val bot = Bot 
end ExerciseBot

      *****************************************************************
      *    COPY-MEMBER: DRPA0199                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONS-SCHNITTSTELLE FÜR DAS UPRO ZWPA0004              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    23.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!23.01.97!    ! NEU                       ! AUERBACH *
      * APA00068!18.11.99!    ! NEUE FUNKTION DRPA0776    ! BOHLINGER*
      * APA02579!07.03.18!2.2 ! NEUE FUNKTION DRPA1882    ! WANG     *
      * APA02855!13.05.19!2.3 ! NEUE FUNKTION DRPA1916    ! SCHEIDEL *
      * DSY05000!02.11.22!    ! NEUE FUNKTION DRPA1980    ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWPA0004.
           05  PI-ZWPA0004-FUNKTION            PIC  X(008).
               88 PI-ZWPA0004-DRPA0200              VALUE 'DRPA0200'.
               88 PI-ZWPA0004-DRPA0776              VALUE 'DRPA0776'.
070318         88 PI-ZWPA0004-DRPA1882              VALUE 'DRPA1882'.
130519         88 PI-ZWPA0004-DRPA1916              VALUE 'DRPA1916'.
021122         88 PI-ZWPA0004-DRPA1980              VALUE 'DRPA1980'.        
      *
      ******* ENDE COPY-MEMBER **************************************
 



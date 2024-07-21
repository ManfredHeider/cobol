      *****************************************************************
      *    COPY-MEMBER: DRKI1470                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1470                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.10.2002/16:00:01
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1470
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1470.
           05  PI-DRKI1470-KEY.


             10 PI-DRKI1470-VRBPERSONENID   PIC X(13).
             10 PI-DRKI1470-VRBPERSONENROLLEKZ   PIC X(01).
             10 PI-DRKI1470-VRBVERTRAGSID   PIC X(13).


           05  PI-DRKI1470-DATEN.


             10 PI-DRKI1470-VRBKOMMBETRKZ   PIC X(1).


      *
       01  PO-DRKI1470.
           05  PO-DRKI1470-RC                 PIC  X(003).
               88 PO-DRKI1470-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1470-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

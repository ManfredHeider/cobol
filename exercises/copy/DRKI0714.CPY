      *****************************************************************
      *    COPY-MEMBER: DRKI0714                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0714                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.10.98/15.48.06
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
      *    FÜR DIE FUNKTION DRKI0714
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0714.
           05  PI-DRKI0714-KEY.


             10 PI-DRKI0714-KVBKIPERSONENID   PIC X(13).
             10 PI-DRKI0714-KVBKIPERSONENROLLE   PIC X(01).
             10 PI-DRKI0714-KVBVBPERSONENID   PIC X(13).
             10 PI-DRKI0714-KVBVBPERSONENROLLE   PIC X(01).

      *
       01  PO-DRKI0714.
           05  PO-DRKI0714-RC                 PIC  X(003).
               88 PO-DRKI0714-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0714-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0714-DATEN.


             10 PO-DRKI0714-KVBVERMITTLERID   PIC X(13).
             10 PO-DRKI0714-KVBVERMITTLERROLLE   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************

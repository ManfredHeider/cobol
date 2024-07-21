      *****************************************************************
      *    COPY-MEMBER: DRKI2047                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2047                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.09.2007/08:36:13
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
      *    FÜR DIE FUNKTION DRKI2047
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2047.
           05  PI-DRKI2047-KEY.


             10 PI-DRKI2047-VMHLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2047-VMHPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2047-VMHVERTRAGSNUMMER   PIC X(15).
             
      *
       01  PO-DRKI2047.
           05  PO-DRKI2047-RC                 PIC  X(003).
               88 PO-DRKI2047-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2047-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2047-DATEN.


             10 PO-DRKI2047-LETZTEAENDERUNG   PIC X(26).
             10 PO-DRKI2047-VMHABSCHLVBNRALT   PIC S9(9) COMP.
             10 PO-DRKI2047-VMHABSCHLVBNRNEU   PIC S9(9) COMP.
             10 PO-DRKI2047-VMHJOBNAME   PIC X(8).
             10 PO-DRKI2047-VMHMANDANTNRALT   PIC S9(9) COMP.
             10 PO-DRKI2047-VMHMANDANTNRNEU   PIC S9(9) COMP.
             10 PO-DRKI2047-VMHVERTRAGSID   PIC X(13).
             

      *
      ******* ENDE COPY-MEMBER **************************************

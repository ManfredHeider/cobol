      *****************************************************************
      *    COPY-MEMBER: DRKI3908                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3908                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.02.2022 / 13:22:42 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-8787!11.10.21!    ! Neuerstellung             ! u130685  *
      * --------!--------!----!---------------------------!----------*
      * DBO-9707!08.02.22! v2 ! NEUES FELD G48BFFAELLIGDAT! u130685  *
      *         !        !    ! NEU GENERIERT             !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3908
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3908.
           05  PI-DRKI3908-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3908-G48BFUKTONR   PIC  X(0002).
             10 PI-DRKI3908-G48BFVNR   PIC  X(0015).
             10 PI-DRKI3908-G48BFWAEHRUNG   PIC  X(0003).
END   *

           05  PI-DRKI3908-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3908-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3908-G48BFAKTUSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFBIC   PIC  X(0011).
             10 PI-DRKI3908-G48BFDARLEHEN   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFDELKZ   PIC  X(0001).
             10 PI-DRKI3908-G48BFENDEDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFENDEZINSDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFENDEZRFREI   PIC  X(0008).
             10 PI-DRKI3908-G48BFFAELLIGDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFIBAN   PIC  X(0034).
             10 PI-DRKI3908-G48BFIBANBF   PIC  X(0034).
             10 PI-DRKI3908-G48BFKREDITAUSDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFKREDITZUSDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFMANDREF   PIC  X(0035).
             10 PI-DRKI3908-G48BFNNAMEBZ   PIC  X(0028).
             10 PI-DRKI3908-G48BFOEFDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFORWORDKZ   PIC  X(0001).
             10 PI-DRKI3908-G48BFPIN   PIC S9(9) COMP.
             10 PI-DRKI3908-G48BFRATEANT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3908-G48BFTILGUNGBEITR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFTILGUNGPROD   PIC  X(0010).
             10 PI-DRKI3908-G48BFTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3908-G48BFTILGUNGSART   PIC  X(0001).
             10 PI-DRKI3908-G48BFUEBERZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3908-G48BFVOLLAUSZDAT   PIC  X(0008).
             10 PI-DRKI3908-G48BFZAHLUNGSW   PIC  X(0001).
             10 PI-DRKI3908-G48BFZINSANT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3908-G48BFZINSART   PIC  X(0002).
             10 PI-DRKI3908-LETZTERAENDERER   PIC  X(0010).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3908-NI1.
           05 PI-DRKI3908-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3908-NI7.
           05 PI-DRKI3908-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3908.
           05  PO-DRKI3908-RC                     PIC  X(003).
               88 PO-DRKI3908-RC-OK               VALUE 'OK '.
               88 PO-DRKI3908-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
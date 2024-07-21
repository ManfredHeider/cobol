      *****************************************************************
      *    COPY-MEMBER: DRKI3906                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3906                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.02.2022 / 13:22:34 / U130685
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
      *    FÜR DIE FUNKTION DRKI3906
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3906.
           05  PI-DRKI3906-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3906-G48BFUKTONR   PIC  X(0002).
             10 PI-DRKI3906-G48BFVNR   PIC  X(0015).
             10 PI-DRKI3906-G48BFWAEHRUNG   PIC  X(0003).
END   *

      *
       01  PO-DRKI3906.
           05  PO-DRKI3906-RC                     PIC  X(003).
               88 PO-DRKI3906-RC-OK               VALUE 'OK '.
               88 PO-DRKI3906-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3906-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3906-G48BFAKTUSALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFANNUITAET   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFBIC   PIC  X(0011).
             10 PO-DRKI3906-G48BFDARLEHEN   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFDELKZ   PIC  X(0001).
             10 PO-DRKI3906-G48BFENDEDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFENDEZINSDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFENDEZRFREI   PIC  X(0008).
             10 PO-DRKI3906-G48BFFAELLIGDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFIBAN   PIC  X(0034).
             10 PO-DRKI3906-G48BFIBANBF   PIC  X(0034).
             10 PO-DRKI3906-G48BFKREDITAUSDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFKREDITZUSDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFMANDREF   PIC  X(0035).
             10 PO-DRKI3906-G48BFNNAMEBZ   PIC  X(0028).
             10 PO-DRKI3906-G48BFOEFDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFORWORDKZ   PIC  X(0001).
             10 PO-DRKI3906-G48BFPIN   PIC S9(9) COMP.
             10 PO-DRKI3906-G48BFRATEANT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3906-G48BFTILGUNGBEITR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFTILGUNGPROD   PIC  X(0010).
             10 PO-DRKI3906-G48BFTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3906-G48BFTILGUNGSART   PIC  X(0001).
             10 PO-DRKI3906-G48BFUEBERZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3906-G48BFVOLLAUSZDAT   PIC  X(0008).
             10 PO-DRKI3906-G48BFZAHLUNGSW   PIC  X(0001).
             10 PO-DRKI3906-G48BFZINSANT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3906-G48BFZINSART   PIC  X(0002).
             10 PO-DRKI3906-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI3906-LETZTERAENDERER   PIC  X(0010).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
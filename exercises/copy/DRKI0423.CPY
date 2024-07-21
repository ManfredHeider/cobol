      *****************************************************************
      *    COPY-MEMBER: DRKI0423                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0423                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.04.2023 / 14:34:37 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0423
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0423.
           05  PI-DRKI0423-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0423-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI0423-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0423-LTSVERTRAGSID   PIC  X(0013).
             10 PI-DRKI0423-LTSZAHLDYNERHOEH   PIC S9(4) COMP.
END   *

           05  PI-DRKI0423-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0423-NI-LTSBEITRZUSTID   PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSBTGRUEKZNPFL  PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSDATDYNERHOEH  PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSDPZNSBNT       PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSKLBRHSSCHL     PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSRABSTUFE      PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSRNTSCHL       PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSRNTSCHLDAT    PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSSCHICHTBEITR  PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSSCHCHTZGRTD    PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSTRBZCHNNG      PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSTTYPBEITR     PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSVERSSUMME     PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSVRTRZSTNDDT    PIC S9(4) COMP.
             15 PI-DRKI0423-N-LTSVRTRZSTNDD     PIC S9(4) COMP.
             15 PI-DRKI0423-NI-LTSWKZ           PIC S9(4) COMP.
END   *

           05  PI-DRKI0423-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0423-LTSBEITRZUSTID   PIC  X(0004).
             10 PI-DRKI0423-LTSBTGRUEKZNPFL   PIC  X(0001).
             10 PI-DRKI0423-LTSDATDYNERHOEH   PIC  X(010).
             10 PI-DRKI0423-LTSDEPZINSUEBANT   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0423-LTSKLBERHAUSSCHL   PIC  X(0001).
             10 PI-DRKI0423-LTSRABSTUFE   PIC  X(0003).
             10 PI-DRKI0423-LTSRNTSCHL   PIC  X(0001).
             10 PI-DRKI0423-LTSRNTSCHLDAT   PIC  X(010).
             10 PI-DRKI0423-LTSSCHICHTBEITR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0423-LTSSCHICHTZUGARTID   PIC  X(0001).
             10 PI-DRKI0423-LTSTARBEZEICHNUNG   PIC  X(0015).
             10 PI-DRKI0423-LTSTTYPBEITR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0423-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0423-LTSVERTRZUSTANDDAT   PIC  X(010).
             10 PI-DRKI0423-LTSVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0423-LTSWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0423-NI1.
           05 PI-DRKI0423-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 16.
       01  PI-DRKI0423-NI7.
           05 PI-DRKI0423-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 16.
      *
       01  PO-DRKI0423.
           05  PO-DRKI0423-RC                     PIC  X(003).
               88 PO-DRKI0423-RC-OK               VALUE 'OK '.
               88 PO-DRKI0423-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

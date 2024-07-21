      *****************************************************************
      *    COPY-MEMBER: DRKI0388                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0388                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.04.2023 / 14:34:33 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0388
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0388.
           05  PI-DRKI0388-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI0388-LTSTTNR   PIC S9(4) COMP.
             10 PI-DRKI0388-LTSVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0388-LTSVERTRAGSID   PIC  X(0013).
             10 PI-DRKI0388-LTSZAHLDYNERHOEH   PIC S9(4) COMP.
END   *

           05  PI-DRKI0388-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0388-NI-LTSBEITRZUSTID   PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSDATDYNERHOEH  PIC S9(4) COMP.
             15 PI-DRKI0388-N-LTSDPZNSBNT       PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSRABSTUFE      PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSRNTSCHL       PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSRNTSCHLDAT    PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSSCHICHTBEITR  PIC S9(4) COMP.
             15 PI-DRKI0388-N-LTSTRBZCHNNG      PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSTTYPBEITR     PIC S9(4) COMP.
             15 PI-DRKI0388-NI-LTSVERSSUMME     PIC S9(4) COMP.
             15 PI-DRKI0388-N-LTSVRTRZSTNDDT    PIC S9(4) COMP.
             15 PI-DRKI0388-N-LTSVRTRZSTNDD     PIC S9(4) COMP.
END   *

           05  PI-DRKI0388-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0388-LTSBEITRZUSTID   PIC  X(0004).
             10 PI-DRKI0388-LTSBTGRUEKZNPFL   PIC  X(0001).
             10 PI-DRKI0388-LTSDATDYNERHOEH   PIC  X(010).
             10 PI-DRKI0388-LTSDEPZINSUEBANT   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0388-LTSKLBERHAUSSCHL   PIC  X(0001).
             10 PI-DRKI0388-LTSRABSTUFE   PIC  X(0003).
             10 PI-DRKI0388-LTSRNTSCHL   PIC  X(0001).
             10 PI-DRKI0388-LTSRNTSCHLDAT   PIC  X(010).
             10 PI-DRKI0388-LTSSCHICHTBEITR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0388-LTSSCHICHTZUGARTID   PIC  X(0001).
             10 PI-DRKI0388-LTSTARBEZEICHNUNG   PIC  X(0015).
             10 PI-DRKI0388-LTSTTYPBEITR   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0388-LTSVERSSUMME   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI0388-LTSVERTRZUSTANDDAT   PIC  X(010).
             10 PI-DRKI0388-LTSVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0388-LTSWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0388-NI1.
           05 PI-DRKI0388-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 12.
       01  PI-DRKI0388-NI7.
           05 PI-DRKI0388-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 12.

      *
       01  PO-DRKI0388.
           05  PO-DRKI0388-RC                     PIC  X(003).
               88 PO-DRKI0388-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0388-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

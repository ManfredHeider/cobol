      ****************************************************************
      *    COPY-MEMBER: DRAN0352                                     *
      *    FUNKTION   : ERM. DB-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR DB                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01403!05.12.18!2.1  !NEU                        !HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1246!02.11.21! PARKDEPOT (C+L WG. DRAN0097) ! RAHNER   *
      * ANTRAG-1401!03.02.22! WIEDERHOLUNG ÜBERGABE AN-1246! RAHNER  *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0352                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0352.
           05 PI-DRAN0352-DB-KEY.
               10 PI-DRAN0352-PGEB-GES         PIC X(2).
               10 PI-DRAN0352-DB-HVANTNUM      PIC X(9).
      *
       01 PO-DRAN0352.
          05 PO-DRAN0352-RC                    PIC   X(003).
             88 PO-DRAN0352-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0352-RC-NOK                   VALUE 'NOK'.
          05 PO-DRAN0352-DATEN.
      *  OUTPUT DATA FOR MAP BAN0034
             10 PO-DRAN0352-VBNRO              PIC X(7).
             10 PO-DRAN0352-PRZVBO             PIC X(1).
             10 PO-DRAN0352-ZNAMEO             PIC X(26).
             10 PO-DRAN0352-VNAMEO             PIC X(26).
             10 PO-DRAN0352-PLZO               PIC X(5).
             10 PO-DRAN0352-ORTO               PIC X(21).
             10 PO-DRAN0352-AUSO               PIC X(3).
             10 PO-DRAN0352-PRODKEY1O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY1O    PIC 9(7).
             10 PO-DRAN0352-ZWP1O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ1O           PIC X(4).
             10 PO-DRAN0352-PRODKEY2O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY2O    PIC 9(7).
             10 PO-DRAN0352-ZWP2O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ2O           PIC X(4).
             10 PO-DRAN0352-PRODKEY3O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY3O    PIC 9(7).
             10 PO-DRAN0352-ZWP3O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ3O           PIC X(4).
             10 PO-DRAN0352-PRODKEY4O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY4O    PIC 9(7).
             10 PO-DRAN0352-ZWP4O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ4O           PIC X(4).
             10 PO-DRAN0352-PRODKEY5O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY5O    PIC 9(7).
             10 PO-DRAN0352-ZWP5O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ5O           PIC X(4).
             10 PO-DRAN0352-PRODKEY6O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY6O    PIC 9(7).
             10 PO-DRAN0352-ZWP6O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ6O           PIC X(4).
             10 PO-DRAN0352-PRODKEY7O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY7O    PIC 9(7).
             10 PO-DRAN0352-ZWP7O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ7O           PIC X(4).
             10 PO-DRAN0352-PRODKEY8O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY8O    PIC 9(7).
             10 PO-DRAN0352-ZWP8O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ8O           PIC X(4).
             10 PO-DRAN0352-PRODKEY9O          PIC 9(4).
             10 PO-DRAN0352-SUMME-PRODKEY9O    PIC 9(7).
             10 PO-DRAN0352-ZWP9O              PIC X(1).
             10 PO-DRAN0352-ELAUFZ9O           PIC X(4).
             10 PO-DRAN0352-VDABLEINO          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0352-HANDO              PIC X(1).
             10 PO-DRAN0352-ANARTO             PIC X(1).
             10 PO-DRAN0352-ANDATO             PIC X(8).
             10 PO-DRAN0352-AFO-34C-VBNR       PIC S9(9) COMP.
             10 PO-DRAN0352-WKZO               PIC X(1).
             10 PO-DRAN0352-WKZTO              PIC X(3).
             10 PO-DRAN0352-SONSTIGE.
                15 PO-DRAN0352-PINO            PIC S9(9) COMP.
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0289.
      *
      ******** ENDE COPY-MEMBER **************************************




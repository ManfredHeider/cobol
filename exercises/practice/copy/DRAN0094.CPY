      ****************************************************************
      *    COPY-MEMBER: DRAN0094                                     *
      *    FUNKTION   : ERM. DWS-BESTAND ÜBER GES, VORSCHLAGSNUMMER *
      *                 -> AUFBEREITET FÜR DWS                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1  !NEU                        !RAHNER   *
      * AAN00730!30.11.07!2.3  !ALLFINANZ                  !RAHNER   *
      * AAN01211!07.05.15!2.4  !ZW, LAUFZEIT               !HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1246!02.11.21! PARKDEPOT (C+L WG. DRAN0097) ! RAHNER   *
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
      *    FÜR DIE FUNKTION DRAN0094                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0094.
           05 PI-DRAN0094-DWS-KEY.
               10 PI-DRAN0094-PGEB-GES           PIC X(2).
               10 PI-DRAN0094-DWS-HVANTNUM       PIC X(9).
      *           OPTIONAL
               10 PI-DRAN0094-DWS-VDABVERM       PIC  X(7).
               10 PI-DRAN0094-DWS-VDABVERMPZ     PIC  X(1).
      *
       01 PO-DRAN0094.
          05 PO-DRAN0094-RC                      PIC   X(003).
             88 PO-DRAN0094-RC-OK                VALUE 'OK '.
             88 PO-DRAN0094-RC-NOK               VALUE 'NOK'.
             88 PO-DRAN0094-RC-KEY-NOT-UNIQUE    VALUE 'KNU'.
          05 PO-DRAN0094-DATEN.
      *  OUTPUT DATA FOR MAP BAN0022
             10 PO-DRAN0094-VBNRO               PIC X(7).
             10 PO-DRAN0094-PRZVBO             PIC X(1).
             10 PO-DRAN0094-ZNAMEO             PIC X(26).
             10 PO-DRAN0094-VNAMEO             PIC X(26).
             10 PO-DRAN0094-PLZO               PIC X(5).
             10 PO-DRAN0094-ORTO               PIC X(21).
             10 PO-DRAN0094-AUSO               PIC X(3).
             10 PO-DRAN0094-PRODKEY1O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY1O    PIC 9(7).
             10 PO-DRAN0094-ZWP1O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ1O           PIC X(4).
             10 PO-DRAN0094-PRODKEY2O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY2O    PIC 9(7).
             10 PO-DRAN0094-ZWP2O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ2O           PIC X(4).
             10 PO-DRAN0094-PRODKEY3O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY3O    PIC 9(7).
             10 PO-DRAN0094-ZWP3O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ3O           PIC X(4).
             10 PO-DRAN0094-PRODKEY4O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY4O    PIC 9(7).
             10 PO-DRAN0094-ZWP4O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ4O           PIC X(4).
             10 PO-DRAN0094-PRODKEY5O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY5O    PIC 9(7).
             10 PO-DRAN0094-ZWP5O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ5O           PIC X(4).
             10 PO-DRAN0094-PRODKEY6O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY6O    PIC 9(7).
             10 PO-DRAN0094-ZWP6O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ6O           PIC X(4).
             10 PO-DRAN0094-PRODKEY7O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY7O    PIC 9(7).
             10 PO-DRAN0094-ZWP7O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ7O           PIC X(4).
             10 PO-DRAN0094-PRODKEY8O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY8O    PIC 9(7).
             10 PO-DRAN0094-ZWP8O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ8O           PIC X(4).
             10 PO-DRAN0094-PRODKEY9O          PIC 9(4).
             10 PO-DRAN0094-SUMME-PRODKEY9O    PIC 9(7).
             10 PO-DRAN0094-ZWP9O              PIC X(1).
             10 PO-DRAN0094-ELAUFZ9O           PIC X(4).
             10 PO-DRAN0094-VDABLEINO          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0094-HANDO              PIC X(1).
             10 PO-DRAN0094-ANARTO             PIC X(1).
             10 PO-DRAN0094-ANDATO             PIC X(8).
             10 PO-DRAN0094-AFO-34C-VBNR       PIC S9(9) COMP.
             10 PO-DRAN0094-WKZO               PIC X(1).
             10 PO-DRAN0094-WKZTO              PIC X(3).
             10 PO-DRAN0094-SONSTIGE.
                15 PO-DRAN0094-PINO            PIC S9(9) COMP.
      *
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0097.
      *
      ******** ENDE COPY-MEMBER **************************************
 


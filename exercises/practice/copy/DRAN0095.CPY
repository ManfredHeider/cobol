      ****************************************************************
      *    COPY-MEMBER: DRAN0095                                     *
      *    FUNKTION   : ERM. DWS-BESTAND ÜBER GES, PIN              *
      *                 -> AUFBEREITET FÜR AN50 (BAN0022)            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1  !NEU                        !RAHNER   *
      * AAN00730!30.11.07!2.3  !ALLFINANZ                  !RAHNER   *
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
      *    FÜR DIE FUNKTION DRAN0095                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0095.
           05 PI-DRAN0095-DWS-KEY.
               10 PI-DRAN0095-PGEB-GES         PIC  X(2).
               10 PI-DRAN0095-DWS-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0095-DWS-VDABVERM    PIC  S9(9) COMP.
      *
       01 PO-DRAN0095.
          05 PO-DRAN0095-RC                      PIC  X(003).
             88 PO-DRAN0095-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0095-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0095-DATEN.
      *  OUTPUT DATA FOR MAP BAN0022
             10 PO-DRAN0095-VBNRO               PIC X(7).
             10 PO-DRAN0095-PRZVBO             PIC X(1).
             10 PO-DRAN0095-ZNAMEO             PIC X(26).
             10 PO-DRAN0095-VNAMEO             PIC X(26).
             10 PO-DRAN0095-PLZO               PIC X(5).
             10 PO-DRAN0095-ORTO               PIC X(21).
             10 PO-DRAN0095-AUSO               PIC X(3).
             10 PO-DRAN0095-PRODKEY1O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY1O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY2O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY2O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY3O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY3O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY4O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY4O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY5O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY5O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY6O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY6O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY7O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY7O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY8O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY8O    PIC 9(7).
             10 PO-DRAN0095-PRODKEY9O          PIC 9(4).
             10 PO-DRAN0095-SUMME-PRODKEY9O    PIC 9(7).
             10 PO-DRAN0095-VDABLEINO          PIC S9(6)V9(1) COMP-3.
             10 PO-DRAN0095-HANDO              PIC X(1).
             10 PO-DRAN0095-ANARTO             PIC X(1).
             10 PO-DRAN0095-ANDATO             PIC X(8).
             10 PO-DRAN0095-WKZO               PIC X(1).
             10 PO-DRAN0095-WKZTO              PIC X(3).
      *    PC-DATEN OHNE AUFBEREITUNG
           COPY DRAN0097.

      ******** ENDE COPY-MEMBER **************************************


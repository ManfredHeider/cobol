      *****************************************************************
      *    COPY-MEMBER: DRKI3957
      *    --------------------------------------------------------
      *    LÖSCHEN DTA-GESAMTBESTAND ZU G21/PROXALTO DER
      *    SCHNITTSTELLE VIRIDIUM
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3957
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10293  !16.09.22! NEU                          ! T13AC20
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRKI3957.
          05 PI-DRKI3957-DATEN.
             10 PI-DRKI3957-VNR               PIC  X(015).
      *
       01 PO-DRKI3957.
          05 PO-DRKI3957-RC                   PIC  X(003).
             88 PO-DRKI3957-RC-OK                  VALUE 'OK '.
          05 PO-DRKI3957-DATEN.
             10 PO-DRKI3957-COUNTER-561       PIC S9(9) BINARY.
             10 PO-DRKI3957-COUNTER-562       PIC S9(9) BINARY.
      *****************************************************************
      *---------------------- ENDE DRKI3957 --------------------------*
      *****************************************************************
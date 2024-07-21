      *****************************************************************
      *    COPY-MEMBER: DRPA1980
      *    --------------------------------------------------------
      *    ERMITTLUNG DES HÖCHSTEN UMSATZSTEUER-SATZES EINES PG
      *    ZUR ERMITTLUNG DER UMSATZSTEUERPFLICHT
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1980
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DSY05000   !02.11.22! NEU                          ! BUCH
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRPA1980.
          05 PI-DRPA1980-DATEN.
             10 PI-DRPA1980-PGEB-GES          PIC S9(004) COMP.
      *
       01 PO-DRPA1980.
          05 PO-DRPA1980-RC                   PIC  X(003).
             88 PO-DRPA1980-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1980-RC-NFD                 VALUE 'NFD'.
          05 PO-DRPA1980-DATEN.
             10 PO-DRPA1980-PROD-UST-SATZ     PIC S9(3)V999 COMP-3.     
      *****************************************************************
      *---------------------- ENDE DRPA1980 --------------------------*
      *****************************************************************

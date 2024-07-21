      *****************************************************************
      *    COPY-MEMBER: DRPA1981
      *    --------------------------------------------------------
      *    DTA GES. 72 INFINA FORMAL PRUEFEN UND NORMIEREN
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1981
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DAISY5194  !01.02.23! NEU                          ! BUCH   
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRPA1981.
             10 PI-DRPA1981-PZVP-VERSION      PIC S9(004) BINARY.
             10 PI-DRPA1981-PEG-DATEN-INFINA.
                COPY DRPA1982.
             10 PI-DRPA1981-KNR-MANDANT       PIC S9(004) BINARY.
      *
       01 PO-DRPA1981.
          05 PO-DRPA1981-RC                   PIC  X(003).
             88 PO-DRPA1981-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1981-RC-VSTAT-GEFUELLT      VALUE 'VSG'.
      *****************************************************************
      *---------------------- ENDE DRPA1981 --------------------------* 
      *****************************************************************

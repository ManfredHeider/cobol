      *****************************************************************
      *    COPY-MEMBER: DRVB1365
      *    --------------------------------------------------------
      *    Ermitteln vollständige E-Mail anhand von VB-Nummer
      *    --------------------------------------------------------
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1365
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * VLA-5717   !08.03.23! ERSTELLEN COPY               ! L. BETZ
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 PI-DRVB1365.
          05 PI-DRVB1365-DATEN.
             10 PI-DRVB1365-MA-ST-NR          PIC S9(009) COMP.
             10 PI-DRVB1365-GESELL-TYP        PIC S9(004) COMP.
      *
       01 PO-DRVB1365.
          05 PO-DRVB1365-RC                   PIC  X(003).
             88 PO-DRVB1365-RC-OK                  VALUE 'OK '.
             88 PO-DRVB1365-RC-NFD                 VALUE 'NFD'.
             88 PO-DRVB1365-RC-NOK                 VALUE 'NOK'.
          05 PO-DRVB1365-DATEN.
             10 PO-DRVB1365-EMAIL             PIC  X(080). 
      *****************************************************************
      *---------------------- ENDE DRVB1365 --------------------------*
      *****************************************************************

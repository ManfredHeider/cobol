      *****************************************************************
      *    COPY-MEMBER: DRPA0431                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPT ERSTELLEN      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    29.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.10.97! NEU                            ! WOLFF    *
      * afpa0012!19.05.98! rc-MFE eingefügt               ! parent   *
      * DSY04569!18.06.22! PI-DRPA0431-PVPT-SORT HINZU    ! SCHEIDEL *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0431                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
        01 PI-DRPA0431.
           05 PI-DRPA0431-DATEN.
              10 PI-DRPA0431-KNR-OBJEKT-HKF-TAB
                                       PIC S9(004) BINARY.
              10 PI-DRPA0431-NTZ-PTR-ID
                                       PIC S9(015) PACKED-DECIMAL.
              10 PI-DRPA0431-PVPT-SORT PIC  X(001).
                 88 PI-DRPA0431-PVPT-AUFSTEIGEND VALUE 'U'
                                                     , ' '.
                 88 PI-DRPA0431-PVPT-ABSTEIGEND  VALUE 'D'.
              10 PI-DRPA0431-PVPT.
                 COPY DRPA0420.
              10 PI-DRPA0431-PEST-PEBD.
                 COPY DRPA0587.
              10 PI-DRPA0431-PMBE.
                 COPY DRPA0411.
              10 PI-DRPA0431-PMPR.
                 COPY DRPA0412.
      *
        01 PO-DRPA0431.
           05 PO-DRPA0431-RC           PIC  X(003).
              88 PO-DRPA0431-RC-OK               VALUE 'OK '.
              88 PO-DRPA0431-RC-VSTAT-MIT-FEHL   VALUE 'VMF'.
              88 PO-DRPA0431-RC-VSTAT-OHNE-FEHL  VALUE 'VOF'.
              88 PO-DRPA0431-RC-MELDUNGSFEHLER   VALUE 'MFE'.
           05 PO-DRPA0431-DATEN.
              10 PO-DRPA0431-PMPT.
                 COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************

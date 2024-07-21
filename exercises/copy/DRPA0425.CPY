      *****************************************************************
      *    COPY-MEMBER: DRPA0425                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BESTANDSMELD. ERST. >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.11.97! 01 ! NEU                       ! WOLFF    *
      * APA00012!16.03.99! 02 ! RC-MELDUNGSFEHLER NEU     ! MUND     *
      * APA00066!17.11.99! 03 ! PI-RFOLGE-NR ENTFERNT     ! STORCH   *
      * DSY04569!16.05.22!    ! PI-PVPT-SORT HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0425                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
        01 PI-DRPA0425.
           05 PI-DRPA0425-DATEN.
              10 FILLER                PIC  X(001).
      *
        01 PO-DRPA0425.
           05 PO-DRPA0425-RC           PIC  X(003).
              88 PO-DRPA0425-RC-OK                   VALUE 'OK '.
              88 PO-DRPA0425-RC-VSTAT-MIT-FEHL       VALUE 'VMF'.
              88 PO-DRPA0425-RC-VSTAT-OHNE-FEHL      VALUE 'VOF'.
              88 PO-DRPA0425-RC-MELDUNGSFEHLER       VALUE 'MFE'.
      *
      ******* ENDE COPY-MEMBER **************************************

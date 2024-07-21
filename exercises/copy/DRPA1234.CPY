      *****************************************************************
      *    COPY-MEMBER: DRPA1234                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<Ermitteln GEVO anhand PGV>>              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    12.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.05.99! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      * APA00090!19.05.00! 02 ! Neue Ausprägung für Gevo  ! Jäger    *
      *         !        !    ! Gutschrift                !          *
      * APA00391!11.10.05!2.3 ! FAELLIG STATT PG-ABR-DAT  ! SCHEIDEL *
      * APA00452!30.10.06!2.4 ! Umbenennung wg. 3-stell.  ! Jäger    *
      *         !        !    ! Produkte                  !          *
      * APA01526!18.03.13!2.5 ! Neue Ausprägung für Gevo  ! BÖTTCHER *
      *         !        !    ! Leben                     !          *
      * DSY04569!12.05.22!    ! NEUER GEVO VERNEUORD      ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1234                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1234.
          05 PI-DRPA1234-DATEN.
             10 PI-DRPA1234-KNR-B-MODUS      PIC S9(004) BINARY SYNC.
             10 PI-DRPA1234-PMPE-FAELLIG-VON PIC  X(010).
             10 PI-DRPA1234-MAX-USED-PMPP    PIC S9(004) BINARY.
      *
             10 PI-DRPA1234-PMPP-TABELLE     OCCURS 50.
                15 PI-DRPA1234-PGEB-GES        PIC S9(004) BINARY.
                15 PI-DRPA1234-PROD-PROD-SCHL  PIC S9(004) BINARY.
                15 PI-DRPA1234-PGV             PIC S9(004) BINARY.
      *
       01 PO-DRPA1234.
          05 PO-DRPA1234-RC                  PIC   X(003).
             88 PO-DRPA1234-RC-OK              VALUE  'OK '.
          05 PO-DRPA1234-MAX-USED-PMPP       PIC S9(004) BINARY.
          05 PO-DRPA1234-PMPP-TABELLE        OCCURS 50.
             10 PO-DRPA1234-PGEB-GES           PIC S9(004) BINARY.
             10 PO-DRPA1234-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PO-DRPA1234-GEVO               PIC  9(003).
                88 PO-DRPA1234-GEVO-NEU             VALUE 001.
                88 PO-DRPA1234-GEVO-FOLGE           VALUE 002.
                88 PO-DRPA1234-GEVO-NEUTRAL         VALUE 003.
                88 PO-DRPA1234-GEVO-WIK             VALUE 004.
                88 PO-DRPA1234-GEVO-RUECK           VALUE 005.
                88 PO-DRPA1234-GEVO-EVS             VALUE 006.
                88 PO-DRPA1234-GEVO-GUTSCHRIFT      VALUE 008.
                88 PO-DRPA1234-GEVO-LEBEN           VALUE 009.
                88 PO-DRPA1234-GEVO-NEUORDNUNG      VALUE 010.


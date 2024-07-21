      *****************************************************************
      *    COPY-MEMBER: DRPA1862 VORBEWEGUNGSERMITTLUNG - PHYSISCH    *
      *    -----------------------------------------------------------*
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    29.08.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * APA02151!29.08.16! NEUERSTELLUNG                  ! SCHEIDEL *
      * APA02438!15.05.17! PG-RECH-NR HINZUGEFüGT         ! KRANZ    *
      * APA02436!09.08.17! PMPT-FAELLIG-VON HINZU         ! KÖPRÜCÜ  *
      * APA02504!04.09.17! T-NUMMER HINZUGEFüGT           ! KRANZ    *
      * APA02492!01.11.17! SCHICHTNR.VORBEW.HINZU         ! KRANZ    *
      * FPA01632!27.03.18! PDPT-ID UND RC-RNR HINZU       ! SCHEIDEL *
      * APA02579!24.04.18! GEL KENNUNG HINZU              ! SCHEIDEL *
      * APA02762!17.04.19! DYN-KZ HINZU                   ! SCHEIDEL *
      * APA02864!15.07.19! PG-GSMT-PROV HINZU             ! KRANZ    *
      * APA02875!07.08.19! HAFTZ-BIS HINZU                ! BUCH     *
      * APA02885!30.09.19! LISTE MIT VORPRODUKTEN         ! BUCH     *
      * APA02875!06.11.19! HAFTZ-BIS HINZU                ! BUCH     *
      * APA02944!29.07.20! Versicherte Person hinzu       ! KÖPRÜCÜ  *
      * DSY04569!08.08.22! VERTRAGSTEIL HINZU             ! SCHEIDEL *
      *         !        !                                !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1862                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1862.
          05 PI-DRPA1862-STEUERUNG.
             10 PI-DRPA1862-UFKT-PGV   PIC S9(004)       BINARY.
             10 PI-DRPA1862-UFKT-SUB   PIC S9(004)       BINARY.
          05 PI-DRPA1862-DATEN.
             10 PI-DRPA1862-KNR-MANDANT
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1862-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1862-PRINTE     PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1862-UNR        PIC S9(004)       BINARY.
             10 PI-DRPA1862-SCHICHT-NR PIC S9(004)       BINARY.
             10 PI-DRPA1862-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1862-FAELLIG-VON
                                       PIC  X(010).
             10 PI-DRPA1862-PMBE-PG-RECH-NR
                                       PIC  X(030).
             10 PI-DRPA1862-TNR-KZ     PIC  X(001).
                88 PI-DRPA1862-TNR-KZ-VORH      VALUE 'T'.
                88 PI-DRPA1862-TNR-KZ-VORH-NEIN VALUE ' '.
             10 PI-DRPA1862-T-NUMMER   PIC  X(020).
             10 PI-DRPA1862-GEL-KENNUNG
                                       PIC  X(006).
             10 PI-DRPA1862-DYN-KZ     PIC  X(001).
             10 PI-DRPA1862-PG-GSMT-PROV
                                       PIC S9(09)V9(02)  PACKED-DECIMAL.
             10 PI-DRPA1862-HAFTZ-BIS  PIC  X(10).
             10 PI-DRPA1862-VP-NR      PIC S9(004)       BINARY.
             10 PI-DRPA1862-ERST-PRODUKTE.
              15 PI-DRPA1862-ERST-PROD-SCHL
                                       PIC S9(004)       BINARY
                                       OCCURS 10  INDEXED BY IX-EPS.
             10 PI-DRPA1862-VERTRAGSTEIL
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1862-PVPT-ID    PIC S9(015)       PACKED-DECIMAL.
      
      *
       01 PO-DRPA1862.
          05 PO-DRPA1862-RC            PIC  X(003).
             88 PO-DRPA1862-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1862-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1862-RC-UNZULAESSIG               VALUE 'UNZ'.
             88 PO-DRPA1862-RC-RNR                       VALUE 'RNR'.
             88 PO-DRPA1862-RC-KENNUNG-STORNO            VALUE 'KES'.
          05 PO-DRPA1862-DATEN.
             10 PO-DRPA1862-PMPR-PGV   PIC S9(004)       BINARY.
             10 PO-DRPA1862-PDPT-PEST-ID
                                       PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1862-PDPT-DGV   PIC S9(004)       BINARY.
             10 PO-DRPA1862-PMPT-FAELLIG-VON
                                       PIC  X(010).
             10 PO-DRPA1862-SCHICHT-NR-VORB  
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1862-PDPT-PDPT-ID
                                       PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1862-VP-NR      PIC S9(004)       BINARY.

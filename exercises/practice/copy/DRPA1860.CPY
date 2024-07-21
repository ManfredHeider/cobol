      *****************************************************************
      *    COPY-MEMBER: DRPA1860 VORBEWEGUNGSERMITTLUNG - FACHLICH    *
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
      *         !        ! RC 'NFN' RAUS, 'UNZ' REIN      !          *
      * APA02504!04.09.17! T-NUMMER HINZUGEFüGT           ! KRANZ    *
      * APA02492!01.11.17! Schichtnr.Vorbew.hinzu         ! KRANZ    *
      * APA02577!29.01.18! RC "NSK" HINZU IM PO-BER.      ! KÖPRÜCÜ  *
      * FPA01632!27.03.18! TABELLE DER STEUERSTATUS       ! SCHEIDEL *
      * APA02579!24.04.18! GEL KENNUNG HINZU              ! SCHEIDEL *
      * APA02762!17.04.19! DYNAMIK-KZ HINZU               ! SCHEIDEL *
      * APA02864!15.07.19! PG-GSMT-PROV HINZU             ! KRANZ    *
      * APA02875!07.08.19! HAFZEITENDE HINZU              ! BUCH     *
      * APA02944!29.07.20! Versicherte Person hinzu       ! KÖPRÜCÜ  *
      * DSY04569!08.08.22! VERTRAGSTEIL HINZU             ! SCHEIDEL *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1860                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1860.
          05 PI-DRPA1860-DATEN.
             10 PI-DRPA1860-KNR-MANDANT
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1860-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1860-PRINTE     PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1860-UNR        PIC S9(004)       BINARY.
             10 PI-DRPA1860-HIST-UNR   PIC S9(004)       BINARY.
             10 PI-DRPA1860-SCHICHT-NR PIC S9(004)       BINARY.
             10 PI-DRPA1860-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1860-FAELLIG-VON
                                       PIC  X(010).
             10 PI-DRPA1860-URSPR-POL-DAT
                                       PIC  X(010).
             10 PI-DRPA1860-PGV        PIC S9(004)       BINARY.
             10 PI-DRPA1860-PMPE-SAM-NR
                                       PIC S9(009)       BINARY.
             10 PI-DRPA1860-KZ-PROD-NEU
                                       PIC  X(001).
             10 PI-DRPA1860-AVERM      PIC S9(009)       BINARY.
             10 PI-DRPA1860-KDBETR     PIC S9(009)       BINARY.
             10 PI-DRPA1860-PMBE-PG-RECH-NR
                                       PIC  X(030).
             10 PI-DRPA1860-TNR-KZ     PIC  X(001).
                88 PI-DRPA1860-TNR-KZ-VORH      VALUE 'T'.
                88 PI-DRPA1860-TNR-KZ-VORH-NEIN VALUE ' '.
             10 PI-DRPA1860-T-NUMMER   PIC  X(020).
             10 PI-DRPA1860-GEL-KENNUNG
                                       PIC  X(006).
             10 PI-DRPA1860-DYN-KZ     PIC  X(001).
             10 PI-DRPA1860-PG-GSMT-PROV 
                                       PIC S9(09)V9(02)  PACKED-DECIMAL.
             10 PI-DRPA1860-HAFTZ-BIS  PIC  X(010).
             10 PI-DRPA1860-VP-NR      PIC S9(004)       BINARY.
             10 PI-DRPA1860-PVPT-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1860-VERTRAGSTEIL
                                       PIC S9(004)       BINARY.
      *
       01 PO-DRPA1860.
          05 PO-DRPA1860-RC            PIC  X(003).
             88 PO-DRPA1860-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1860-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1860-RC-NSK                       VALUE 'NSK'.
             88 PO-DRPA1860-RC-UNZULAESSIG               VALUE 'UNZ'.
             88 PO-DRPA1860-RC-RNR                       VALUE 'RNR'.
          05 PO-DRPA1860-DATEN.
             10 PO-DRPA1860-PEST-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1860-DGV        PIC S9(004)       BINARY.
             10 PO-DRPA1860-PMPR-PGV   PIC S9(004)       BINARY.
             10 PO-DRPA1860-FAELLIG-VON
                                       PIC  X(010).
             10 PO-DRPA1860-SCHICHT-NR-VORB  
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1860-VP-NR      PIC S9(004)       BINARY.
          05 PO-DRPA1860-STEUSTAT.
             COPY DRPA1843.
      ************************ ENDE DRPA1860 **************************
 
 







      ****************************************************************
      *    COPY-MEMBER: DRPA1586  FUNKTION SELECT-ALL STANDARD-OZP   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01539!28.02.14! 2.2! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02220!11.03.16! 2.3! PROV-SATZ ERWEITER        ! BUCH     *
      * APA02353!07.03.17! 2.4! TELESON FELDER            ! KÖPRÜCÜ  *
      * APA02579!14.05.18! 2.6! EINFÜHRUNG ALAG           ! BUCH     *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1586                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>SELECT ALL<<<       TABELLE PMPRH TBPA0315             *
      *                                                              *
      ****************************************************************
      *
        01 PI-DRPA1586.
           05 PI-DRPA1586-PMPR-KEY.
              10 PI-DRPA1586-PGEB-GES  PIC S9(004)       BINARY.
              10 PI-DRPA1586-PMPR-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PI-DRPA1586-PMPR-VERSION
                                       PIC S9(004)       BINARY.
      *
        01 PO-DRPA1586.
           05 PO-DRPA1586-RC           PIC  X(003).
              88 PO-DRPA1586-RC-OK          VALUE 'OK '.
              88 PO-DRPA1586-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1586-PMPR-DATEN.
              10 PO-DRPA1586-PMPR-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PMPR-V-GUELTIG-AB
                                       PIC  X(026).
              10 PO-DRPA1586-PMPR-V-GUELTIG-BIS
                                       PIC  X(010).
              10 PO-DRPA1586-PMPR-V-KENNUNG
                                       PIC  X(008).
              10 PO-DRPA1586-PMBE-ID   PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1586-PMBE-VERSION
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PRINTE    PIC S9(015)       PACKED-DECIMAL.
              10 PO-DRPA1586-PGEB-GES  PIC S9(004)       BINARY.
              10 PO-DRPA1586-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PMPR-PGV  PIC S9(004)       BINARY.
              10 PO-DRPA1586-PMPR-PPFL-BEITRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PPFL-SUMME
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PG-BEITRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PG-SUMME
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PG-PROV-STZ
                                       PIC S9(004)V9(04) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PG-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PO-DRPA1586-PRKO-KOND-NR-GSP
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PRKO-KOND-NR-AP-FP
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PMPR-PG-DATEN
                                       PIC  X(030).
              10 PO-DRPA1586-PMPR-VSTAT
                                       PIC  X(004).
              10 PO-DRPA1586-PMPR-VSTAT-UNR
                                       PIC S9(004)       BINARY.
              10 PO-DRPA1586-PMPR-LAST-UPDATE
                                       PIC  X(026).
              10 PO-DRPA1586-PMPR-KENNUNG
                                       PIC  X(008).
              10 PO-DRPA1586-PMPR-PG-PRODART      PIC S9(4) BINARY.
              10 PO-DRPA1586-KNR-DYN-KZ-1         PIC S9(4) BINARY.
              10 PO-DRPA1586-KNR-DYN-KZ-2         PIC S9(4) BINARY.
              10 PO-DRPA1586-KNR-BEW-HRKFT        PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-BEW-SCHL        PIC S9(9) BINARY.
              10 PO-DRPA1586-PMPR-PRO-RATA-Z      PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-PRO-RATA-N      PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-BWERT-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-RWERT-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-BZ-DAUER        PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-BEITRAG-JAHR
                                          PIC S9(7)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-NEUST-KZ        PIC X(1).
              10 PO-DRPA1586-PMPR-TARIF-SCHLGRP   PIC X(15).
              10 PO-DRPA1586-KNR-TARIF-KZ         PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-TARIF-GRP       PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-DAUER-GSMT      PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-HAFTZ-VON       PIC X(10).
              10 PO-DRPA1586-PMPR-HAFTZ-BIS       PIC X(10).
              10 PO-DRPA1586-PMPR-PG-TARIF        PIC X(15).
              10 PO-DRPA1586-PMPR-RWERT-BEITRAG
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PROV-BEITRAG
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-PROV-VERDIENT
                                          PIC S9(4) BINARY.
              10 PO-DRPA1586-PMPR-VERS-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-AE-KZ           PIC X(1).
              10 PO-DRPA1586-PMPR-TLSN-TARIF    PIC X(50).
              10 PO-DRPA1586-PMPR-TLSN-LAUFZEIT PIC S9(4)  USAGE COMP.
              10 PO-DRPA1586-PMPR-TLSN-VRBRAUCH PIC S9(09) USAGE COMP.
              10 PO-DRPA1586-KNR-TLSN-STORNO    PIC S9(4)  USAGE COMP.
              10 PO-DRPA1586-PMPR-TLSN-Z-NR     PIC X(20).
              10 PO-DRPA1586-PMPR-GEL-GEVO      PIC X(07).
              10 PO-DRPA1586-PMPR-GEL-RABST     PIC X(05).
              10 PO-DRPA1586-PMPR-GEL-EALTER    PIC S9(04) BINARY.
              10 PO-DRPA1586-PMPR-NBB-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-AUW-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PO-DRPA1586-PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.
 
      * ******************* ENDE COPY-MEMBER *************************


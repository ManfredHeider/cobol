      ****************************************************************
      *    COPY-MEMBER: DRPA1584  FUNKTION INSERT STANDARD-OZP       *
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
      * APA02220!11.03.16! 2.3! PROV-SATZ ERWEITERT       ! BUCH     *
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
      *    FÜR DIE FUNKTION DRPA1584                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMPRH TBPA0315             *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1584.
           05 PI-DRPA1584-UFKT         PIC  9(002).
           05 PI-DRPA1584-PMPR-DATEN.
              10 PI-DRPA1584-PMPR-ID         PIC S9(015) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-VERSION       PIC S9(004)  BINARY.
              10 PI-DRPA1584-PMPR-V-GUELTIG-AB  PIC  X(026).
              10 PI-DRPA1584-PMPR-V-GUELTIG-BIS PIC  X(010).
              10 PI-DRPA1584-PMPR-V-KENNUNG     PIC  X(008).
              10 PI-DRPA1584-PMBE-ID         PIC S9(015) PACKED-DECIMAL.
              10 PI-DRPA1584-PMBE-VERSION          PIC S9(004) BINARY.
              10 PI-DRPA1584-PRINTE    PIC S9(015) PACKED-DECIMAL.
              10 PI-DRPA1584-PGEB-GES  PIC S9(004) BINARY.
              10 PI-DRPA1584-PROD-PROD-SCHL PIC S9(004) BINARY.
              10 PI-DRPA1584-PMPR-PGV       PIC S9(004) BINARY.
              10 PI-DRPA1584-PMPR-PPFL-BEITRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PPFL-SUMME
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PG-BEITRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PG-SUMME
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PG-PROV-STZ
                                       PIC S9(004)V9(04) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PG-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
              10 PI-DRPA1584-PRKO-KOND-NR-GSP PIC S9(004) BINARY.
              10 PI-DRPA1584-PRKO-KOND-NR-AP-FP PIC S9(004) BINARY.
              10 PI-DRPA1584-PMPR-PG-DATEN      PIC  X(030).
              10 PI-DRPA1584-PMPR-VSTAT         PIC  X(004).
              10 PI-DRPA1584-PMPR-VSTAT-UNR     PIC S9(004) BINARY.
              10 PI-DRPA1584-PMPR-LAST-UPDATE   PIC  X(026).
              10 PI-DRPA1584-PMPR-KENNUNG       PIC  X(008).
              10 PI-DRPA1584-PMPR-PG-PRODART    PIC S9(4) BINARY.
              10 PI-DRPA1584-KNR-DYN-KZ-1       PIC S9(4) BINARY.
              10 PI-DRPA1584-KNR-DYN-KZ-2       PIC S9(4) BINARY.
              10 PI-DRPA1584-KNR-BEW-HRKFT      PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-BEW-SCHL      PIC S9(9) BINARY.
              10 PI-DRPA1584-PMPR-PRO-RATA-Z    PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-PRO-RATA-N    PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-BWERT-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-RWERT-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-BZ-DAUER      PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-BEITRAG-JAHR
                                          PIC S9(7)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-NEUST-KZ      PIC X(1).
              10 PI-DRPA1584-PMPR-TARIF-SCHLGRP PIC X(15).
              10 PI-DRPA1584-KNR-TARIF-KZ       PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-TARIF-GRP     PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-DAUER-GSMT    PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-HAFTZ-VON     PIC X(10).
              10 PI-DRPA1584-PMPR-HAFTZ-BIS     PIC X(10).
              10 PI-DRPA1584-PMPR-PG-TARIF      PIC X(15).
              10 PI-DRPA1584-PMPR-RWERT-BEITRAG
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PROV-BEITRAG
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-PROV-VERDIENT PIC S9(4) BINARY.
              10 PI-DRPA1584-PMPR-VERS-SUMME
                                          PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-AE-KZ         PIC X(1).
              10 PI-DRPA1584-PMPR-TLSN-TARIF    PIC X(50).
              10 PI-DRPA1584-PMPR-TLSN-LAUFZEIT PIC S9(4)  USAGE COMP.
              10 PI-DRPA1584-PMPR-TLSN-VRBRAUCH PIC S9(09) USAGE COMP.
              10 PI-DRPA1584-KNR-TLSN-STORNO    PIC S9(4)  USAGE COMP.
              10 PI-DRPA1584-PMPR-TLSN-Z-NR     PIC X(20).
              10 PI-DRPA1584-PMPR-GEL-GEVO      PIC X(07).
              10 PI-DRPA1584-PMPR-GEL-RABST     PIC X(05).
              10 PI-DRPA1584-PMPR-GEL-EALTER    PIC S9(04) BINARY.
              10 PI-DRPA1584-PMPR-NBB-SUMME
                                         PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-AUW-SUMME
                                         PIC S9(9)V9(2) PACKED-DECIMAL.
              10 PI-DRPA1584-PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.
      *
        01 PO-DRPA1584.
           05 PO-DRPA1584-RC           PIC  X(003).
              88 PO-DRPA1584-RC-OK          VALUE 'OK '.
              88 PO-DRPA1584-RC-DUPLICATE-VALUE
                                            VALUE 'DVA'.
      *---------------------------------------------------------------*
 
 


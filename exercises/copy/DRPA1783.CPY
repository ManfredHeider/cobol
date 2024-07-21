      ****************************************************************
      *    COPY-MEMBER: DRPA1783  FUNKTION ALLE PROD ZUR PMBE-ID     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STRAUSS                                         *
      *    DATUM:    13.12.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01702!13.12.13! 2.0! NEUERSTELLUNG             ! STRAUSS  *
      * APA02220!10.03.16! 2.4! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02353!23.02.17! 2.5! Einführung Teleson        ! Buch     *
      * APA02579!14.05.18! 2.6! EINFÜHRUNG ALAG           ! BUCH     *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1783.
          05 PI-DRPA1783-PROGRAMM         PIC  X(008).
          05 PI-DRPA1783-UFKT             PIC  9(002).
          05 PI-DRPA1783-DATEN.
             10 PI-DRPA1783-PGEB-GES      PIC S9(004)  BINARY.
             10 PI-DRPA1783-PRINTE        PIC S9(015)  PACKED-DECIMAL.
             10 PI-DRPA1783-PMBE-ID       PIC S9(015)  PACKED-DECIMAL.
      *
       01 PO-DRPA1783.
          05 PO-DRPA1783-RC               PIC  X(003).
             88 PO-DRPA1783-RC-OK              VALUE 'OK '.
             88 PO-DRPA1783-RC-LEER            VALUE 'LER'.
             88 PO-DRPA1783-RC-EOF             VALUE 'EOF'.
      *
          05 PO-DRPA1783-DATEN.
               30 PMPRH-DATEN.
                35 PMPR-ID             PIC S9(015) PACKED-DECIMAL.
                35 PMPR-VERSION        PIC S9(004) BINARY.
                35 PMPR-V-GUELTIG-AB   PIC  X(026).
                35 PMPR-V-GUELTIG-BIS  PIC  X(010).
                35 PMPR-V-KENNUNG      PIC  X(008).
                35 PMBE-ID             PIC S9(015) PACKED-DECIMAL.
                35 PMBE-VERSION        PIC S9(004) BINARY.
                35 PRINTE              PIC S9(015) PACKED-DECIMAL.
                35 PGEB-GES            PIC S9(004) BINARY.
                35 PROD-PROD-SCHL      PIC S9(004) BINARY.
                35 PMPR-PGV            PIC S9(004) BINARY.
                35 PMPR-PPFL-BEITRAG   PIC S9(009)V9(2) PACKED-DECIMAL.
                35 PMPR-PPFL-SUMME     PIC S9(009)V9(2) PACKED-DECIMAL.
                35 PMPR-PG-BEITRAG     PIC S9(009)V9(2) PACKED-DECIMAL.
                35 PMPR-PG-SUMME       PIC S9(009)V9(2) PACKED-DECIMAL.
                35 PMPR-PG-PROV-STZ    PIC S9(004)V9(4) PACKED-DECIMAL.
                35 PMPR-PG-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
                35 PRKO-KOND-NR-GSP    PIC S9(004) BINARY.
                35 PRKO-KOND-NR-AFP    PIC S9(004) BINARY.
                35 PMPR-PG-DATEN       PIC  X(030).
                35 PMPR-VSTAT          PIC  X(004).
                35 PMPR-VSTAT-UNR      PIC S9(004) BINARY.
                35 PMPR-LAST-UPDATE    PIC  X(026).
                35 PMPR-KENNUNG        PIC  X(008).
                35 PMPR-PG-PRODART     PIC S9(4) BINARY.
                35 KNR-DYN-KZ-1        PIC S9(4) BINARY.
                35 KNR-DYN-KZ-2        PIC S9(4) BINARY.
                35 KNR-BEW-HRKFT       PIC S9(4) BINARY.
                35 PMPR-BEW-SCHL       PIC S9(9) BINARY.
                35 PMPR-PRO-RATA-Z     PIC S9(4) BINARY.
                35 PMPR-PRO-RATA-N     PIC S9(4) BINARY.
                35 PMPR-BWERT-SUMME    PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-RWERT-SUMME    PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-BZ-DAUER       PIC S9(4) BINARY.
                35 PMPR-BEITRAG-JAHR   PIC S9(7)V9(2) PACKED-DECIMAL.
                35 PMPR-NEUST-KZ       PIC X(1).
                35 PMPR-TARIF-SCHLGRP  PIC X(15).
                35 KNR-TARIF-KZ        PIC S9(4) BINARY.
                35 PMPR-TARIF-GRP      PIC S9(4) BINARY.
                35 PMPR-DAUER-GSMT     PIC S9(4) BINARY.
                35 PMPR-HAFTZ-VON      PIC X(10).
                35 PMPR-HAFTZ-BIS      PIC X(10).
                35 PMPR-PG-TARIF       PIC X(15).
                35 PMPR-RWERT-BEITRAG  PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-PROV-BEITRAG   PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-PROV-VERDIENT  PIC S9(4) BINARY.
                35 PMPR-VERS-SUMME     PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-AE-KZ          PIC X(1).
                35 PMPR-TLSN-TARIF     PIC X(50).
                35 PMPR-TLSN-LAUFZEIT  PIC S9(4)    USAGE COMP.
                35 PMPR-TLSN-VRBRAUCH  PIC S9(09)   USAGE COMP.
                35 KNR-TLSN-STORNO     PIC S9(4)    USAGE COMP.
                35 PMPR-TLSN-Z-NR      PIC X(20).
                35 PMPR-GEL-GEVO       PIC X(07).
                35 PMPR-GEL-RABST      PIC X(05).
                35 PMPR-GEL-EALTER     PIC S9(4) BINARY.
                35 PMPR-NBB-SUMME      PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-AUW-SUMME      PIC S9(9)V9(2) PACKED-DECIMAL.
                35 PMPR-VERTRAGSTEIL   PIC S9(004) BINARY.
      *
      * **************** ENDE COPY-MEMBER ***************************
 
 


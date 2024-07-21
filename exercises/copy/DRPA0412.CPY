      *****************************************************************
      *    COPY-MEMBER: DRPA0412                                      *
      *    --------------------------------------------------------   *
      *    PMPR-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      * APA01526!09.04.13! 2.1! ERGÄNZUNGEN FÜR KALYPSO   ! BÖTTCHER *
      * APA02220!10.03.16! 2.2! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPR-PG-PROV-STZ         !          *
      * APA02353!21.02.17! 2.3! TELESON                   ! BUCH     *
      * APA02579!24.04.18! 2.4! GENERALI LEBEN            ! SCHEIDEL *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PMPR.                   *
      *                                                              *
      ****************************************************************
      *
             30 PMPR-DATEN.
                35 PMPR-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PMPR-VERSION        PIC S9(004)       BINARY.
                35 PMBE-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PMBE-VERSION        PIC S9(004)       BINARY.
                35 PRINTE              PIC S9(015)       PACKED-DECIMAL.
                35 PGEB-GES            PIC S9(004)       BINARY.
                35 PROD-PROD-SCHL      PIC S9(004)       BINARY.
                35 PMPR-PGV            PIC S9(004)       BINARY.
                35 PMPR-PPFL-BEITRAG   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PPFL-SUMME     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PG-BEITRAG     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PG-SUMME       PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PG-PROV-STZ    PIC S9(004)V9(04) PACKED-DECIMAL.
                35 PMPR-PG-GSMT-PROV   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PRKO-KOND-NR-GSP    PIC S9(004)       BINARY.
                35 PRKO-KOND-NR-AFP    PIC S9(004)       BINARY.
                35 PMPR-PG-PROD-DATEN  PIC  X(030).
                35 PMPR-VSTAT          PIC  X(004).
                35 PMPR-VSTAT-UNR      PIC S9(004)       BINARY.
      
                35 PMPR-PG-PRODART     PIC S9(004)       BINARY.
                35 KNR-DYN-KZ-1        PIC S9(004)       BINARY.
                35 KNR-DYN-KZ-2        PIC S9(004)       BINARY.
                35 KNR-BEW-HRKFT       PIC S9(004)       BINARY.
                35 PMPR-BEW-SCHL       PIC S9(009)       BINARY.
                35 PMPR-PRO-RATA-Z     PIC S9(004)       BINARY.
                35 PMPR-PRO-RATA-N     PIC S9(004)       BINARY.
                35 PMPR-BWERT-SUMME    PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-RWERT-SUMME    PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-BZ-DAUER       PIC S9(004)       BINARY.
                35 PMPR-BEITRAG-JAHR   PIC S9(007)V9(02) PACKED-DECIMAL.
                35 PMPR-NEUST-KZ       PIC  X(001).
                35 PMPR-TARIF-SCHLGRP  PIC  X(015).
                35 KNR-TARIF-KZ        PIC S9(004)       BINARY.
                35 PMPR-TARIF-GRP      PIC S9(004)       BINARY.
                35 PMPR-DAUER-GSMT     PIC S9(004)       BINARY.
                35 PMPR-HAFTZ-VON      PIC  X(010).
                35 PMPR-HAFTZ-BIS      PIC  X(010).
                35 PMPR-PG-TARIF       PIC  X(015).
                35 PMPR-RWERT-BEITRAG  PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PROV-BEITRAG   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-PROV-VERDIENT  PIC S9(004)       BINARY.
                35 PMPR-VERS-SUMME     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-AE-KZ          PIC  X(001).
                35 PMPR-TLSN-TARIF     PIC  X(050).
                35 PMPR-TLSN-LAUFZEIT  PIC S9(004)       BINARY.
                35 PMPR-TLSN-VRBRAUCH  PIC S9(009)       BINARY.
                35 KNR-TLSN-STORNO     PIC S9(004)       BINARY.
                35 PMPR-TLSN-Z-NR      PIC  X(020).
                35 PMPR-GEL-GEVO       PIC  X(007).
                35 PMPR-GEL-RABST      PIC  X(005).
                35 PMPR-GEL-EALTER     PIC S9(004)       BINARY.
                35 PMPR-NBB-SUMME      PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-AUW-SUMME      PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPR-VERTRAGSTEIL   PIC S9(004)       BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


      *****************************************************************
      *    COPY-MEMBER: DRPA1461                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPPH >>                         *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!12.02.01! 2.0! NEU                       ! MUND     *
      * APA01526!21.03.13! 2.5! TABELLENERWEITERUNG       ! STORCH   *
      * APA01932!31.03.15! 2.6! TABELLENERWEITERUNG LUCY  ! KÖPRÜCÜ  *
      * APA02220!10.03.16! 2.7! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!31.01.17! 2.8! NEUE FELDER FlexN (AMV)   ! KÖPRÜCÜ  *
      * APA02353!03.03.17!2.11! NEUE FELDER Teleson       ! Buch     *
      * APA02579!14.05.18!2.12! NEUE FELDER ALAG          ! BUCH     *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1461                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1461.
          05 PI-DRPA1461-PROGRAMM            PIC  X(008).
          05 PI-DRPA1461-UFKT                PIC  9(002).
          05 PI-DRPA1461-SCHALTER            PIC  9.
          05 PI-DRPA1461-DATEN.
             10 PI-DRPA1461-PMPG-ID          PIC  S9(15) COMP-3.
             10 PI-DRPA1461-PMPG-VERSION     PIC  S9(4)  COMP.
      *
       01 PO-DRPA1461.
          05 PO-DRPA1461-RC                    PIC X(003).
             88 PO-DRPA1461-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1461-RC-EOF                 VALUE 'EOF'.
             88 PO-DRPA1461-RC-LEER                VALUE 'LER'.
          05 PO-DRPA1461-DATEN.
             10 PO-DRPA1461-PMPP-ID            PIC S9(15) COMP-3.
             10 PO-DRPA1461-PMPG-ID            PIC S9(15) COMP-3.
             10 PO-DRPA1461-PGEB-GES           PIC S9(04) COMP.
             10 PO-DRPA1461-PROD-PROD-SCHL     PIC S9(04) BINARY.
             10 PO-DRPA1461-PGV                PIC S9(04) COMP.
             10 PO-DRPA1461-PMPP-PPFL-BEITRAG  PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PPFL-SUMME    PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PG-BEITRAG    PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PG-SUMME      PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PG-PROV-STZ   PIC S9(04)V9999 COMP-3.
             10 PO-DRPA1461-PMPP-PG-GSMT-PROV  PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA1461-PKON-KOND-NR       PIC S9(04) COMP.
             10 PO-DRPA1461-PMPP-PG-PROD-DATEN PIC X(30).
             10 PO-DRPA1461-PMPP-LAST-UPDATE   PIC X(26).
             10 PO-DRPA1461-PMPP-KENNUNG       PIC X(08).
             10 PO-DRPA1461-PMPG-VERSION       PIC S9(04)  COMP.
             10 PO-DRPA1461-PMPPH-LAST-UPDATE  PIC X(26).
             10 PO-DRPA1461-PMPPH-KENNUNG      PIC X(08).
             10 PO-DRPA1461-PMPP-PG-PRODART    PIC S9(4) COMP.
             10 PO-DRPA1461-KNR-DYN-KZ-1       PIC S9(4) COMP.
             10 PO-DRPA1461-KNR-DYN-KZ-2       PIC S9(4) COMP.
             10 PO-DRPA1461-KNR-BEW-HRKFT      PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-BEW-SCHL      PIC S9(9) COMP.
             10 PO-DRPA1461-PMPP-PRO-RATA-Z    PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-PRO-RATA-N    PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-BWERT-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-RWERT-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-BZ-DAUER      PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-BEITRAG-JAHR  PIC S9(7)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-NEUST-KZ      PIC X(1).
             10 PO-DRPA1461-PMPP-TARIF-SCHLGRP PIC X(15).
             10 PO-DRPA1461-KNR-TARIF-KZ       PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-TARIF-GRP     PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-DAUER-GSMT    PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-HAFTZ-VON     PIC X(10).
             10 PO-DRPA1461-PMPP-HAFTZ-BIS     PIC X(10).
             10 PO-DRPA1461-PMPP-PG-TARIF      PIC X(15).
             10 PO-DRPA1461-PMPP-RWERT-BEITRAG PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PROV-BEITRAG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PROV-VERDIENT PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-VERS-SUMME    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-BUZTAR        PIC S9(4) COMP.
             10 PO-DRPA1461-PMPP-AE-KZ         PIC X(01).
             10 PO-DRPA1461-PMPP-PG-UST-BETRAG PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-PG-UST-SATZ   PIC S9(3)V9(3) COMP-3.
             10 PO-DRPA1461-PMPP-VSP-KOMBI-NR  PIC S9(05) USAGE COMP.
             10 PO-DRPA1461-PMPP-VSP-KOMBI-BZ  PIC X(03).
             10 PO-DRPA1461-PMPP-VSP-FN-BER    PIC X(01).
             10 PO-DRPA1461-PMPP-VSP-FN-REL    PIC X(01).
             10 PO-DRPA1461-PMPP-TLSN-TARIF    PIC X(050).
             10 PO-DRPA1461-PMPP-TLSN-LAUFZEIT PIC S9(004) COMP.
             10 PO-DRPA1461-PMPP-TLSN-VRBRAUCH PIC S9(009) COMP.
             10 PO-DRPA1461-KNR-TLSN-STORNO    PIC S9(004) COMP.
             10 PO-DRPA1461-PMPP-TLSN-Z-NR     PIC X(020).
             10 PO-DRPA1461-PMPP-GEL-GEVO      PIC X(007).
             10 PO-DRPA1461-PMPP-GEL-TARBEZ    PIC X(020).
             10 PO-DRPA1461-PMPP-GEL-RABST     PIC X(005).
             10 PO-DRPA1461-PMPP-GEL-EALTER    PIC S9(004) COMP.
             10 PO-DRPA1461-PMPP-NBB-SUMME     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-AUW-SUMME     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1461-PMPP-VERTRAGSTEIL  PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 


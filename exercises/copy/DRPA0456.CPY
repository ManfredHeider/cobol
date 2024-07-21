      *****************************************************************
      *    COPY-MEMBER: DRPA0456                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPP >>                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.08.97! 1.0! NEU                       ! D.WILHELM*
      * AFPA0012!01.09.97! 1.1! ÄNDERUNG NAMEN PROV-GESAMT! D.WILHELM*
      *         !        !    ! UND PROV-SATZ             !          *
      * AFPA0012!13.10.97! 1.2! PG-DATEN -> PG-PROV-DATEN ! D.WILHELM*
      *         !        !    ! NAMENSÄNDERUNG            !          *
      * AFPA0012!07.01.98! 1.3! ERWEITERUNG ALLE FELDER   ! D.WILHELM*
      * APA00452!01.11.06! 2.1! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA01526!20.03.13! 2.2! ERWEITERUNG UM NEUE DATEN-! STORCH   *
      *         !        !    ! FELDER (PO-BEREICH)       !          *
      * APA01932!30.03.15! 2.3! ERWEITERUNG PO-BEREICH    ! KÖPRÜCÜ  *
      * APA02220!10.03.16! 2.4! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!30.01.17! 2.5! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02353!03.03.17! 2.7! ERWEITERT UM Teleson      ! Buch     *
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
      *    FÜR DIE FUNKTION DRPA0456                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0456.
          05 PI-DRPA0456-PROGRAMM            PIC  X(008).
          05 PI-DRPA0456-UFKT                PIC  9(002).
          05 PI-DRPA0456-SCHALTER            PIC  9.
          05 PI-DRPA0456-DATEN.
             10 PI-DRPA0456-PMPG-ID          PIC  S9(15) COMP-3.
      *
       01 PO-DRPA0456.
          05 PO-DRPA0456-RC                  PIC  X(003).
             88 PO-DRPA0456-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0456-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA0456-RC-LEER               VALUE 'LER'.
          05 PO-DRPA0456-DATEN.
             10 PO-DRPA0456-PMPP-ID            PIC S9(15) COMP-3.
             10 PO-DRPA0456-PMPG-ID            PIC S9(15) COMP-3.
             10 PO-DRPA0456-PGEB-GES           PIC S9(04) COMP.
             10 PO-DRPA0456-PROD-PROD-SCHL     PIC S9(04) BINARY.
             10 PO-DRPA0456-PGV                PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-PPFL-BEITRAG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PPFL-SUMME    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PG-BEITRAG    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PG-SUMME      PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PG-PROV-STZ   PIC S9(04)V9999 COMP-3.
             10 PO-DRPA0456-PMPP-PG-GSMT-PROV  PIC S9(09)V9(2) COMP-3.
             10 PO-DRPA0456-PKON-KOND-NR       PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-PG-PROD-DATEN PIC X(30).
             10 PO-DRPA0456-PMPP-LAST-UPDATE   PIC X(26).
             10 PO-DRPA0456-PMPP-KENNUNG       PIC X(8).
             10 PO-DRPA0456-PMPP-PG-PRODART    PIC S9(04) COMP.
             10 PO-DRPA0456-KNR-DYN-KZ-1       PIC S9(04) COMP.
             10 PO-DRPA0456-KNR-DYN-KZ-2       PIC S9(04) COMP.
             10 PO-DRPA0456-KNR-BEW-HRKFT      PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-BEW-SCHL      PIC S9(09) COMP.
             10 PO-DRPA0456-PMPP-PRO-RATA-Z    PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-PRO-RATA-N    PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-BWERT-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-RWERT-SUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-BZ-DAUER      PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-BEITRAG-JAHR  PIC S9(7)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-NEUST-KZ      PIC X(1).
             10 PO-DRPA0456-PMPP-TARIF-SCHLGRP PIC X(15).
             10 PO-DRPA0456-KNR-TARIF-KZ       PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-TARIF-GRP     PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-DAUER-GSMT    PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-HAFTZ-VON     PIC X(10).
             10 PO-DRPA0456-PMPP-HAFTZ-BIS     PIC X(10).
             10 PO-DRPA0456-PMPP-PG-TARIF      PIC X(15).
             10 PO-DRPA0456-PMPP-RWERT-BEITRAG PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PROV-BEITRAG  PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PROV-VERDIENT PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-VERS-SUMME    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-BUZTAR        PIC S9(04) COMP.
             10 PO-DRPA0456-PMPP-AE-KZ         PIC X(01).
             10 PO-DRPA0456-PMPP-PG-UST-BETRAG PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-PG-UST-SATZ   PIC S9(03)V999 COMP-3.
             10 PO-DRPA0456-PMPP-VSP-KOMBI-NR  PIC S9(005) COMP.
             10 PO-DRPA0456-PMPP-VSP-KOMBI-BZ  PIC X(003).
             10 PO-DRPA0456-PMPP-VSP-FN-BER    PIC X(001).
             10 PO-DRPA0456-PMPP-VSP-FN-REL    PIC X(001).
             10 PO-DRPA0456-PMPP-TLSN-TARIF    PIC X(050).
             10 PO-DRPA0456-PMPP-TLSN-LAUFZEIT PIC S9(004) COMP.
             10 PO-DRPA0456-PMPP-TLSN-VRBRAUCH PIC S9(009) COMP.
             10 PO-DRPA0456-KNR-TLSN-STORNO    PIC S9(004) COMP.
             10 PO-DRPA0456-PMPP-TLSN-Z-NR     PIC X(020).
             10 PO-DRPA0456-PMPP-GEL-GEVO      PIC X(7).
             10 PO-DRPA0456-PMPP-GEL-TARBEZ    PIC X(20).
             10 PO-DRPA0456-PMPP-GEL-RABST     PIC X(5).
             10 PO-DRPA0456-PMPP-GEL-EALTER    PIC S9(4) USAGE COMP.
             10 PO-DRPA0456-PMPP-NBB-SUMME     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-AUW-SUMME     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA0456-PMPP-VERTRAGSTEIL  PIC S9(4) BINARY.    
      *


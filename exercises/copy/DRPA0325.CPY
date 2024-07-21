      ****************************************************************
      *    COPY-MEMBER: DRPA0325  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      *         !31.07.97!1.1 ! PMPP-PG-BEITRAG EINGEFüGT ! OPRITZ   *
      *         !31.07.97!1.1 ! PMPP-PG-SUMME   EINGEFüGT ! OPRITZ   *
      *         !31.07.97!1.1 ! PKON-KOND-NR-GSP IN       !          *
      *         !        !    ! PKON-KOND-NR UMGENANT     ! OPRITZ   *
      * AFPA0022!03.09.97!1.3 !-PROV-SATZ ->-PG-PROV-STZ  ! OPRITZ   *
      *         !        !    !-PROV-GESAMT->-PG-GSMT-PROV!          *
      *         !        !    !-KOND-NR-GSP WIEDER DA     !          *
      * AFPA00??!07.10.97!    ! PKON_KOND_NR_GSP UMBENANNT! STORCH   *
      * AFPA00??!09.10.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENäNDERUNGEN        !          *
      * APA00452!06.11.06!2.1 ! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * APA01539!26.07.13!2.2 ! ERWEITERUNGEN F. KALYPSO  ! RAHNER   *
      * APA01932!30.03.15!2.3 ! ERWEITERUNGEN F. LUCY     ! KÖPRÜCÜ  *
      * APA02220!10.03.16!2.5 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!30.01.17!2.6 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02353!03.03.17!2.8 ! ERWEITERT UM Teleson      ! Buch     *
      * APA02579!26.04.18!2.9 ! FriWi neue DB2-Attribute  ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0325                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBPA0103 PMPP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0325.
          05 PI-DRPA0325-PMPP-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0325-PMPG-KEY.
             10 PI-DRPA0325-PMPP-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0325-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRAP0325-PGEB-GES          PIC S9(004) COMP-3.
             10 PI-DRPA0325-PROD-PROD-SCHL    PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-PGV             PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-PPFL-BEITRAG    PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PPFL-SUMME      PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PG-BEITRAG      PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PG-SUMME        PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PG-PROV-STZ     PIC S9(004)V9(4) COMP-3.
          05 PI-DRPA0325-PMPP-PG-GSMT-PROV    PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PRKO-KOND-NR-GSP     PIC S9(004)      COMP.
          05 PI-DRPA0325-PMPP-PG-PROD-DATEN   PIC X(030).
          05 PI-DRPA0325-PMPP-KENNUNG         PIC X(008).
          05 PI-DRPA0325-PMPP-PG-PRODART      PIC S9(004) COMP.
          05 PI-DRPA0325-KNR-DYN-KZ-1         PIC S9(004) COMP.
          05 PI-DRPA0325-KNR-DYN-KZ-2         PIC S9(004) COMP.
          05 PI-DRPA0325-KNR-BEW-HRKFT        PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-BEW-SCHL        PIC S9(009) COMP.
          05 PI-DRPA0325-PMPP-PRO-RATA-Z      PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-PRO-RATA-N      PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-BWERT-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-RWERT-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-BZ-DAUER        PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-BEITRAG-JAHR    PIC S9(007)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-NEUST-KZ        PIC X(001).
          05 PI-DRPA0325-PMPP-TARIF-SCHLGRP   PIC X(015).
          05 PI-DRPA0325-KNR-TARIF-KZ         PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-TARIF-GRP       PIC S9(4)   COMP.
          05 PI-DRPA0325-PMPP-DAUER-GSMT      PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-HAFTZ-VON       PIC X(010).
          05 PI-DRPA0325-PMPP-HAFTZ-BIS       PIC X(010).
          05 PI-DRPA0325-PMPP-PG-TARIF        PIC X(015).
          05 PI-DRPA0325-PMPP-RWERT-BEITRAG   PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PROV-BEITRAG    PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PROV-VERDIENT   PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-VERS-SUMME      PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-BUZTAR          PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-AE-KZ           PIC X(1).
          05 PI-DRPA0325-PMPP-PG-UST-BETRAG   PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-PG-UST-SATZ     PIC S9(003)V9(3) COMP-3.
          05 PI-DRPA0325-PMPP-VSP-KOMBI-NR    PIC S9(005) COMP.
          05 PI-DRPA0325-PMPP-VSP-KOMBI-BZ    PIC X(003).
          05 PI-DRPA0325-PMPP-VSP-FN-BER      PIC X(001).
          05 PI-DRPA0325-PMPP-VSP-FN-REL      PIC X(001).
          05 PI-DRPA0325-PMPP-TLSN-TARIF      PIC X(050).
          05 PI-DRPA0325-PMPP-TLSN-LAUFZEIT   PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-TLSN-VRBRAUCH   PIC S9(009) COMP.
          05 PI-DRPA0325-KNR-TLSN-STORNO      PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-TLSN-Z-NR       PIC X(020).
          05 PI-DRPA0325-PMPP-GEL-GEVO        PIC X(007).
          05 PI-DRPA0325-PMPP-GEL-TARBEZ      PIC X(020).
          05 PI-DRPA0325-PMPP-GEL-RABST       PIC X(005).
          05 PI-DRPA0325-PMPP-GEL-EALTER      PIC S9(004) COMP.
          05 PI-DRPA0325-PMPP-NBB-SUMME       PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-AUW-SUMME       PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA0325-PMPP-VERTRAGSTEIL    PIC S9(004) BINARY.
      *
       01 PO-DRPA0325.
          05 PO-DRPA0325-RC                   PIC  X(003).
             88 PO-DRPA0325-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0325-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0325-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 



      ****************************************************************
      *    COPY-MEMBER: DRPA0324  FUNKTION INSERT STANDARD-OZP       *
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
      *         !31.08.97!1.1 ! PMPP_PG_BEITRAG EINGEFÜGT ! OPRITZ   *
      *         !31.08.97!1.1 ! PMPP_PG_SUMME   EINGEFÜGT ! OPRITZ   *
      *         !31.08.97!1.2 ! PKON_KOND_NR_GSP IN       !          *
      *         !        !    ! PKON_KOND_NR UMBENANNT    ! OPRITZ   *
      * AFPA0012!07.10.97!    ! PKON_KOND_NR_GSP UMBENANNT! STORCH   *
      * AFPA0012!09.10.97!    ! ANPASSUNGEN AUFGRUND VON  ! STORCH   *
      *         !        !    ! TABELLENÄNDERUNGEN        !          *
      * APA00012!08.07.99!    ! PI-DRPA0324-PMPP-DATEN    ! STORCH   *
      *         !        !    ! DEFINIERT.                !          *
      * APA01539!26.07.13!2.3 ! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!31.03.15!2.4 ! ERGÄNZUNGEN FÜR LUCY      ! KÖPRÜCÜ  *
      * APA02220!10.03.16!2.6 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!30.01.17!2.7 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02353!03.03.17!2.9 ! ERWEITERT UM Teleson      ! Buch     *
      * APA02579!26.04.18!2.10! FriWi neue DB2-Attribute  ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0324                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE      TBPA0103 PMPP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0324.
          05 PI-DRPA0324-UFKT                 PIC  X(002).
          05 PI-DRPA0324-PMPP-DATEN.
            10 PI-DRPA0324-PMPG-KEY.
               15 PI-DRPA0324-PMPP-ID         PIC S9(015) COMP-3.
               15 PI-DRPA0324-PMPG-ID         PIC S9(015) COMP-3.
               15 PI-DRPA0324-PGEB-GES        PIC S9(004) COMP.
               15 PI-DRPA0324-PROD-PROD-SCHL  PIC S9(004) BINARY.
            10 PI-DRPA0324-PMPP-PGV           PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-PPFL-BEITRAG  PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PPFL-SUMME    PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PG-BEITRAG    PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PG-SUMME      PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PG-PROV-STZ   PIC S9(004)V9(4) COMP-3.
            10 PI-DRPA0324-PMPP-PG-GSMT-PROV  PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PRKO-KOND-NR-GSP   PIC S9(004)      COMP.
            10 PI-DRPA0324-PMPP-PG-PROD-DATEN PIC X(030).
            10 PI-DRPA0324-PMPP-LAST-UPDATE   PIC X(026).
            10 PI-DRPA0324-PMPP-KENNUNG       PIC X(008).
            10 PI-DRPA0324-PMPP-PG-PRODART    PIC S9(004) COMP.
            10 PI-DRPA0324-KNR-DYN-KZ-1       PIC S9(004) COMP.
            10 PI-DRPA0324-KNR-DYN-KZ-2       PIC S9(004) COMP.
            10 PI-DRPA0324-KNR-BEW-HRKFT      PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-BEW-SCHL      PIC S9(009) COMP.
            10 PI-DRPA0324-PMPP-PRO-RATA-Z    PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-PRO-RATA-N    PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-BWERT-SUMME   PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-RWERT-SUMME   PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-BZ-DAUER      PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-BEITRAG-JAHR  PIC S9(007)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-NEUST-KZ      PIC X(001).
            10 PI-DRPA0324-PMPP-TARIF-SCHLGRP PIC X(015).
            10 PI-DRPA0324-KNR-TARIF-KZ       PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-TARIF-GRP     PIC S9(4)   COMP.
            10 PI-DRPA0324-PMPP-DAUER-GSMT    PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-HAFTZ-VON     PIC X(010).
            10 PI-DRPA0324-PMPP-HAFTZ-BIS     PIC X(010).
            10 PI-DRPA0324-PMPP-PG-TARIF      PIC X(015).
            10 PI-DRPA0324-PMPP-RWERT-BEITRAG PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PROV-BEITRAG  PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PROV-VERDIENT PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-VERS-SUMME    PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-BUZTAR        PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-AE-KZ         PIC X(1).
            10 PI-DRPA0324-PMPP-PG-UST-BETRAG PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-PG-UST-SATZ   PIC S9(003)V9(3) COMP-3.
            10 PI-DRPA0324-PMPP-VSP-KOMBI-NR  PIC S9(005) COMP.
            10 PI-DRPA0324-PMPP-VSP-KOMBI-BZ  PIC X(003).
            10 PI-DRPA0324-PMPP-VSP-FN-BER    PIC X(001).
            10 PI-DRPA0324-PMPP-VSP-FN-REL    PIC X(001).
            10 PI-DRPA0324-PMPP-TLSN-TARIF    PIC X(050).
            10 PI-DRPA0324-PMPP-TLSN-LAUFZEIT PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-TLSN-VRBRAUCH PIC S9(009) COMP.
            10 PI-DRPA0324-KNR-TLSN-STORNO    PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-TLSN-Z-NR     PIC X(020).
            10 PI-DRPA0324-PMPP-GEL-GEVO      PIC X(007).
            10 PI-DRPA0324-PMPP-GEL-TARBEZ    PIC X(020).
            10 PI-DRPA0324-PMPP-GEL-RABST     PIC X(005).
            10 PI-DRPA0324-PMPP-GEL-EALTER    PIC S9(004) COMP.
            10 PI-DRPA0324-PMPP-NBB-SUMME     PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-AUW-SUMME     PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA0324-PMPP-VERTRAGSTEIL  PIC S9(004) BINARY.

      *
       01 PO-DRPA0324.
          05 PO-DRPA0324-RC                   PIC  X(003).
             88 PO-DRPA0324-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0324-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
 
 



      ****************************************************************
      *    COPY-MEMBER: DRPA1446  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNER                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG             ! BRENNER  *
      * APA00452!06.11.06!2.01! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * APA01539!19.03.13!2.2 ! KALYPSO ERGAENZUNGEN      !HARALANOVA*
      * APA01539!19.03.13!2.3 ! NEUES ATTRIBUT TARIF-GRP  ! STORCH   *
      * APA01539!17.02.14!2.4 ! NEUES ATTRIBUT AE-KZ      ! RAHNER   *
      * APA01932!31.03.15!2.5 ! LUCY ERGAENZUNGEN         ! KÖPRÜCÜ  *
      * APA02220!10.03.16!2.6 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!30.01.17!2.7 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02353!03.03.17!2.13! ERWEITERT UM Teleson      ! Buch     *
      * APA02579!26.04.18!2.14! FriWi neue DB2-Attribute  ! Hofmann  *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL * 
      *         !        !    !                           !          * 
      *         !        !    !                           !          * 
      *         !        !    !                           !          * 
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1446                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBPA0133 PMPPH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1446.
          05 PI-DRPA1446-PMPG-KEY.
             10 PI-DRPA1446-PMPP-ID         PIC S9(015)      COMP-3.
             10 PI-DRPA1446-PMPG-ID         PIC S9(015)      COMP-3.
             10 PI-DRAP0325-PGEB-GES        PIC S9(004)      COMP.
             10 PI-DRPA1446-PROD-PROD-SCHL  PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-PGV           PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-PPFL-BEITRAG  PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-PPFL-SUMME    PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-PG-BEITRAG    PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-PG-SUMME      PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-PG-PROV-STZ   PIC S9(004)V9(4) COMP-3.
          05 PI-DRPA1446-PMPP-PG-GSMT-PROV  PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PRKO-KOND-NR-GSP   PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-PG-PROD-DATEN PIC  X(030).
          05 PI-DRPA1446-PMPP-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1446-PMPP-KENNUNG       PIC  X(008).
          05 PI-DRPA1446-PMPG-VERSION       PIC  S9(04)      COMP.
          05 PI-DRPA1446-PMPPH-LAST-UPDATE  PIC  X(026).
          05 PI-DRPA1446-PMPP-PG-PRODART    PIC S9(004)      COMP.
          05 PI-DRPA1446-KNR-DYN-KZ-1       PIC S9(004)      COMP.
          05 PI-DRPA1446-KNR-DYN-KZ-2       PIC S9(004)      COMP.
          05 PI-DRPA1446-KNR-BEW-HRKFT      PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-BEW-SCHL      PIC S9(009)      COMP.
          05 PI-DRPA1446-PMPP-PRO-RATA-Z    PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-PRO-RATA-N    PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-BWERT-SUMME   PIC S9(9)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-RWERT-SUMME   PIC S9(9)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-BZ-DAUER      PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-BEITRAG-JAHR  PIC S9(7)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-NEUST-KZ      PIC X(001).
          05 PI-DRPA1446-PMPP-TARIF-SCHLGRP PIC X(015).
          05 PI-DRPA1446-KNR-TARIF-KZ       PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-TARIF-GRP     PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-DAUER-GSMT    PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-HAFTZ-VON     PIC X(010).
          05 PI-DRPA1446-PMPP-HAFTZ-BIS     PIC X(010).
          05 PI-DRPA1446-PMPP-PG-TARIF      PIC X(015).
          05 PI-DRPA1446-PMPP-RWERT-BEITRAG PIC S9(9)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-PROV-BEITRAG  PIC S9(9)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-PROV-VERDIENT PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-VERS-SUMME    PIC S9(9)V9(2)   COMP-3.
          05 PI-DRPA1446-PMPP-BUZTAR        PIC S9(004)      COMP.
          05 PI-DRPA1446-PMPP-AE-KZ         PIC X(001).
          05 PI-DRPA1446-PMPP-PG-UST-BETRAG PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-PG-UST-SATZ   PIC S9(003)V9(3) COMP-3.
          05 PI-DRPA1446-PMPP-VSP-KOMBI-NR  PIC S9(005) COMP.
          05 PI-DRPA1446-PMPP-VSP-KOMBI-BZ  PIC X(003).
          05 PI-DRPA1446-PMPP-VSP-FN-BER    PIC X(001).
          05 PI-DRPA1446-PMPP-VSP-FN-REL    PIC X(001).
          05 PI-DRPA1446-PMPP-TLSN-TARIF    PIC X(050).
          05 PI-DRPA1446-PMPP-TLSN-LAUFZEIT PIC S9(004) COMP.
          05 PI-DRPA1446-PMPP-TLSN-VRBRAUCH PIC S9(009) COMP.
          05 PI-DRPA1446-KNR-TLSN-STORNO    PIC S9(004) COMP.
          05 PI-DRPA1446-PMPP-TLSN-Z-NR     PIC X(020).
          05 PI-DRPA1446-PMPP-GEL-GEVO      PIC X(007).
          05 PI-DRPA1446-PMPP-GEL-TARBEZ    PIC X(020).
          05 PI-DRPA1446-PMPP-GEL-RABST     PIC X(005).
          05 PI-DRPA1446-PMPP-GEL-EALTER    PIC S9(004) COMP.
          05 PI-DRPA1446-PMPP-NBB-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-AUW-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PI-DRPA1446-PMPP-VERTRAGSTEIL  PIC S9(004) BINARY.
      *
       01 PO-DRPA1446.
          05 PO-DRPA1446-RC                 PIC  X(003).
             88 PO-DRPA1446-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1446-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1446-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 



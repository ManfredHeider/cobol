      ****************************************************************
      *    COPY-MEMBER: DRPA1445  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNER                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                ! NAME      *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG            !BRENNER    *
      * APA01539!19.03.13!2.2 ! KALYPSO ERGAENZUNGEN     !HARALANOVA *
      * APA01539!19.03.13!2.3 ! NEUES ATTRIBUT TARIF-GRP ! STORCH    *
      *         !06.08.13!2.5 ! PMPP-AE-KZ ERGÄNZ.KALYPSO! STRAUSS   *
      * APA01932!31.03.15!2.6 ! LUCY ERGAENZUNGEN        ! KÖPRÜCÜ   *
      * APA02220!10.03.16!2.7 ! ÄNDERUNG FORMAT %-SÄTZE  ! STORCH    *
      * APA02275!30.01.17!2.8 ! ERWEITERT UM FlexN-Felder! KÖPRÜCÜ   *
      * APA02353!03.03.17!2.12! ERWEITERT UM Teleson     ! Buch      *
      * APA02579!26.04.18!2.13! FrWi neue DB2-Attribute  ! Hofmann   *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU   ! BUCH      *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU       ! SCHEIDEL  *
      *         !        !    !                          !           *
      *         !        !    !                          !           *
      *         !        !    !                          !           *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1445                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE      TBPA0133 PMPPH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1445.
          05 PI-DRPA1445-UFKT                 PIC  X(002).
          05 PI-DRPA1445-PMPP-DATEN.
            10 PI-DRPA1445-PMPG-KEY.
               15 PI-DRPA1445-PMPP-ID         PIC S9(015)      COMP-3.
               15 PI-DRPA1445-PMPG-ID         PIC S9(015)      COMP-3.
               15 PI-DRPA1445-PGEB-GES        PIC S9(004)      COMP.
               15 PI-DRPA1445-PROD-PROD-SCHL  PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-PGV           PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-PPFL-BEITRAG  PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-PPFL-SUMME    PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-PG-BEITRAG    PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-PG-SUMME      PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-PG-PROV-STZ   PIC S9(004)V9(4) COMP-3.
            10 PI-DRPA1445-PMPP-PG-GSMT-PROV  PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PRKO-KOND-NR-GSP   PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-PG-PROD-DATEN PIC  X(030).
            10 PI-DRPA1445-PMPP-LAST-UPDATE   PIC  X(026).
            10 PI-DRPA1445-PMPP-KENNUNG       PIC  X(008).
            10 PI-DRPA1445-PMPG-VERSION       PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPPH-LAST-UPDATE  PIC  X(026).
            10 PI-DRPA1445-PMPPH-KENNUNG      PIC  X(008).
            10 PI-DRPA1445-PMPP-PG-PRODART    PIC S9(004)      COMP.
            10 PI-DRPA1445-KNR-DYN-KZ-1       PIC S9(004)      COMP.
            10 PI-DRPA1445-KNR-DYN-KZ-2       PIC S9(004)      COMP.
            10 PI-DRPA1445-KNR-BEW-HRKFT      PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-BEW-SCHL      PIC S9(009)      COMP.
            10 PI-DRPA1445-PMPP-PRO-RATA-Z    PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-PRO-RATA-N    PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-BWERT-SUMME   PIC S9(9)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-RWERT-SUMME   PIC S9(9)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-BZ-DAUER      PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-BEITRAG-JAHR  PIC S9(7)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-NEUST-KZ      PIC X(001).
            10 PI-DRPA1445-PMPP-TARIF-SCHLGRP PIC X(015).
            10 PI-DRPA1445-KNR-TARIF-KZ       PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-TARIF-GRP     PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-DAUER-GSMT    PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-HAFTZ-VON     PIC X(010).
            10 PI-DRPA1445-PMPP-HAFTZ-BIS     PIC X(010).
            10 PI-DRPA1445-PMPP-PG-TARIF      PIC X(015).
            10 PI-DRPA1445-PMPP-RWERT-BEITRAG PIC S9(9)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-PROV-BEITRAG  PIC S9(9)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-PROV-VERDIENT PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-VERS-SUMME    PIC S9(9)V9(2)   COMP-3.
            10 PI-DRPA1445-PMPP-BUZTAR        PIC S9(004)      COMP.
            10 PI-DRPA1445-PMPP-AE-KZ         PIC  X(001).
            10 PI-DRPA1445-PMPP-PG-UST-BETRAG PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-PG-UST-SATZ   PIC S9(003)V9(3) COMP-3.
            10 PI-DRPA1445-PMPP-VSP-KOMBI-NR  PIC S9(005) COMP.
            10 PI-DRPA1445-PMPP-VSP-KOMBI-BZ  PIC X(003).
            10 PI-DRPA1445-PMPP-VSP-FN-BER    PIC X(001).
            10 PI-DRPA1445-PMPP-VSP-FN-REL    PIC X(001).
            10 PI-DRPA1445-PMPP-TLSN-TARIF    PIC X(050).
            10 PI-DRPA1445-PMPP-TLSN-LAUFZEIT PIC S9(004) COMP.
            10 PI-DRPA1445-PMPP-TLSN-VRBRAUCH PIC S9(009) COMP.
            10 PI-DRPA1445-KNR-TLSN-STORNO    PIC S9(004) COMP.
            10 PI-DRPA1445-PMPP-TLSN-Z-NR     PIC X(020).
            10 PI-DRPA1445-PMPP-GEL-GEVO      PIC X(007).
            10 PI-DRPA1445-PMPP-GEL-TARBEZ    PIC X(020).
            10 PI-DRPA1445-PMPP-GEL-RABST     PIC X(005).
            10 PI-DRPA1445-PMPP-GEL-EALTER    PIC S9(004) COMP.
            10 PI-DRPA1445-PMPP-NBB-SUMME     PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-AUW-SUMME     PIC S9(009)V9(2) COMP-3.
            10 PI-DRPA1445-PMPP-VERTRAGSTEIL  PIC S9(004) BINARY.
      
      *
       01 PO-DRPA1445.
          05 PO-DRPA1445-RC                   PIC  X(003).
             88 PO-DRPA1445-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1445-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
 
 
 



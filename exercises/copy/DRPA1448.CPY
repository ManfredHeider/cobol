      ****************************************************************
      *    COPY-MEMBER: DRPA1448  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNER                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG             !BRENNER   *
      * APA00452!06.11.06!2.01! UMSTELLUNG 3-STELL. PROD. !JÄGER     *
      * APA01539!19.03.13!2.3 ! KALYPSO ERGAENZUNGEN      !HARALANOVA*
      * APA01539!19.03.13!2.4 ! NEUES ATTRIBUT TARIF-GRP  !STORCH    *
      * APA01539!17.02.14!2.5 ! PMPP-AE-KZ ERGÄNZ.KALYPSO !RAHNER    *
      * APA01932!31.03.15!2.6 ! LUCY ERGAENZUNGEN         !KÖPRÜCÜ   *
      *         !        !    ! PO-DRPA1448-PMPP-AE-KZ    !KÖPRÜCÜ   *
      *         !        !    ! KORRIGIERT                !          *
      * APA02220!10.03.16!2.7 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02275!30.01.17!2.8 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
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
      *    FÜR DIE FUNKTION DRPA1448                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0133 PMPPH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1448.
          05 PI-DRPA1448-PMPG-KEY.
             10 PI-DRPA1448-PMPP-ID           PIC S9(015) COMP-3.
      *
       01 PO-DRPA1448.
          05 PO-DRPA1448-RC                   PIC  X(003).
             88 PO-DRPA1448-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1448-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA1448-PMPG-ID              PIC S9(015) COMP-3.
          05 PO-DRPA1448-PGEB-GES             PIC S9(004) COMP.
          05 PO-DRPA1448-PROD-PROD-SCHL       PIC S9(004) COMP.
      *
          05 PO-DRPA1448-PMPP-PGV             PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-PPFL-BEITRAG    PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PPFL-SUMME      PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PG-BEITRAG      PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PG-SUMME        PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PG-PROV-STZ     PIC S9(004)V9(4) COMP-3.
          05 PO-DRPA1448-PMPP-PG-GSMT-PROV    PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PRKO-KOND-NR-GSP     PIC S9(004)      COMP.
          05 PO-DRPA1448-PMPP-PG-PROD-DATEN   PIC  X(030).
          05 PO-DRPA1448-PMPP-LAST-UPDATE     PIC  X(026).
          05 PO-DRPA1448-PMPP-KENNUNG         PIC  X(008).
          05 PO-DRPA1448-PMPG-VERSION         PIC S9(004)      COMP.
          05 PO-DRPA1448-PMPPH-LAST-UPDATE    PIC  X(026).
          05 PO-DRPA1448-PMPPH-KENNUNG        PIC  X(008).
          05 PO-DRPA1448-PMPP-PG-PRODART      PIC S9(004) COMP.
          05 PO-DRPA1448-KNR-DYN-KZ-1         PIC S9(004) COMP.
          05 PO-DRPA1448-KNR-DYN-KZ-2         PIC S9(004) COMP.
          05 PO-DRPA1448-KNR-BEW-HRKFT        PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-BEW-SCHL        PIC S9(009) COMP.
          05 PO-DRPA1448-PMPP-PRO-RATA-Z      PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-PRO-RATA-N      PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-BWERT-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-RWERT-SUMME     PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-BZ-DAUER        PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-BEITRAG-JAHR    PIC S9(007)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-NEUST-KZ        PIC X(001).
          05 PO-DRPA1448-PMPP-TARIF-SCHLGRP   PIC X(015).
          05 PO-DRPA1448-KNR-TARIF-KZ         PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-TARIF-GRP       PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-DAUER-GSMT      PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-HAFTZ-VON       PIC X(010).
          05 PO-DRPA1448-PMPP-HAFTZ-BIS       PIC X(010).
          05 PO-DRPA1448-PMPP-PG-TARIF        PIC X(015).
          05 PO-DRPA1448-PMPP-RWERT-BEITRAG   PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PROV-BEITRAG    PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PROV-VERDIENT   PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-VERS-SUMME      PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-BUZTAR          PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-AE-KZ           PIC X(001).
          05 PO-DRPA1448-PMPP-PG-UST-BETRAG   PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-PG-UST-SATZ     PIC S9(003)V9(3) COMP-3.
          05 PO-DRPA1448-PMPP-VSP-KOMBI-NR    PIC S9(005) COMP.
          05 PO-DRPA1448-PMPP-VSP-KOMBI-BZ    PIC X(003).
          05 PO-DRPA1448-PMPP-VSP-FN-BER      PIC X(001).
          05 PO-DRPA1448-PMPP-VSP-FN-REL      PIC X(001).
          05 PO-DRPA1448-PMPP-TLSN-TARIF      PIC X(050).
          05 PO-DRPA1448-PMPP-TLSN-LAUFZEIT   PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-TLSN-VRBRAUCH   PIC S9(009) COMP.
          05 PO-DRPA1448-KNR-TLSN-STORNO      PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-TLSN-Z-NR       PIC X(020).
          05 PO-DRPA1448-PMPP-GEL-GEVO        PIC X(007).
          05 PO-DRPA1448-PMPP-GEL-TARBEZ      PIC X(020).
          05 PO-DRPA1448-PMPP-GEL-RABST       PIC X(005).
          05 PO-DRPA1448-PMPP-GEL-EALTER      PIC S9(004) COMP.
          05 PO-DRPA1448-PMPP-NBB-SUMME       PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-AUW-SUMME       PIC S9(009)V9(2) COMP-3.
          05 PO-DRPA1448-PMPP-VERTRAGSTEIL    PIC S9(004) BINARY.
      *
      * ********************* ENDE COPY-MEMBER **********************
 



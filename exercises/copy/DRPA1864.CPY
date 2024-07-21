      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1864.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1864.CPv  $
      *
      *   Rev 2.2   Apr 23 2018 08:27:14   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.1   Jan 03 2017 08:48:14   t13ae37
      *APA02347: BKK-Linde als eigenständige Gesellschaft
      *
      *   Rev 2.0   Nov 29 2016 15:18:02   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1438  PEG-DATENSATZ FUER BKK GES 51
      *    --------------------------------------------------------
      ****************************************************************
      *    VERSION 2.1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02347!29.11.16!2.1 ! NEU                       !BUCH      *
      * APA02579!23.04.18!2.2 ! Erweiterung ALAG          !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      *ENDE-LOGBUCH                                                  *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    BKK-DTA'S.                                                *
      *                                                              *
      ****************************************************************
      *
             30 BKK-DATENSATZ.
                35 VNR                    PIC  9(015).
                35 VVNR1                  PIC  9(015).
                35 VVNR2                  PIC  9(015).
                35 VVNR3                  PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 VERTRAG-ABLAUF         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 FAELLIG-BIS            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 KUNDE-NNAME            PIC  X(025).
                35 KUNDE-VNAME            PIC  X(020).
                35 ABR-DAT-PG             PIC  X(010).
                35 EINL-DAT-PG            PIC  X(010).
                35 PIN                    PIC  9(007).
                35 ZAHLWEISE              PIC  9(003).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 SYSTEM-KZ              PIC  X(003).
                35 GV-TIMESTAMP           PIC  X(026).
                35 FAELLIG-DAUER          PIC  9(004).
                35 HAFTZEITBEGINN         PIC  X(010).
                35 HAFTZEITABLAUF         PIC  X(010).
                35 HAFTZEITDAUER          PIC  9(004).
                35 SONDERHAFT-KZ          PIC  X(002).
                35 SONDERKENNZEICHEN      PIC  X(002).
                35 VERSICHERTE-PERSON     PIC  X(002).
                35 FILLER                 PIC  X(066).
                35 TAB-INDEX              PIC  9(002).
                35 PRODUKT-TABELLE.
230418*            40 PRODUKT-ZEILE       OCCURS 50.
230418             40 PRODUKT-ZEILE       OCCURS 45.
                      45 PGV-PG           PIC  9(004).
                      45 BUCHREG          PIC  X(005).
230418                45 FILLER           PIC  X(004).
                      45 PRODUKT-PG       PIC  X(004).
                      45 PPFL-BETRAG      PIC S9(011)V9(02).
                      45 PROV-SATZ        PIC S9(005)V9(02).
                      45 PROV-BETRAG      PIC S9(011)V9(02).
                      45 JAHRES-NETTO-PR  PIC S9(011)V9(02).
                      45 VERTR-T-VERSION  PIC  9(015).
 
 


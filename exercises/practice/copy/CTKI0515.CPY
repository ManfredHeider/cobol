      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0515.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0515.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 10:16:06   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.2   Oct 27 2018 15:24:34   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.1   Oct 25 2018 14:44:36   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.0   Sep 7 2018 11:29:26   T13EX78
      *Archiverstellung
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DBO-8960!30.08.21!    ! Neu G42SSTATUS            ! ATAR     *
      *--------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_SPARPLAN TABLE
CCC004*    ( G42SVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42SLFDNR                      SMALLINT NOT NULL,
CCC004*      G42SOEFDAT                     CHAR(8) NOT NULL,
CCC004*      G42SENDEDAT                    CHAR(8) NOT NULL,
CCC004*      G42SKONTOINHABER               CHAR(30) NOT NULL,
CCC004*      G42SWKN                        CHAR(6) NOT NULL,
CCC004*      G42SISIN                       CHAR(12) NOT NULL,
CCC004*      G42SSPARRATE                   DECIMAL(9, 2) NOT NULL,
CCC004*      G42SDYNAMIK                    DECIMAL(4, 2) NOT NULL,
CCC004*      G42SZAHLWEISE                  CHAR(1) NOT NULL,
CCC004*      G42SEINAUSDATUM                CHAR(2) NOT NULL,
CCC004*      G42SEINZAHFRISTEND             CHAR(8) NOT NULL,
CCC004*      G42SEINAUSTYP                  CHAR(2) NOT NULL,
CCC004*      G42SDATBEGINNAUS               CHAR(8) NOT NULL,
CCC004*      G42SDATENDEAUS                 CHAR(8) NOT NULL,
CCC004*      G42SKOSTENPROZ                 DECIMAL(3, 2) NOT NULL,
CCC004*      G42SURSPRSPARLST               DECIMAL(8, 2) NOT NULL,
CCC004*      G42SURSPARRATE                 DECIMAL(6, 2) NOT NULL,
CCC004*      G42SAAUFSCHLURSPR              DECIMAL(4, 2) NOT NULL,
CCC004*      G42SAAUFSCHLAKT                DECIMAL(4, 2) NOT NULL,
CCC004*      G42SVERTLAUFZEIT               SMALLINT NOT NULL,
CCC004*      G42SBEGDATDYNAM                CHAR(8) NOT NULL,
CCC004*      G42SZLASTRUECK                 SMALLINT NOT NULL,
CCC004*      G42SKUENDMERKMAL               CHAR(1) NOT NULL,
CCC004*      G42SDATVERAUFLOE               CHAR(8) NOT NULL,
CCC004*      G42SBONIFIKATION               DECIMAL(7, 4) NOT NULL,
CCC004*      G42SIBAN                       CHAR(34) NOT NULL,
CCC004*      G42SBIC                        CHAR(11) NOT NULL,
CCC004*      G42SMANDREF                    CHAR(35) NOT NULL,
CCC004*      G42SSTATUS                     CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_SPARPLAN                *
      ******************************************************************
       01  G42-SPARPLAN-GESAMT.
        05 NI-G42-SPARPLAN-TAB.
           10 NI-G42-SPARPLAN-ELE PIC S9(04) COMP OCCURS 30 TIMES.
        05 NI-G42-SPARPLAN REDEFINES NI-G42-SPARPLAN-TAB.
           10 NI-G42SVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42SLFDNR          PIC S9(4) COMP.
           10 NI-G42SOEFDAT         PIC S9(4) COMP.
           10 NI-G42SENDEDAT        PIC S9(4) COMP.
           10 NI-G42SKONTOINHABER   PIC S9(4) COMP.
           10 NI-G42SWKN            PIC S9(4) COMP.
           10 NI-G42SISIN           PIC S9(4) COMP.
           10 NI-G42SSPARRATE       PIC S9(4) COMP.
           10 NI-G42SDYNAMIK        PIC S9(4) COMP.
           10 NI-G42SZAHLWEISE      PIC S9(4) COMP.
           10 NI-G42SEINAUSDATUM    PIC S9(4) COMP.
           10 NI-G42SEINZAHFRISTEND PIC S9(4) COMP.
           10 NI-G42SEINAUSTYP      PIC S9(4) COMP.
           10 NI-G42SDATBEGINNAUS   PIC S9(4) COMP.
           10 NI-G42SDATENDEAUS     PIC S9(4) COMP.
           10 NI-G42SKOSTENPROZ     PIC S9(4) COMP.
           10 NI-G42SURSPRSPARLST   PIC S9(4) COMP.
           10 NI-G42SURSPARRATE     PIC S9(4) COMP.
           10 NI-G42SAAUFSCHLURSPR  PIC S9(4) COMP.
           10 NI-G42SAAUFSCHLAKT    PIC S9(4) COMP.
           10 NI-G42SVERTLAUFZEIT   PIC S9(4) COMP.
           10 NI-G42SBEGDATDYNAM    PIC S9(4) COMP.
           10 NI-G42SZLASTRUECK     PIC S9(4) COMP.
           10 NI-G42SKUENDMERKMAL   PIC S9(4) COMP.
           10 NI-G42SDATVERAUFLOE   PIC S9(4) COMP.
           10 NI-G42SBONIFIKATION   PIC S9(4) COMP.
           10 NI-G42SIBAN           PIC S9(4) COMP.
           10 NI-G42SBIC            PIC S9(4) COMP.
           10 NI-G42SMANDREF        PIC S9(4) COMP.
           10 NI-G42SSTATUS         PIC S9(4) COMP.
        05 DCL-G42-SPARPLAN.
           10 G42SVERTRAGSNR       PIC X(15).
           10 G42SLFDNR            PIC S9(4) USAGE COMP.
           10 G42SOEFDAT           PIC X(8).
           10 G42SENDEDAT          PIC X(8).
           10 G42SKONTOINHABER     PIC X(30).
           10 G42SWKN              PIC X(6).
           10 G42SISIN             PIC X(12).
           10 G42SSPARRATE         PIC S9(7)V9(2) USAGE COMP-3.
           10 G42SDYNAMIK          PIC S9(2)V9(2) USAGE COMP-3.
           10 G42SZAHLWEISE        PIC X(1).
           10 G42SEINAUSDATUM      PIC X(2).
           10 G42SEINZAHFRISTEND   PIC X(8).
           10 G42SEINAUSTYP        PIC X(2).
           10 G42SDATBEGINNAUS     PIC X(8).
           10 G42SDATENDEAUS       PIC X(8).
           10 G42SKOSTENPROZ       PIC S9(1)V9(2) USAGE COMP-3.
           10 G42SURSPRSPARLST     PIC S9(6)V9(2) USAGE COMP-3.
           10 G42SURSPARRATE       PIC S9(4)V9(2) USAGE COMP-3.
           10 G42SAAUFSCHLURSPR    PIC S9(2)V9(2) USAGE COMP-3.
           10 G42SAAUFSCHLAKT      PIC S9(2)V9(2) USAGE COMP-3.
           10 G42SVERTLAUFZEIT     PIC S9(4) USAGE COMP.
           10 G42SBEGDATDYNAM      PIC X(8).
           10 G42SZLASTRUECK       PIC S9(4) USAGE COMP.
           10 G42SKUENDMERKMAL     PIC X(1).
           10 G42SDATVERAUFLOE     PIC X(8).
           10 G42SBONIFIKATION     PIC S9(3)V9(4) USAGE COMP-3.
           10 G42SIBAN             PIC X(34).
           10 G42SBIC              PIC X(11).
           10 G42SMANDREF          PIC X(35).
           10 G42SSTATUS           PIC X(1).
      ************************** ENDE-DCLGEN *************************
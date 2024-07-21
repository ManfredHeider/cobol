      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0409.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0409.CPv  $
      *
      *   Rev 2.2   27 Feb 2008 14:28:40   t13ex27
      *AKL00665: Allfinanz; DCLGENs und DB-Änderung Privatkunden
      *
      *   Rev 2.1   Feb 07 2006 15:42:54   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.0   Feb 07 2006 12:48:30   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE HAFTPFLICHTVERT
      ******************************************************************
CCC005*    EXEC SQL DECLARE HAFTPFLICHTVERT TABLE
CCC004*    ( HPVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      HPVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      HPVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      HPVBEDINGUNG                   CHAR(16) NOT NULL,
CCC004*      HPVRISIKO                      CHAR(50) NOT NULL,
CCC004*      HPVKLAUSELTARIF                CHAR(80) NOT NULL,
CCC004*      HPVZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      HPVRANSCHRIFTPLZ               CHAR(6) NOT NULL,
CCC004*      HPVRANSCHRIFTSTR               CHAR(40) NOT NULL,
CCC004*      HPVRANSCHRIFTORT               CHAR(30) NOT NULL,
CCC004*      HPVRANSCHRIFTLKZ               INTEGER NOT NULL,
CCC004*      HPVMITVERSPERSON               CHAR(35) NOT NULL,
CCC004*      HPVRANSCHRHAUSNR               CHAR(15) NOT NULL,
CCC004*      HPVREGULIERUNGKZ               CHAR(2) NOT NULL,
CCC004*      HPVMINDBEITRKZ                 CHAR(2) NOT NULL,
CCC004*      HPVLFREIVONDAT                 DATE,
CCC004*      HPVLFREIBISDAT                 DATE,
CCC004*      HPVSELBSTBKZ                   CHAR(2) NOT NULL,
CCC004*      HPVSELBSTBPROZ                 DECIMAL(4, 2) NOT NULL,
CCC004*      HPVSELBSTBFEST                 INTEGER NOT NULL,
CCC004*      HPVSELBSTBTEXT                 CHAR(50) NOT NULL,
CCC004*      HPVAUSLSCHADKZ                 CHAR(2) NOT NULL,
CCC004*      HPVVERWSCHADKZ                 CHAR(2) NOT NULL,
CCC004*      HPVPRODUKT                     CHAR(30) NOT NULL,
CCC004*      HPVNAMELTERNVJKI               CHAR(45) NOT NULL,
CCC004*      HPVNAMVJKINDERN                CHAR(45) NOT NULL,
CCC004*      HPVABWEICHENDEVP               CHAR(35) NOT NULL,
CCC004*      HPVMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      HPVDIFFDECKKZ                  CHAR(2) NOT NULL,
CCC004*      HPVUMSTELLUNGDAT               DATE,
CCC004*      HPVGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      HPVVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      HPVKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      HPVZIELGRUPPE                  CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  HAFTPFLICHTVERT
      ******************************************************************
       01  HAFTPFLICHTVERT-GESAMT.
         05 NI-HAFTPFLICHTVERT-TAB.
           10 NI-HAFTPFLICHTVERT-ELE PIC S9(4) COMP OCCURS 35 TIMES.
         05 NI-HAFTPFLICHTVERT REDEFINES NI-HAFTPFLICHTVERT-TAB.
           10 NI-HPVVERTRAGSID        PIC S9(4) COMP.
           10 NI-HPVVETLFDNR          PIC S9(4) COMP.
           10 NI-HPVEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-HPVBEDINGUNG         PIC S9(4) COMP.
           10 NI-HPVRISIKO            PIC S9(4) COMP.
           10 NI-HPVKLAUSELTARIF      PIC S9(4) COMP.
           10 NI-HPVZUSATZINFO        PIC S9(4) COMP.
           10 NI-HPVRANSCHRIFTPLZ     PIC S9(4) COMP.
           10 NI-HPVRANSCHRIFTSTR     PIC S9(4) COMP.
           10 NI-HPVRANSCHRIFTORT     PIC S9(4) COMP.
           10 NI-HPVRANSCHRIFTLKZ     PIC S9(4) COMP.
           10 NI-HPVMITVERSPERSON     PIC S9(4) COMP.
           10 NI-HPVRANSCHRHAUSNR     PIC S9(4) COMP.
           10 NI-HPVREGULIERUNGKZ     PIC S9(4) COMP.
           10 NI-HPVMINDBEITRKZ       PIC S9(4) COMP.
           10 NI-HPVLFREIVONDAT       PIC S9(4) COMP.
           10 NI-HPVLFREIBISDAT       PIC S9(4) COMP.
           10 NI-HPVSELBSTBKZ         PIC S9(4) COMP.
           10 NI-HPVSELBSTBPROZ       PIC S9(4) COMP.
           10 NI-HPVSELBSTBFEST       PIC S9(4) COMP.
           10 NI-HPVSELBSTBTEXT       PIC S9(4) COMP.
           10 NI-HPVAUSLSCHADKZ       PIC S9(4) COMP.
           10 NI-HPVVERWSCHADKZ       PIC S9(4) COMP.
           10 NI-HPVPRODUKT           PIC S9(4) COMP.
           10 NI-HPVNAMELTERNVJKI     PIC S9(4) COMP.
           10 NI-HPVNAMVJKINDERN      PIC S9(4) COMP.
           10 NI-HPVABWEICHENDEVP     PIC S9(4) COMP.
           10 NI-HPVMAILINGAKTION     PIC S9(4) COMP.
           10 NI-HPVDIFFDECKKZ        PIC S9(4) COMP.
           10 NI-HPVUMSTELLUNGDAT     PIC S9(4) COMP.
           10 NI-HPVGESELLSCHAFT      PIC S9(4) COMP.
           10 NI-HPVVERSNUMMER        PIC S9(4) COMP.
           10 NI-HPVKUENDIGUNGKZ      PIC S9(4) COMP.
           10 NI-HPVZIELGRUPPE        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-HAFTPFLICHTVERT.
           10 HPVVERTRAGSID        PIC X(13).
           10 HPVVETLFDNR          PIC S9(4)  COMP.
           10 HPVEINZVTGHOSTID     PIC X(15).
           10 HPVBEDINGUNG         PIC X(16).
           10 HPVRISIKO            PIC X(50).
           10 HPVKLAUSELTARIF      PIC X(80).
           10 HPVZUSATZINFO        PIC X(100).
           10 HPVRANSCHRIFTPLZ     PIC X(6).
           10 HPVRANSCHRIFTSTR     PIC X(40).
           10 HPVRANSCHRIFTORT     PIC X(30).
           10 HPVRANSCHRIFTLKZ     PIC S9(9)  COMP.
           10 HPVMITVERSPERSON     PIC X(35).
           10 HPVRANSCHRHAUSNR     PIC X(15).
           10 HPVREGULIERUNGKZ     PIC X(2).
           10 HPVMINDBEITRKZ       PIC X(2).
           10 HPVLFREIVONDAT       PIC X(10).
           10 HPVLFREIBISDAT       PIC X(10).
           10 HPVSELBSTBKZ         PIC X(2).
           10 HPVSELBSTBPROZ       PIC S9(2)V9(2)  COMP-3.
           10 HPVSELBSTBFEST       PIC S9(9)  COMP.
           10 HPVSELBSTBTEXT       PIC X(50).
           10 HPVAUSLSCHADKZ       PIC X(2).
           10 HPVVERWSCHADKZ       PIC X(2).
           10 HPVPRODUKT           PIC X(30).
           10 HPVNAMELTERNVJKI     PIC X(45).
           10 HPVNAMVJKINDERN      PIC X(45).
           10 HPVABWEICHENDEVP     PIC X(35).
           10 HPVMAILINGAKTION     PIC X(70).
           10 HPVDIFFDECKKZ        PIC X(2).
           10 HPVUMSTELLUNGDAT     PIC X(10).
           10 HPVGESELLSCHAFT      PIC X(50).
           10 HPVVERSNUMMER        PIC X(40).
           10 HPVKUENDIGUNGKZ      PIC X(2).
           10 HPVZIELGRUPPE        PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0431.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0431.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 16:37:40   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow OZP 
      *ZMKI0431
      *
      *   Rev 2.2   15 May 2008 12:42:38   t13ex27
      *AKL00665: Allfinanz; AMV-DB-Änderung
      *
      *   Rev 2.1   17 Oct 2007 11:08:52   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:03:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE ZUSCHLAGABSCHLAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE ZUSCHLAGABSCHLAG TABLE
CCC004*    ( VZAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VZAVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VZAUNIVLFDNR                   SMALLINT NOT NULL,
CCC004*      VZAEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VZAZUABSCHLPROZ                DECIMAL(4, 2) NOT NULL,
CCC004*      VZAZUABSCHLWERT                DECIMAL(9, 2) NOT NULL,
CCC004*      VZAZUABSCHLKZ                  CHAR(3) NOT NULL,
CCC004*      VZAZUABSCHLAGTXT               CHAR(30) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VZANUMMER                      DECIMAL(9, 0) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  ZUSCHLAGABSCHLAG
      ******************************************************************
       01  ZUSCHLAGABSCHLAG-GESAMT.
         05 NI-ZUSCHLAGABSCHLAG-TAB.
           10 NI-ZUSCHLAGABSCHLAG-ELE PIC S9(4) COMP OCCURS 10 TIMES.
         05 NI-ZUSCHLAGABSCHLAG REDEFINES NI-ZUSCHLAGABSCHLAG-TAB.
           10 NI-VZAVERTRAGSID        PIC S9(4) COMP.
           10 NI-VZAVETLFDNR          PIC S9(4) COMP.
           10 NI-VZAUNIVLFDNR         PIC S9(4) COMP.
           10 NI-VZAEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VZAZUABSCHLPROZ      PIC S9(4) COMP.
           10 NI-VZAZUABSCHLWERT      PIC S9(4) COMP.
           10 NI-VZAZUABSCHLKZ        PIC S9(4) COMP.
           10 NI-VZAZUABSCHLAGTXT     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
           10 NI-VZANUMMER            PIC S9(4) COMP.
         05 DCL-ZUSCHLAGABSCHLAG.
           10 VZAVERTRAGSID        PIC X(13).
           10 VZAVETLFDNR          PIC S9(4) COMP.
           10 VZAUNIVLFDNR         PIC S9(4) COMP.
           10 VZAEINZVTGHOSTID     PIC X(15).
           10 VZAZUABSCHLPROZ      PIC S9(2)V9(2) COMP-3.
           10 VZAZUABSCHLWERT      PIC S9(7)V9(2) COMP-3.
           10 VZAZUABSCHLKZ        PIC X(3).
           10 VZAZUABSCHLAGTXT     PIC X(30).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VZANUMMER            PIC S9(9)V COMP-3.
      ******************************************************************


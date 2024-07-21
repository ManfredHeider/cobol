      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0422.CPY  $
 
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0422.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 16:40:12   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow OZP 
      *ZMKI0422
      *
      *   Rev 2.2   Sep 15 2011 15:16:42   t13ex14
      *AKI01037: SUH - FK: Aktualisierung SGWBESVEREINB
      *
      *   Rev 2.1   17 Oct 2007 11:07:44   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Žnderung
      *
      *   Rev 2.0   27 Sep 2007 12:01:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE SGWBESVEREINB
      ******************************************************************
CCC005*    EXEC SQL DECLARE SGWBESVEREINB TABLE
CCC004*    ( SGWVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      SGWVETLFDNR                    SMALLINT NOT NULL,
CCC004*      SGWRGID                        SMALLINT NOT NULL,
CCC004*      SGWPSGBLFDNR                   SMALLINT NOT NULL,
CCC004*      SGWEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      SGWBEDKLAUSELNR                CHAR(20) NOT NULL,
CCC004*      SGWVERTBTEILTXT                CHAR(80) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SGWBESVEREINB               *
      ******************************************************************
       01  SGWBESVEREINB-GESAMT.
        05 NI-SGWBESVEREINB-TAB.
           10 NI-SGWBESVEREINB-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-SGWBESVEREINB REDEFINES NI-SGWBESVEREINB-TAB.
           10 NI-SGWVERTRAGSID      PIC S9(4) COMP.
           10 NI-SGWVETLFDNR        PIC S9(4) COMP.
           10 NI-SGWRGID            PIC S9(4) COMP.
           10 NI-SGWPSGBLFDNR       PIC S9(4) COMP.
           10 NI-SGWEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-SGWBEDKLAUSELNR    PIC S9(4) COMP.
           10 NI-SGWVERTBTEILTXT    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-SGWBESVEREINB.
           10 SGWVERTRAGSID        PIC X(13).
           10 SGWVETLFDNR          PIC S9(4) USAGE COMP.
           10 SGWRGID              PIC S9(4) USAGE COMP.
           10 SGWPSGBLFDNR         PIC S9(4) USAGE COMP.
           10 SGWEINZVTGHOSTID     PIC X(15).
           10 SGWBEDKLAUSELNR      PIC X(20).
           10 SGWVERTBTEILTXT      PIC X(80).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


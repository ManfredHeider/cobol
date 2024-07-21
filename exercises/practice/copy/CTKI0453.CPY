      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/10/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0453.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0453.CPv  $
      *
      *   Rev 2.6   Oct 02 2017 11:04:32   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.5   Sep 14 2017 14:02:10   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.4   Sep 06 2017 14:15:02   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.3   Aug 08 2013 09:26:50   t13ex52
      *AKI01226: G50 CENTRAL BAP Tarifumstellung 2013
      *
      *   Rev 2.2   Oct 19 2012 13:39:48   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:27:42   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Oct 1 2012 16:57:22   T13EX20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE CKVPERSUMSTTARIF TABLE
CCC004*    ( PUMVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      PUMPERSONENNR                  SMALLINT NOT NULL,
CCC004*      PUMLEISTUNG                    DECIMAL(9, 2) NOT NULL,
CCC004*      PUMTARIFUMSTELLG               CHAR(12) NOT NULL,
CCC004*      PUMBEITRAG                     DECIMAL(9, 2),
CCC004*      PUMRISIKOZUSCHLG               DECIMAL(9, 2),
CCC004*      PUMGUTSCHRIFTBEF               DECIMAL(9, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      PUMSELBSTBEHALT                DECIMAL(9, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CKVPERSUMSTTARIF            *
      ******************************************************************
       01  CKVPERSUMSTTARIF-GESAMT.
        05 NI-CKVPERSUMSTTARIF-TAB.
           10 NI-CKVPERSUMSTTARIF-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05 NI-CKVPERSUMSTTARIF REDEFINES NI-CKVPERSUMSTTARIF-TAB.
           10 NI-PUMVERTRAGSID      PIC S9(4) COMP.
           10 NI-PUMPERSONENNR      PIC S9(4) COMP.
           10 NI-PUMLEISTUNG        PIC S9(4) COMP.
           10 NI-PUMTARIFUMSTELLG   PIC S9(4) COMP.
           10 NI-PUMBEITRAG         PIC S9(4) COMP.
           10 NI-PUMRISIKOZUSCHLG   PIC S9(4) COMP.
           10 NI-PUMGUTSCHRIFTBEF   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-PUMSELBSTBEHALT    PIC S9(4) COMP.
        05 DCL-CKVPERSUMSTTARIF.
           10 PUMVERTRAGSID        PIC X(13).
           10 PUMPERSONENNR        PIC S9(4) USAGE COMP.
           10 PUMLEISTUNG          PIC S9(7)V9(2) USAGE COMP-3.
           10 PUMTARIFUMSTELLG     PIC X(12).
           10 PUMBEITRAG           PIC S9(7)V9(2) USAGE COMP-3.
           10 PUMRISIKOZUSCHLG     PIC S9(7)V9(2) USAGE COMP-3.
           10 PUMGUTSCHRIFTBEF     PIC S9(7)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
           10 PUMSELBSTBEHALT      PIC S9(7)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************
 
 


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0403.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0403.CPv  $
      *
      *   Rev 2.6   Mar 15 2017 14:31:34   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.5   16 Dec 2008 11:30:20   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.4   01 Jul 2008 06:53:14   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben
      *
      *   Rev 2.3   04 Jun 2008 15:59:12   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben DB-Änderung
      *
      *   Rev 2.2   Sep 20 2005 08:45:28   t13ex49
      *AKI00483: Riester Förderung - OPC-Änderungen - BTGVERF - OZP
      *
      *   Rev 2.1   Jun 27 2005 16:24:44   t13ex27
      *AKI00505: DB-Änderung für Fix 77
      *
      *   Rev 2.0   Jun 27 2005 16:07:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE RIESTERZULAGE
      ******************************************************************
CCC005*    EXEC SQL DECLARE RIESTERZULAGE TABLE
CCC004*    ( RZLVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      RZLZULAGENJAHR                 SMALLINT NOT NULL,
CCC004*      RZLDATUMZULAGE                 DATE,
CCC004*      RZLZULAGEBETRAG                DECIMAL(11, 2) NOT NULL,
CCC004*      RZLZULAGEBETRAGWKZ             CHAR(1) NOT NULL,
CCC004*      RZLKUERZUNG_KTY                SMALLINT,
CCC004*      RZLKUERZUNG_KNR                DECIMAL(15, 0),
CCC004*      RZLANSPRUCH_KTY                SMALLINT,
CCC004*      RZLANSPRUCH_KNR                DECIMAL(15, 0),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE RIESTERZULAGE               *
      ******************************************************************
       01  RIESTERZULAGE-GESAMT.
        05 NI-RIESTERZULAGE-TAB.
           10 NI-RIESTERZULAGE-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05 NI-RIESTERZULAGE REDEFINES NI-RIESTERZULAGE-TAB.
           10 NI-RZLVERTRAGSID      PIC S9(4) COMP.
           10 NI-RZLZULAGENJAHR     PIC S9(4) COMP.
           10 NI-RZLDATUMZULAGE     PIC S9(4) COMP.
           10 NI-RZLZULAGEBETRAG    PIC S9(4) COMP.
           10 NI-RZLZULAGEBETRAGWKZ PIC S9(4) COMP.
           10 NI-RZLKUERZUNG-KTY    PIC S9(4) COMP.
           10 NI-RZLKUERZUNG-KNR    PIC S9(4) COMP.
           10 NI-RZLANSPRUCH-KTY    PIC S9(4) COMP.
           10 NI-RZLANSPRUCH-KNR    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-RIESTERZULAGE.
           10 RZLVERTRAGSID        PIC X(13).
           10 RZLZULAGENJAHR       PIC S9(4) USAGE COMP.
           10 RZLDATUMZULAGE       PIC X(10).
           10 RZLZULAGEBETRAG      PIC S9(9)V9(2) USAGE COMP-3.
           10 RZLZULAGEBETRAGWKZ   PIC X(1).
           10 RZLKUERZUNG-KTY      PIC S9(4) USAGE COMP.
           10 RZLKUERZUNG-KNR      PIC S9(15)V USAGE COMP-3.
           10 RZLANSPRUCH-KTY      PIC S9(4) USAGE COMP.
           10 RZLANSPRUCH-KNR      PIC S9(15)V USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18/06/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0005.CPv  $
      *
      *   Rev 2.1   Aug 10 2010 11:51:24   t13ex39
      *AKI00966: DVAG Österreich: Anpassung der VB-Nummernprüfung U
      *PKI0209
      *
      *   Rev 2.0   Jun 18 2010 10:00:28   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE MITARBMANDANT TABLE
CCC004*    ( MITARBEITERNR                  INTEGER NOT NULL,
CCC004*      MITARBMANDANTNR                INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MITARBMANDANT               *
      ******************************************************************
       01  MITARBMANDANT-GESAMT.
        05 NI-MITARBMANDANT-TAB.
           10 NI-MITARBMANDANT-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05 NI-MITARBMANDANT REDEFINES NI-MITARBMANDANT-TAB.
           10 NI-MITARBEITERNR      PIC S9(4) COMP.
           10 NI-MITARBMANDANTNR    PIC S9(4) COMP.
        05 DCL-MITARBMANDANT.
           10 MITARBEITERNR        PIC S9(9) USAGE COMP.
           10 MITARBMANDANTNR      PIC S9(9) USAGE COMP.
      ************************** ENDE-DCLGEN *************************

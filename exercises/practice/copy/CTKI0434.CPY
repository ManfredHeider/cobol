      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0434.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0434.CPv  $
      *
      *   Rev 2.1   04 Jun 2008 15:59:26   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben DB-Änderung
      *
      *   Rev 2.0   07 May 2008 09:39:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE PALETTE
      ******************************************************************
CCC005*    EXEC SQL DECLARE PALETTE TABLE
CCC004*    ( PALPALETTENNAME                CHAR(8) NOT NULL,
CCC004*      PALFONDAKT                     CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  PALETTE
      ******************************************************************
       01  PALETTE-GESAMT.
         05 NI-PALETTE-TAB.
           10 NI-PALETTE-ELE PIC S9(4) COMP OCCURS 2 TIMES.
         05 NI-PALETTE REDEFINES NI-PALETTE-TAB.
           10 NI-PALPALETTENNAME    PIC S9(4) COMP.
           10 NI-PALFONDAKT         PIC S9(4) COMP.
         05 DCL-PALETTE.
           10 PALPALETTENNAME       PIC X(8).
           10 PALFONDAKT            PIC X(1).
      ******************************************************************
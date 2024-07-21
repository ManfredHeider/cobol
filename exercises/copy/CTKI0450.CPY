      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/10/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0450.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0450.CPv  $
      *
      *   Rev 2.2   Oct 19 2012 13:39:54   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:27:48   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Oct 1 2012 16:53:10   T13EX20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE CKVVERVORSCHLAG TABLE
CCC004*    ( VVSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VVSVERTRAGSNUMMER              CHAR(15),
CCC004*      VVSVTRGERHOEHPRZ               DECIMAL(9, 2),
CCC004*      VVSVTRGBEITRRUEK               DECIMAL(9, 2),
CCC004*      VVSKUNDBINDVTRG                CHAR(1),
CCC004*      VVSSTORNOHAFTUNG               CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CKVVERVORSCHLAG             *
      ******************************************************************
       01  CKVVERVORSCHLAG-GESAMT.
        05 NI-CKVVERVORSCHLAG-TAB.
           10 NI-CKVVERVORSCHLAG-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05 NI-CKVVERVORSCHLAG REDEFINES NI-CKVVERVORSCHLAG-TAB.
           10 NI-VVSVERTRAGSID      PIC S9(4) COMP.
           10 NI-VVSVERTRAGSNUMMER  PIC S9(4) COMP.
           10 NI-VVSVTRGERHOEHPRZ   PIC S9(4) COMP.
           10 NI-VVSVTRGBEITRRUEK   PIC S9(4) COMP.
           10 NI-VVSKUNDBINDVTRG    PIC S9(4) COMP.
           10 NI-VVSSTORNOHAFTUNG   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-CKVVERVORSCHLAG.
           10 VVSVERTRAGSID        PIC X(13).
           10 VVSVERTRAGSNUMMER    PIC X(15).
           10 VVSVTRGERHOEHPRZ     PIC S9(7)V9(2) USAGE COMP-3.
           10 VVSVTRGBEITRRUEK     PIC S9(7)V9(2) USAGE COMP-3.
           10 VVSKUNDBINDVTRG      PIC X(1).
           10 VVSSTORNOHAFTUNG     PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

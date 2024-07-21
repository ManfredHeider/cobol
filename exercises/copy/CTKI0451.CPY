      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/10/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0451.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0451.CPv  $
      *
      *   Rev 2.3   Oct 22 2018 08:14:30   t13ae31
      *AKI01630: CKV Bordero neues Feld OZP
      *
      *   Rev 2.2   Oct 19 2012 13:40:00   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Oct 18 2012 10:27:54   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Oct 1 2012 16:55:47   T13EX20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE CKVPERSVORSCHLAG TABLE
CCC004*    ( VPSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VPSPERSONENNR                  SMALLINT NOT NULL,
CCC004*      VPSPERSERHOEHPRZ               DECIMAL(9, 2),
CCC004*      VPSPERSRUECKSTLG               DECIMAL(9, 2),
CCC004*      VPSPERSBEITRRUEK               DECIMAL(9, 2),
CCC004*      VPSKUNDBINDPERS                CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VPSBEITRVORTEIL                DECIMAL(9, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CKVPERSVORSCHLAG            *
      ******************************************************************
       01  CKVPERSVORSCHLAG-GESAMT.
        05 NI-CKVPERSVORSCHLAG-TAB.
           10 NI-CKVPERSVORSCHLAG-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-CKVPERSVORSCHLAG REDEFINES NI-CKVPERSVORSCHLAG-TAB.
           10 NI-VPSVERTRAGSID      PIC S9(4) COMP.
           10 NI-VPSPERSONENNR      PIC S9(4) COMP.
           10 NI-VPSPERSERHOEHPRZ   PIC S9(4) COMP.
           10 NI-VPSPERSRUECKSTLG   PIC S9(4) COMP.
           10 NI-VPSPERSBEITRRUEK   PIC S9(4) COMP.
           10 NI-VPSKUNDBINDPERS    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VPSBEITRVORTEIL    PIC S9(4) COMP.
        05 DCL-CKVPERSVORSCHLAG.
           10 VPSVERTRAGSID        PIC X(13).
           10 VPSPERSONENNR        PIC S9(4) USAGE COMP.
           10 VPSPERSERHOEHPRZ     PIC S9(7)V9(2) USAGE COMP-3.
           10 VPSPERSRUECKSTLG     PIC S9(7)V9(2) USAGE COMP-3.
           10 VPSPERSBEITRRUEK     PIC S9(7)V9(2) USAGE COMP-3.
           10 VPSKUNDBINDPERS      PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VPSBEITRVORTEIL      PIC S9(7)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************


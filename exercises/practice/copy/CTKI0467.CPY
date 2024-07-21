      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/05/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0467.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0467.CPv  $
      *
      *   Rev 2.3   May 27 2014 13:06:24   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467 SWZ
      *
      *   Rev 2.2   May 19 2014 15:27:54   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.1   May 16 2014 11:26:38   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.0   May 14 2014 07:22:06   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OIFKUENDIGUNG TABLE
CCC004*    ( OIFVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      OIFGUTHABPRODLFDNR             SMALLINT NOT NULL,
CCC004*      OIFFONDID                      SMALLINT NOT NULL,
CCC004*      OIFGHALFDNR                    SMALLINT NOT NULL,
CCC004*      OIFDATRUECKZAHLUNG             DATE NOT NULL,
CCC004*      OIFANTEILE                     DECIMAL(13, 4),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE OIFKUENDIGUNG               *
      ******************************************************************
       01  OIFKUENDIGUNG-GESAMT.
        05 NI-OIFKUENDIGUNG-TAB.
           10 NI-OIFKUENDIGUNG-ELE PIC S9(04) COMP OCCURS 7 TIMES.
        05 NI-OIFKUENDIGUNG REDEFINES NI-OIFKUENDIGUNG-TAB.
           10 NI-OIFVERTRAGSID      PIC S9(4) COMP.
           10 NI-OIFGUTHABPRODLFDNR PIC S9(4) COMP.
           10 NI-OIFFONDID          PIC S9(4) COMP.
           10 NI-OIFGHALFDNR        PIC S9(4) COMP.
           10 NI-OIFDATRUECKZAHLUNG PIC S9(4) COMP.
           10 NI-OIFANTEILE         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-OIFKUENDIGUNG.
           10 OIFVERTRAGSID        PIC X(13).
           10 OIFGUTHABPRODLFDNR   PIC S9(4) USAGE COMP.
           10 OIFFONDID            PIC S9(4) USAGE COMP.
           10 OIFGHALFDNR          PIC S9(4) USAGE COMP.
           10 OIFDATRUECKZAHLUNG   PIC X(10).
           10 OIFANTEILE           PIC S9(9)V9(4) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

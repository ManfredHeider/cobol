      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/02/17
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0481.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0481.CPv  $
      *
      *   Rev 2.2   Aug 27 2019 14:56:40   t13ex78
      *AKI01692: Standard-OZPs
      *
      *   Rev 2.1   Mar 15 2017 14:31:42   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.0   Feb 14 2017 16:30:08   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DIREKTKAUF TABLE
CCC004*    ( DIRVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DIRLFDNR                       SMALLINT NOT NULL,
CCC004*      DIRPRODUKTID                   SMALLINT NOT NULL,
CCC004*      DIRBUCHDAT                     DATE NOT NULL,
CCC004*      DIRUMSATZ                      DECIMAL(9, 2) NOT NULL,
CCC004*      DIRSTUECKZAHL                  SMALLINT NOT NULL,
CCC004*      DIRPRODBEZ                     VARCHAR(30) NOT NULL,
CCC004*      DIRMETALL                      CHAR(1),
CCC004*      DIRGRAMM                       DECIMAL(12, 2) NOT NULL,
CCC004*      DIREMAIL                       VARCHAR(40),
CCC004*      DIRRABATT                      CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DIREKTKAUF                  *
      ******************************************************************
       01  DIREKTKAUF-GESAMT.
        05 NI-DIREKTKAUF-TAB.
           10 NI-DIREKTKAUF-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-DIREKTKAUF REDEFINES NI-DIREKTKAUF-TAB.
           10 NI-DIRVERTRAGSID      PIC S9(4) COMP.
           10 NI-DIRLFDNR           PIC S9(4) COMP.
           10 NI-DIRPRODUKTID       PIC S9(4) COMP.
           10 NI-DIRBUCHDAT         PIC S9(4) COMP.
           10 NI-DIRUMSATZ          PIC S9(4) COMP.
           10 NI-DIRSTUECKZAHL      PIC S9(4) COMP.
           10 NI-DIRPRODBEZ         PIC S9(4) COMP.
           10 NI-DIRMETALL          PIC S9(4) COMP.
           10 NI-DIRGRAMM           PIC S9(4) COMP.
           10 NI-DIREMAIL           PIC S9(4) COMP.
           10 NI-DIRRABATT          PIC S9(4) COMP.
        05 DCL-DIREKTKAUF.
           10 DIRVERTRAGSID        PIC X(13).
           10 DIRLFDNR             PIC S9(4) USAGE COMP.
           10 DIRPRODUKTID         PIC S9(4) USAGE COMP.
           10 DIRBUCHDAT           PIC X(10).
           10 DIRUMSATZ            PIC S9(7)V9(2) USAGE COMP-3.
           10 DIRSTUECKZAHL        PIC S9(4) USAGE COMP.
           10 DIRPRODBEZ.
              49 DIRPRODBEZ-LEN    PIC S9(4) USAGE COMP.
              49 DIRPRODBEZ-TEXT   PIC X(30).
           10 DIRMETALL            PIC X(1).
           10 DIRGRAMM             PIC S9(10)V9(2) USAGE COMP-3.
           10 DIREMAIL.
              49 DIREMAIL-LEN      PIC S9(4) USAGE COMP.
              49 DIREMAIL-TEXT     PIC X(40).
           10 DIRRABATT            PIC X(1).
      ************************** ENDE-DCLGEN *************************


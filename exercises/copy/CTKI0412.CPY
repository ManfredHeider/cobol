      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0412.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0412.CPv  $
      *
      *   Rev 2.2   Nov 21 2013 10:12:48   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.1   May 29 2006 10:21:56   t13ex27
      *AKI00594: DB-Änderung für Fix 88 DCLGENs
      *
      *   Rev 2.0   May 19 2006 08:38:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE KFZTARIFBEST
      ******************************************************************
CCC005*    EXEC SQL DECLARE KFZTARIFBEST TABLE
CCC004*    ( KFZTARVERTRAGSID               CHAR(13) NOT NULL,
CCC004*      KFZTARLFDNR                    SMALLINT NOT NULL,
CCC004*      KFZTARTBKTX                    CHAR(10),
CCC004*      KFZTARTBTXT                    CHAR(50),
CCC004*      KFZTARTBGDVKZ                  CHAR(50),
CCC004*      KFZTARTBSPARTE                 CHAR(6)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KFZTARIFBEST                *
      ******************************************************************
       01  KFZTARIFBEST-GESAMT.
        05 NI-KFZTARIFBEST-TAB.
           10 NI-KFZTARIFBEST-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-KFZTARIFBEST REDEFINES NI-KFZTARIFBEST-TAB.
           10 NI-KFZTARVERTRAGSID   PIC S9(4) COMP.
           10 NI-KFZTARLFDNR        PIC S9(4) COMP.
           10 NI-KFZTARTBKTX        PIC S9(4) COMP.
           10 NI-KFZTARTBTXT        PIC S9(4) COMP.
           10 NI-KFZTARTBGDVKZ      PIC S9(4) COMP.
           10 NI-KFZTARTBSPARTE     PIC S9(4) COMP.
        05 DCL-KFZTARIFBEST.
           10 KFZTARVERTRAGSID     PIC X(13).
           10 KFZTARLFDNR          PIC S9(4) USAGE COMP.
           10 KFZTARTBKTX          PIC X(10).
           10 KFZTARTBTXT          PIC X(50).
           10 KFZTARTBGDVKZ        PIC X(50).
           10 KFZTARTBSPARTE       PIC X(6).
      ************************** ENDE-DCLGEN *************************

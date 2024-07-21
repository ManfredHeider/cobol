      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/03/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0532.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0532.CPv  $
      *
      *   Rev 2.2   Jun 05 2019 11:21:44   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.1   Apr 15 2019 11:27:18   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.0   Mar 22 2019 15:41:35   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G67_VERTRAG TABLE
CCC004*    ( G67VERTRAGSNR                  CHAR(15) NOT NULL,
CCC004*      G67MANDANT                     INTEGER NOT NULL,
CCC004*      G67ABSCHLUSSVBNR               INTEGER NOT NULL,
CCC004*      G67VRTRBEGINN                  CHAR(8) NOT NULL,
CCC004*      G67VRTRENDE                    CHAR(8) NOT NULL,
CCC004*      G67DLBETRAG                    DECIMAL(14, 2) NOT NULL,
CCC004*      G67KREDITZUS                   CHAR(8) NOT NULL,
CCC004*      G67KREDITAUSZ                  CHAR(8) NOT NULL,
CCC004*      G67KREDITART                   CHAR(6) NOT NULL,
CCC004*      G67SALDO                       DECIMAL(14, 2) NOT NULL,
CCC004*      G67ZINSSATZ                    DECIMAL(6, 4) NOT NULL,
CCC004*      G67RATE                        DECIMAL(14, 2) NOT NULL,
CCC004*      G67FAELLIGKEIT                 CHAR(8) NOT NULL,
CCC004*      G67SICHERHEIT                  CHAR(1) NOT NULL,
CCC004*      G67RESTLAUFZEIT                CHAR(3) NOT NULL,
CCC004*      G67STATUS                      CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G67_VERTRAG                 *
      ******************************************************************
       01  G67-VERTRAG-GESAMT.
        05 NI-G67-VERTRAG-TAB.
           10 NI-G67-VERTRAG-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-G67-VERTRAG REDEFINES NI-G67-VERTRAG-TAB.
           10 NI-G67VERTRAGSNR      PIC S9(4) COMP.
           10 NI-G67MANDANT         PIC S9(4) COMP.
           10 NI-G67ABSCHLUSSVBNR   PIC S9(4) COMP.
           10 NI-G67VRTRBEGINN      PIC S9(4) COMP.
           10 NI-G67VRTRENDE        PIC S9(4) COMP.
           10 NI-G67DLBETRAG        PIC S9(4) COMP.
           10 NI-G67KREDITZUS       PIC S9(4) COMP.
           10 NI-G67KREDITAUSZ      PIC S9(4) COMP.
           10 NI-G67KREDITART       PIC S9(4) COMP.
           10 NI-G67SALDO           PIC S9(4) COMP.
           10 NI-G67ZINSSATZ        PIC S9(4) COMP.
           10 NI-G67RATE            PIC S9(4) COMP.
           10 NI-G67FAELLIGKEIT     PIC S9(4) COMP.
           10 NI-G67SICHERHEIT      PIC S9(4) COMP.
           10 NI-G67RESTLAUFZEIT    PIC S9(4) COMP.
           10 NI-G67STATUS          PIC S9(4) COMP.
        05 DCL-G67_VERTRAG.
           10 G67VERTRAGSNR        PIC X(15).
           10 G67MANDANT           PIC S9(9) USAGE COMP.
           10 G67ABSCHLUSSVBNR     PIC S9(9) USAGE COMP.
           10 G67VRTRBEGINN        PIC X(8).
           10 G67VRTRENDE          PIC X(8).
           10 G67DLBETRAG          PIC S9(12)V9(2) USAGE COMP-3.
           10 G67KREDITZUS         PIC X(8).
           10 G67KREDITAUSZ        PIC X(8).
           10 G67KREDITART         PIC X(6).
           10 G67SALDO             PIC S9(12)V9(2) USAGE COMP-3.
           10 G67ZINSSATZ          PIC S9(2)V9(4) USAGE COMP-3.
           10 G67RATE              PIC S9(12)V9(2) USAGE COMP-3.
           10 G67FAELLIGKEIT       PIC X(8).
           10 G67SICHERHEIT        PIC X(1).
           10 G67RESTLAUFZEIT      PIC X(3).
           10 G67STATUS            PIC X(1).
      ************************** ENDE-DCLGEN *************************



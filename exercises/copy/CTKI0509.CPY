      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0509.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0509.CPv  $
      *
      *   Rev 2.3   Nov 21 2019 08:36:38   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.2   Nov 28 2018 17:34:46   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:18:24   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:08:58   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE SPERRARCHIV TABLE
CCC004*    ( SPAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      SPAVERTRAGSNR                  CHAR(15) NOT NULL,
CCC004*      SPAPARTGESELLID                SMALLINT NOT NULL,
CCC004*      SPASTATUS_KTY                  SMALLINT NOT NULL,
CCC004*      SPASTATUS_KNR                  SMALLINT NOT NULL,
CCC004*      SPAGRUND                       CHAR(1) NOT NULL,
CCC004*      SPASPERRDAT                    DATE NOT NULL,
CCC004*      SPAARCHIVTST                   TIMESTAMP,
CCC004*      SPALOESCHTST                   TIMESTAMP,
CCC004*      SPAENTSPERRTST                 TIMESTAMP,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      SPALFDNR                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SPERRARCHIV                 *
      ******************************************************************
       01  SPERRARCHIV-GESAMT.
        05 NI-SPERRARCHIV-TAB.
           10 NI-SPERRARCHIV-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05 NI-SPERRARCHIV REDEFINES NI-SPERRARCHIV-TAB.
           10 NI-SPAVERTRAGSID      PIC S9(4) COMP.
           10 NI-SPAVERTRAGSNR      PIC S9(4) COMP.
           10 NI-SPAPARTGESELLID    PIC S9(4) COMP.
           10 NI-SPASTATUS-KTY      PIC S9(4) COMP.
           10 NI-SPASTATUS-KNR      PIC S9(4) COMP.
           10 NI-SPAGRUND           PIC S9(4) COMP.
           10 NI-SPASPERRDAT        PIC S9(4) COMP.
           10 NI-SPAARCHIVTST       PIC S9(4) COMP.
           10 NI-SPALOESCHTST       PIC S9(4) COMP.
           10 NI-SPAENTSPERRTST     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-SPALFDNR           PIC S9(4) COMP.
        05 DCL-SPERRARCHIV.
           10 SPAVERTRAGSID        PIC X(13).
           10 SPAVERTRAGSNR        PIC X(15).
           10 SPAPARTGESELLID      PIC S9(4) USAGE COMP.
           10 SPASTATUS-KTY        PIC S9(4) USAGE COMP.
           10 SPASTATUS-KNR        PIC S9(4) USAGE COMP.
           10 SPAGRUND             PIC X(1).
           10 SPASPERRDAT          PIC X(10).
           10 SPAARCHIVTST         PIC X(26).
           10 SPALOESCHTST         PIC X(26).
           10 SPAENTSPERRTST       PIC X(26).
           10 LETZTEAENDERUNG      PIC X(26).
           10 SPALFDNR             PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************


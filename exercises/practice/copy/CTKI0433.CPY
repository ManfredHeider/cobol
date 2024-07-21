      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0433.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0433.CPv  $
      *
      *   Rev 2.3   Mar 02 2015 16:24:18   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.2   Sep 01 2014 10:36:32   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   04 Jun 2008 15:59:20   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben DB-Änderung
      *
      *   Rev 2.0   07 May 2008 09:38:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE LEBENFONDSBEZ
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENFONDSBEZ TABLE
CCC004*    ( LFBFONDSKUERZEL                CHAR(2) NOT NULL,
CCC004*      LFBFONDSBEZEICH                CHAR(50) NOT NULL,
CCC004*      LFBWKN                         CHAR(6),
CCC004*      LFBISIN                        CHAR(12),
CCC004*      LFBGUELTIGAB                   DATE NOT NULL,
CCC004*      LFBGUELTIGBIS                  DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENFONDSBEZ               *
      ******************************************************************
       01  LEBENFONDSBEZ-GESAMT.
        05 NI-LEBENFONDSBEZ-TAB.
           10 NI-LEBENFONDSBEZ-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-LEBENFONDSBEZ REDEFINES NI-LEBENFONDSBEZ-TAB.
           10 NI-LFBFONDSKUERZEL    PIC S9(4) COMP.
           10 NI-LFBFONDSBEZEICH    PIC S9(4) COMP.
           10 NI-LFBWKN             PIC S9(4) COMP.
           10 NI-LFBISIN            PIC S9(4) COMP.
           10 NI-LFBGUELTIGAB       PIC S9(4) COMP.
           10 NI-LFBGUELTIGBIS      PIC S9(4) COMP.
        05 DCL-LEBENFONDSBEZ.
           10 LFBFONDSKUERZEL      PIC X(2).
           10 LFBFONDSBEZEICH      PIC X(50).
           10 LFBWKN               PIC X(6).
           10 LFBISIN              PIC X(12).
           10 LFBGUELTIGAB         PIC X(10).
           10 LFBGUELTIGBIS        PIC X(10).
      ************************** ENDE-DCLGEN *************************


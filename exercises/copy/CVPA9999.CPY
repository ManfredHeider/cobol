      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvpa9999.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\cvpa9999.CPv  $
      *
      *   Rev 2.1   Feb 23 2000 14:05:40   T13EX21
      *APA00081 - Erweiterung um PGEB-GES
      *
      *   Rev 2.0   26 Jul 1999 10:39:00   T01JM2S
      *APA00012 - NT-Umstellung
      *
      *   Rev 1.0   23 Oct 1998 15:04:12   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIPA9999)                                         *
      *        LANGUAGE(COBOL)                                         *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIPA9999 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROT_ID                        INTEGER NOT NULL,
CCC004*      KNR_PROT_ART                   INTEGER NOT NULL,
CCC004*      PROT_ERSTELL_DAT               DATE NOT NULL,
CCC004*      KNR_OBJEKT_HKF_TAB             INTEGER NOT NULL,
CCC004*      PROT_PTR_ID                    INTEGER NOT NULL,
CCC004*      KNR_PROT_PTR_OBJ               SMALLINT NOT NULL,
CCC004*      KNR_PROT_PTR_AUSPR             SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIPA9999                           *
      ******************************************************************
       01  VIPA9999-GESAMT.
         05  DCL-VIPA9999.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROT-ID              PIC S9(9) USAGE COMP.
           10 KNR-PROT-ART         PIC S9(9) USAGE COMP.
           10 PROT-ERSTELL-DAT     PIC X(10).
           10 KNR-OBJEKT-HKF-TAB   PIC S9(9) USAGE COMP.
           10 PROT-PTR-ID          PIC S9(9) USAGE COMP.
           10 KNR-PROT-PTR-OBJ     PIC S9(4) USAGE COMP.
           10 KNR-PROT-PTR-AUSPR   PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************

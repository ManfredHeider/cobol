      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0125.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPA0125.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:28:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Aug 1997 13:13:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0125)                                         *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0125 TABLE
CCC004*    ( PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KNR_FKT_BEREICH                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KOF_GSMT_PROV                  DECIMAL(15, 2) NOT NULL,
CCC004*      KOF_ANZ_BEWEGUNG               INTEGER NOT NULL,
CCC004*      KOF_ABSCHLUSS_DAT              DATE NOT NULL,
CCC004*      KOF_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KOF_KENNUNG                    CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0125                           *
      ******************************************************************
       01  TBPA0125-GESAMT.
         05  DCL-TBPA0125.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KOF-GSMT-PROV        PIC S9(13)V9(2) USAGE COMP-3.
           10 KOF-ANZ-BEWEGUNG     PIC S9(9) USAGE COMP.
           10 KOF-ABSCHLUSS-DAT    PIC X(10).
           10 KOF-LAST-UPDATE      PIC X(26).
           10 KOF-KENNUNG          PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

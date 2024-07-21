      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0126.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPA0126.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:26:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:29:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   04 Aug 1997 13:13:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0126)                                         *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0126 TABLE
CCC004*    ( PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KNR_FKT_BEREICH_AB             SMALLINT NOT NULL,
CCC004*      KNR_FKT_BEREICH_ZU             SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KOFV_GSMT_PROV                 DECIMAL(15, 2) NOT NULL,
CCC004*      KOFV_ANZ_BEW_AB                INTEGER NOT NULL,
CCC004*      KOFV_ANZ_BEW_ZU                INTEGER NOT NULL,
CCC004*      KOFV_ABSCHLUSS_DAT             DATE NOT NULL,
CCC004*      KOFV_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      KOFV_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0126                           *
      ******************************************************************
       01  TBPA0126-GESAMT.
         05  DCL-TBPA0126.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH-AB   PIC S9(4) USAGE COMP.
           10 KNR-FKT-BEREICH-ZU   PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KOFV-GSMT-PROV       PIC S9(13)V9(2) USAGE COMP-3.
           10 KOFV-ANZ-BEW-AB      PIC S9(9) USAGE COMP.
           10 KOFV-ANZ-BEW-ZU      PIC S9(9) USAGE COMP.
           10 KOFV-ABSCHLUSS-DAT   PIC X(10).
           10 KOFV-LAST-UPDATE     PIC X(26).
           10 KOFV-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0101.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0101.cpv  $
      *
      *   Rev 2.1   Feb 17 2000 16:00:46   T13EX44
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:26:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:51:38   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:39:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0101)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0101 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PMPG_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPG_VERSION                   SMALLINT NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPG_LFD_NR_VNR                INTEGER NOT NULL,
CCC004*      PMPG_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PMPGH_LAST_UPDATE              TIMESTAMP NOT NULL,
CCC004*      PMPGH_KENNUNG                  CHAR(8) NOT NULL,
CCC004*      PMPG_INHALT                    VARCHAR(3900) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0101                     *
      ******************************************************************
       01  TBPA0101-GESAMT.
         05  DCL-TBPA0101.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-VERSION         PIC S9(4) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPG-LFD-NR-VNR      PIC S9(9) USAGE COMP.
           10 PMPG-VSTAT           PIC X(4).
           10 PMPGH-LAST-UPDATE    PIC X(26).
           10 PMPGH-KENNUNG        PIC X(8).
           10 PMPG-INHALT.
              49 PMPG-INHALT-LEN   PIC S9(4) USAGE COMP.
              49 PMPG-INHALT-TEXT  PIC X(3900).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

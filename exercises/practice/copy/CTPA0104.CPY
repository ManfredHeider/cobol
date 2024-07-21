      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0104.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTPA0104.CPv  $
      *
      *   Rev 2.4   Aug 06 2015 09:38:40   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.3   Aug 06 2015 09:18:02   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Jul 16 2015 15:17:54   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Feb 17 2000 16:00:50   T13EX44
      *
      *
      *   Rev 2.0   Feb 27 1999 14:26:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:53:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:40:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0104)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0104 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PDIF_PGV                       SMALLINT NOT NULL,
CCC004*      PDIF_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PDIF_ANZ_BEWEGUNG              INTEGER NOT NULL,
CCC004*      PDIF_GUTSCHRIFTEN              DECIMAL(15, 2) NOT NULL,
CCC004*      PDIF_BELASTUNGEN               DECIMAL(15, 2) NOT NULL,
CCC004*      PDIF_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PDIF_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PDIF_UST_BETRAG_GUT            DECIMAL(11, 2) NOT NULL,
CCC004*      PDIF_UST_BETRAG_BEL            DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0104                     *
      ******************************************************************
       01  TBPA0104-GESAMT.
         05  DCL-TBPA0104.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PDIF-PGV             PIC S9(4) USAGE COMP.
           10 PDIF-VSTAT           PIC X(4).
           10 PDIF-ANZ-BEWEGUNG    PIC S9(9) USAGE COMP.
           10 PDIF-GUTSCHRIFTEN    PIC S9(13)V9(2) USAGE COMP-3.
           10 PDIF-BELASTUNGEN     PIC S9(13)V9(2) USAGE COMP-3.
           10 PDIF-LAST-UPDATE     PIC X(26).
           10 PDIF-KENNUNG         PIC X(8).
290615     10 PDIF-UST-BETRAG-GUT  PIC S9(009)V9(2) COMP-3.
290615     10 PDIF-UST-BETRAG-BEL  PIC S9(009)V9(2) COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0017.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTPA0017.CPv  $
      *
      *   Rev 2.6   05 Mar 2018 11:37:14   t13ex67
      *APA02481: Anzeige des Buchungsdatums im PG11 - MSS
      *
      *   Rev 2.5   Jul 16 2015 15:10:16   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.4   Jun 19 2012 09:32:06   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:57:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 14:08:44   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Apr 19 2006 09:45:52   t13ae46
      *APA00449: Kontrolle manueller Datensätze (Anforderung KPMG a
      *n Daisy)
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   11 Nov 1998 08:45:50   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.2   Mar 06 1997 14:12:36   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:32:36   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:28:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0017                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0017                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0017)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0017))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0017 TABLE
CCC004*    ( PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KNR_DTA_ART                    SMALLINT NOT NULL,
CCC004*      KNR_DTA_SPLIT_ART              SMALLINT NOT NULL,
CCC004*      PDTA_ORIGINAL_ID               DECIMAL(15, 0) NOT NULL,
CCC004*      PDTA_ERSTELL_DAT               DATE NOT NULL,
CCC004*      PDTA_TRANSF_DAT                DATE NOT NULL,
CCC004*      PDTA_STAND_DAT                 DATE NOT NULL,
CCC004*      PDTA_ANZ_SATZ                  INTEGER NOT NULL,
CCC004*      PDTA_ANZ_FEHL                  INTEGER NOT NULL,
CCC004*      PDTA_KONTR_WERT                DECIMAL(13, 2) NOT NULL,
CCC004*      PDTA_INHALT                    CHAR(40) NOT NULL,
CCC004*      PDTA_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PDTA_ERSTELLT_VON              CHAR(8) NOT NULL,
CCC004*      PDTA_PVW_VON                   CHAR(8) NOT NULL,
CCC004*      PDTA_PVW_DAT                   CHAR(10)NOT NULL,
CCC004*      PDTA_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PDTA_KENNUNG                   CHAR(8)  NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PDTA_UST_BETRAG                DECIMAL(11, 2) NOT NULL,
CCC004*      PDTA_BUCH_DAT                  CHAR(10)NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0017                     *
      ******************************************************************
       01  TBPA0017-GESAMT.
           05  DCL-TBPA0017.
               10 PDTA-ID           PIC S9(15)V USAGE COMP-3.
               10 PGEB-GES          PIC S9(4) USAGE COMP.
               10 KNR-DTA-ART       PIC S9(4) USAGE COMP.
               10 KNR-DTA-SPLIT-ART PIC S9(4) USAGE COMP.
               10 PDTA-ORIGINAL-ID  PIC S9(15)V USAGE COMP-3.
               10 PDTA-ERSTELL-DAT  PIC X(10).
               10 PDTA-TRANSF-DAT   PIC X(10).
               10 PDTA-STAND-DAT    PIC X(10).
               10 PDTA-ANZ-SATZ     PIC S9(9) USAGE COMP.
               10 PDTA-ANZ-FEHL     PIC S9(9) USAGE COMP.
               10 PDTA-KONTR-WERT   PIC S9(11)V9(2) USAGE COMP-3.
               10 PDTA-INHALT       PIC X(40).
               10 PDTA-VSTAT        PIC X(4).
050406         10 PDTA-ERSTELLT-VON PIC X(8).
050406         10 PDTA-PVW-VON      PIC X(8).
050406         10 PDTA-PVW-DAT      PIC X(10).
               10 PDTA-LAST-UPDATE  PIC X(26).
               10 PDTA-KENNUNG      PIC X(8).
230212         10 KNR-MANDANT       PIC S9(4) USAGE COMP.
220415         10 PDTA-UST-BETRAG   PIC S9(09)V9(2) USAGE COMP-3.
100717         10 PDTA-BUCH-DAT     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************



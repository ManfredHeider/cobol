      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0025.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTPA0025.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:32:12   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:57:10   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:20:52   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jun 22 1999 23:08:58   T13AE18
      *apa00012
      *
      *   Rev 2.0   Feb 27 1999 14:26:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:01:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0025)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0025 TABLE
CCC004*    ( PRUB_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PDPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPT_VERSION                   SMALLINT NOT NULL,
CCC004*      KNR_BUCH_GRUND_GRP             SMALLINT NOT NULL,
CCC004*      PRUB_UMBUCH_DAT                DATE NOT NULL,
CCC004*      PRUB_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0025                           *
      ******************************************************************
       01  TBPA0025-GESAMT.
         05  DCL-TBPA0025.
           10 PRUB-ID              PIC S9(15)V USAGE COMP-3.
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 KNR-BUCH-GRUND-GRP   PIC S9(4) USAGE COMP.
           10 PRUB-UMBUCH-DAT      PIC X(10).
           10 PRUB-KENNUNG         PIC X(8).
           10 KNR-MANDANT          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************

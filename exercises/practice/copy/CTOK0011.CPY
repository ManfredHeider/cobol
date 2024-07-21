      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0011.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTOK0011.CPv  $
      *
      *   Rev 2.3   Sep 05 2018 15:30:48   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI -Abgleich
      *
      *   Rev 2.2   25 Jan 2007 11:41:08   t13ex49
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Teil 2
      *
      *   Rev 2.0   Apr 06 2006 15:43:34   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13KIS.OKIMAPPING)                                *
      *        LIBRARY(T13EX39.PRIVATE.DATA(CTKIOKI))                  *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13KIS.OKIMAPPING TABLE
CCC004*    ( OMHOSTTBNAME                   CHAR(30) NOT NULL,
CCC004*      OMHOSTATTRIBUT                 CHAR(30) NOT NULL,
CCC004*      OMGESELLSCHAFT                 SMALLINT NOT NULL,
CCC004*      OMOKITBNAMEE                   CHAR(30) NOT NULL,
CCC004*      OMOKIATTRIBUTE                 CHAR(30) NOT NULL,
CCC004*      OMGESID                        CHAR(1),
CCC004*      OMID_NR                        CHAR(1),
CCC004*      OMINSUPD                       CHAR(1),
CCC004*      OMDELUE                        CHAR(1),
CCC004*      OMSUBSELECT                    CHAR(2),
CCC004*      OMOKIFORMAT                    CHAR(1) NOT NULL,
CCC004*      OMSUPUMS                       CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13KIS.OKIMAPPING                  *
      ******************************************************************
       01  OKIMAPPING-GESAMT.
         05 NI-OKIMAPPING-TAB.
           10 NI-OKIMAPPING-ELE PIC S9(4) COMP OCCURS 12 TIMES.
         05 NI-OKIMAPPING REDEFINES NI-OKIMAPPING-TAB.
           10 NI-OMHOSTTBNAME         PIC S9(4) COMP.
           10 NI-OMHOSTATTRIBUT       PIC S9(4) COMP.
           10 NI-OMGESELLSCHAFT       PIC S9(4) COMP.
           10 NI-OMOKITBNAMEE         PIC S9(4) COMP.
           10 NI-OMOKIATTRIBUTE       PIC S9(4) COMP.
           10 NI-OMGESID              PIC S9(4) COMP.
           10 NI-OMID-NR              PIC S9(4) COMP.
           10 NI-OMINSUPD             PIC S9(4) COMP.
           10 NI-OMDELUE              PIC S9(4) COMP.
           10 NI-OMSUBSELECT          PIC S9(4) COMP.
           10 NI-OMOKIFORMAT          PIC S9(4) COMP.
           10 NI-OMSUPUMS             PIC S9(4) COMP.
         05 DCL-OKIMAPPING.
           10 OMHOSTTBNAME         PIC X(30).
           10 OMHOSTATTRIBUT       PIC X(30).
           10 OMGESELLSCHAFT       PIC S9(4) USAGE COMP.
           10 OMOKITBNAMEE         PIC X(30).
           10 OMOKIATTRIBUTE       PIC X(30).
           10 OMGESID              PIC X(1).
           10 OMID-NR              PIC X(1).
           10 OMINSUPD             PIC X(1).
           10 OMDELUE              PIC X(1).
           10 OMSUBSELECT          PIC X(2).
           10 OMOKIFORMAT          PIC X(1).
           10 OMSUPUMS             PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************


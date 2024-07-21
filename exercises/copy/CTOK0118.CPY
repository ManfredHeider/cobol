      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0118.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0118.CPv  $
      *
      *   Rev 2.1   Nov 22 2006 12:04:58   t13ex43
      *AKI00612: Migration Kommunikation_MIG
      *
      *   Rev 2.0   Nov 13 2006 15:15:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TCOK0118)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTOK0118)               *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE KOMMUNIKATION_MIG TABLE
CCC004*    ( KOMMPERSONENID CHAR (13) NOT NULL,
CCC004*      KOMMTELARTID SMALLINT NOT NULL,
CCC004*      KOMMLAUFENDENUMMER INTEGER NOT NULL,
CCC004*      KOMMNUMMER VARCHAR (128) NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KOMMUNIKATION-MIG-GESAMT.
        05  NI-KOMMUNIKATION-MIG-TAB.
           10 NI-KOMMUNIKATION-MIG-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  NI-KOMMUNIKATION-MIG  REDEFINES  NI-KOMMUNIKATION-MIG-TAB.
           10 NI-KOMMPERSONENID           PIC S9(4) COMP.
           10 NI-KOMMTELARTID             PIC S9(4) COMP.
           10 NI-KOMMLAUFENDENUMMER       PIC S9(4) COMP.
           10 NI-KOMMNUMMER               PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-KOMMUNIKATION-MIG.
           10 KOMMPERSONENID              PIC X(13).
           10 KOMMTELARTID                PIC S9(4) COMP.
           10 KOMMLAUFENDENUMMER          PIC S9(9) COMP.
           10 KOMMNUMMER.
              49 KOMMNUMMER-LEN           PIC S9(4) COMP.
              49 KOMMNUMMER-TEXT          PIC X(128).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
      ****   ENDE CTOK0118   *******************************************
      ******************************************************************

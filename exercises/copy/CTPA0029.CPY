      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0029.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0029.CPv  $
      *
      *   Rev 2.3   Sep 02 2016 12:37:36   t13ex24
      *APA02151: BasisRente verteilte Abschlussprovisionen
      *
      *   Rev 2.2   Jan 13 2014 10:23:38   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.1   Apr 11 2013 14:05:10   t13ex44
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PGV-Ermittlung
      *
      *   Rev 2.0   Feb 27 1999 14:26:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:02:16   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13DIS.TBPA0029)                                  *
      *        LIBRARY(T130005.DI010000.SRCDCL(CTPA0029))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13DIS.TBPA0029 TABLE
CCC004*    ( PGVE_BEW_SCHL_IN               CHAR(6) NOT NULL,
CCC004*      PGVE_BEW_SCHL_EX               CHAR(6),
CCC004*      PGVE_PRODART                   CHAR(1) NOT NULL,
CCC004*      PGVE_DYN_KZ_2                  CHAR(1) NOT NULL,
CCC004*      PGVE_AE_KZ_IN                  CHAR(1) NOT NULL,
CCC004*      PGVE_AE_KZ_EX                  CHAR(1),
CCC004*      PGVE_UNR_IN                    CHAR(2) NOT NULL,
CCC004*      PGVE_UNR_EX                    CHAR(2),
CCC004*      PGVE_SCHICHT_NR_IN             CHAR(2) NOT NULL,
CCC004*      PGVE_SCHICHT_NR_EX             CHAR(2),
CCC004*      PGVE_VVG_KZ                    CHAR(1) NOT NULL,
CCC004*      PGVE_NEUST_KZ                  CHAR(1) NOT NULL,
CCC004*      PGVE_PROV_SCHL_IN              CHAR(5) NOT NULL,
CCC004*      PGVE_PROV_SCHL_EX              CHAR(5),
CCC004*      PGVE_GESPROV_KZ                CHAR(1) NOT NULL,
CCC004*      PGVE_PGV                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13DIS.TBPA0029                    *
      ******************************************************************
       01  DCL-TBPA0029.
           10 PGVE-BEW-SCHL-IN     PIC X(6).
           10 PGVE-BEW-SCHL-EX     PIC X(6).
           10 PGVE-PRODART         PIC X(1).
           10 PGVE-DYN-KZ-2        PIC X(1).
           10 PGVE-AE-KZ-IN        PIC X(1).
           10 PGVE-AE-KZ-EX        PIC X(1).
           10 PGVE-UNR-IN          PIC X(2).
           10 PGVE-UNR-EX          PIC X(2).
           10 PGVE-SCHICHT-NR-IN   PIC X(2).
           10 PGVE-SCHICHT-NR-EX   PIC X(2).
           10 PGVE-VVG-KZ          PIC X(1).
           10 PGVE-NEUST-KZ        PIC X(1).
           10 PGVE-PROV-SCHL-IN    PIC X(5).
           10 PGVE-PROV-SCHL-EX    PIC X(5).
           10 PGVE-GESPROV-KZ      PIC X(1).
           10 PGVE-PGV             PIC S9(4) USAGE COMP.
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  NI-TBPA0029.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 14 TIMES.
       01  FILLER REDEFINES NI-TBPA0029.
           10 NI-PGVE-BEW-SCHL-IN  PIC S9(4) USAGE COMP.
           10 NI-PGVE-BEW-SCHL-EX  PIC S9(4) USAGE COMP.
           10 NI-PGVE-PRODART      PIC S9(4) USAGE COMP.
           10 NI-PGVE-DYN-KZ-2     PIC S9(4) USAGE COMP.
           10 NI-PGVE-AE-KZ-IN     PIC S9(4) USAGE COMP.
           10 NI-PGVE-AE-KZ-EX     PIC S9(4) USAGE COMP.
           10 NI-PGVE-UNR-IN       PIC S9(4) USAGE COMP.
           10 NI-PGVE-UNR-EX       PIC S9(4) USAGE COMP.
           10 NI-PGVE-SCHICHT-NR-IN
                                   PIC S9(4) USAGE COMP.
           10 NI-PGVE-SCHICHT-NR-EX
                                   PIC S9(4) USAGE COMP.
           10 NI-PGVE-VVG-KZ       PIC S9(4) USAGE COMP.
           10 NI-PGVE-NEUST-KZ     PIC S9(4) USAGE COMP.
           10 NI-PGVE-PROV-SCHL-IN PIC S9(4) USAGE COMP.
           10 NI-PGVE-PROV-SCHL-EX PIC S9(4) USAGE COMP.
           10 NI-PGVE-GESPROV-KZ   PIC S9(4) USAGE COMP.
           10 NI-PGVE-PGV          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************


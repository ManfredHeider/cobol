      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0050.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVAT0050.CPv  $
      *
      *   Rev 2.1   May 03 2013 12:23:28   t13ae13
      *AVB01283: Hinterlegen IHK-Adressen im VB-Dialog
      *
      *   Rev 2.0   Apr 29 2013 11:27:08   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIAT0050)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0050))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0050 TABLE
CCC004*    ( IHK_NR                     SMALLINT  NOT NULL,
CCC004*      IHK_BEHOERDE               CHAR(40) NOT NULL WITH DEFAULT,
CCC004*      IHK_STRASSE                CHAR(40) NOT NULL WITH DEFAULT,
CCC004*      IHK_PLZ                    CHAR(9) NOT NULL WITH DEFAULT,
CCC004*      IHK_ORT                    CHAR(40) NOT NULL WITH DEFAULT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0050                           *
      ******************************************************************
       01  VIAT0050-GESAMT.
         05 NI-VIAT0050-TAB.
           10 NI-VIAT0050-ELE      PIC S9(4)  COMP   OCCURS   5 TIMES.
         05 NI-VIAT0050            REDEFINES  NI-VIAT0050-TAB.
           10 NI-IHK-NR            PIC S9(4)  COMP.
           10 NI-IHK-BEHOERDE      PIC S9(4)  COMP.
           10 NI-IHK-STRASSE       PIC S9(4)  COMP.
           10 NI-IHK-PLZ           PIC S9(4)  COMP.
           10 NI-IHK-ORT           PIC S9(4)  COMP.
         05 DCL-VIAT0050.
           10 IHK-NR               PIC S9(4) USAGE COMP.
           10 IHK-BEHOERDE         PIC X(40).
           10 IHK-STRASSE          PIC X(40).
           10 IHK-PLZ              PIC X(9).
           10 IHK-ORT              PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0122.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0122.CPv  $
      *
      *   Rev 2.2   Jan 09 2018 13:43:22   t13ae13
      *AVB01418: Fehler bei Compile auf M: (swl) 
      *
      *   Rev 2.1   Jan 04 2012 11:02:36   t13ae13
      *AVB01200: Erfassung von Branche und Unternehmen bei Neueinst
      *ellungen
      *
      *   Rev 2.0   Dec 01 2011 11:50:18   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01200!01.12.11!2.1 ! NEU                       ! S. Walb  *
      * AVB01418!09.01.18!2.2 ! '-' --> '_'               ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0122)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0122))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0122 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      BRAUNTZ_DAT_VON                DATE NOT NULL,
CCC004*      BRAUNTZ_LAST_UPD               TIMESTAMP NOT NULL,
CCC004*      KTY_BRANCHE                    SMALLINT NOT NULL,
CCC004*      KNR_BRANCHE                    SMALLINT NOT NULL,
CCC004*      KTY_UNT_IN_BRA                 SMALLINT NOT NULL,
CCC004*      KNR_UNT_IN_BRA                 SMALLINT NOT NULL,
CCC004*      BRAUNTZ_KUNDE                  CHAR(1) NOT NULL

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0122                           *
      ******************************************************************
       01  VIVB0122-GESAMT.
        05  NI-VIVB0122-TAB.
           10  NI-VIVB0122-ELE     PIC S9(04) COMP   OCCURS 8   TIMES.
        05  NI-VIVB0122            REDEFINES  NI-VIVB0122-TAB.
           10 NI-MA-ST-NR          PIC S9(04) COMP.
           10 NI-BRAUNTZ-DAT-VON   PIC S9(04) COMP.
           10 NI-BRAUNTZ-LAST-UPD  PIC S9(04) COMP.
           10 NI-KTY-BRANCHE       PIC S9(04) COMP.
           10 NI-KNR-BRANCHE       PIC S9(04) COMP.
           10 NI-KTY-UNT-IN-BRA    PIC S9(04) COMP.
           10 NI-KNR-UNT-IN-BRA    PIC S9(04) COMP.
           10 NI-BRAUNTZ-KUNDE     PIC S9(04) COMP.

        05  DCL-VIVB0122.
           10 MA-ST-NR             PIC S9(09) USAGE COMP.
           10 BRAUNTZ-DAT-VON      PIC X(10).
           10 BRAUNTZ-LAST-UPD     PIC X(26).
           10 KTY-BRANCHE          PIC S9(04) USAGE COMP.
           10 KNR-BRANCHE          PIC S9(04) USAGE COMP.
           10 KTY-UNT-IN-BRA       PIC S9(04) USAGE COMP.
           10 KNR-UNT-IN-BRA       PIC S9(04) USAGE COMP.
           10 BRAUNTZ-KUNDE        PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************


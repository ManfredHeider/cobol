      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0073.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/MF/CPY/CVVB0073.CPv  $
      *
      *   Rev 2.7   Nov 30 2001 10:19:34   t13ae34
      *AVB00559 - x-ter check-in
      *
      *   Rev 2.6   Nov 29 2001 18:41:40   t13ae34
      *AVB00559-2.check in
      *
      *   Rev 2.5   Nov 29 2001 18:21:32   t13ae34
      *AVB00559 - VB41 DVAG-Wien Umstellungsdatum
      *hier: source-line too long! 
      *
      *   Rev 2.4   Mar 03 2000 15:23:32   T13AE28
      * 
      *
      *   Rev 2.3   Mar 03 2000 14:07:28   T13AE28
      * 
      *
      *   Rev 2.2   Feb 21 2000 12:37:16   T13AE28
      * 
      *
      *   Rev 2.1   Feb 18 2000 15:34:30   T13AE28
      * 
      *
      *   Rev 2.0   Feb 18 2000 10:15:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************        
      * DCLGEN TABLE(VIVB0073)                                         *        
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0073))              *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVB0073 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      BA_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      BA_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KTY_ANSCHREIB_TYP              SMALLINT NOT NULL,
CCC004*      KNR_ANSCHREIB_TYP              SMALLINT NOT NULL,
CCC004*      BA_DRUCK_DATUM                 TIMESTAMP,
CCC004*      BA_PROGRAMM_ID                 CHAR(8) NOT NULL,
CCC004*      BA_USER_ID                     CHAR(8)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0073                           *
      ******************************************************************
       01  VIVB0073-GESAMT.
        05  NI-VIVB0073-TAB.
           10 NI-VIVB0073-ELE      PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-VIVB0073            REDEFINES   NI-VIVB0073-TAB.
           10 NI-MA-ST-NR             PIC S9(04)  COMP.
           10 NI-BA-DAT-VON           PIC S9(04)  COMP.
           10 NI-BA-LAST-UPDATE       PIC S9(04)  COMP.
           10 NI-KTY-ANSCHREIB-TYP    PIC S9(04)  COMP.
           10 NI-KNR-ANSCHREIB-TYP    PIC S9(04)  COMP.
           10 NI-BA-DRUCK-DATUM       PIC S9(04)  COMP.
           10 NI-BA-PROGRAMM-ID       PIC S9(04)  COMP.
           10 NI-BA-USER-ID           PIC S9(04)  COMP.
        05  DCL-VIVB0073.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 BA-DAT-VON           PIC X(26).                                   
           10 BA-LAST-UPDATE       PIC X(26).                                   
           10 KTY-ANSCHREIB-TYP    PIC S9(4) USAGE COMP.                        
           10 KNR-ANSCHREIB-TYP    PIC S9(4) USAGE COMP.                        
           10 BA-DRUCK-DATUM       PIC X(26).                                   
           10 BA-PROGRAMM-ID       PIC X(08).                                   
           10 BA-USER-ID           PIC X(08).                                   
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *        
      ******************************************************************        

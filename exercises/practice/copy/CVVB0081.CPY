      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0081.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\CVVB0081.CPv  $
      *
      *   Rev 2.4   02 Jul 2001 11:14:02   t13ae13
      *AVB00534 - Einführung PIN
      *
      *   Rev 2.3   25 Jun 2001 07:44:38   t13ae13
      *AVB00534 - Einführung PIN für VB-Portal
      *
      *   Rev 2.2   18 May 2001 15:46:16   t13ae30
      * 
      *
      *   Rev 2.1   09 May 2001 14:42:58   t13ae30
      * 
      *
      *   Rev 2.0   20 Feb 2001 13:22:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVB0081)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0081))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0081 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      MPW                            CHAR (10) NOT NULL,
CCC004*      MMPZ_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      MMPZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      MMPZ_DAT_BIS                   DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVB0081                    *
      ******************************************************************
       01  VIVB0081-GESAMT.
        05 DCL-VIVB0081.
           10 MA-ST-NR                     PIC S9(9) USAGE COMP.
210601*    10 MPW                          PIC S9(4) USAGE COMP.
210601     10 MPW                          PIC X(10).
           10 MMPZ-DAT-VON                 PIC X(26).
           10 MMPZ-LAST-UPDATE             PIC X(26).
           10 MMPZ-DAT-BIS                 PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************

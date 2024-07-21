      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0080.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\CVVB0080.CPv  $
      *
      *   Rev 2.3   02 Jul 2001 11:11:18   t13ae13
      *AVB00534 - Einführung PIN
      *
      *   Rev 2.2   18 May 2001 15:46:12   t13ae30
      *AAS00274 - PIN für VB-Portal
      *
      *
      *   Rev 2.1   09 May 2001 14:42:50   t13ae30
      *AVB00534 - Masterpasswort für VB-Portal
      *
      *   Rev 2.0   20 Feb 2001 13:22:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(T13AES.VIVB0080)                                  *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVB0080))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0080 TABLE
CCC004*    ( MPW                            CHAR (10) NOT NULL,
CCC004*      MPW_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KTY_MPW_STATUS                 SMALLINT NOT NULL,
CCC004*      KNR_MPW_STATUS                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AES.VIVB0078                    *
      ******************************************************************
       01  VIVB0080-GESAMT.
        05 DCL-VIVB0080.
           10 MPW                        PIC X(10).
           10 MPW-LAST-UPDATE            PIC X(26).
           10 KTY-MPW-STATUS             PIC S9(4)  USAGE COMP.
           10 KNR-MPW-STATUS             PIC S9(4)  USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************

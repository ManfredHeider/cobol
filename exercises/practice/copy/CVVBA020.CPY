      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 02 1997 09:26:20   T13EX11
      *AFVB0303  neuer BES-View wg. zusätzlichem
      *          Attribut BES_BEZUGS_NR
      *
      *   Rev 1.0   Apr 02 1997 08:55:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CVVBA020                                     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCHÖLLHAMMER                                 *
      *    DATUM:    20.11.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0303!20.11.96!    ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN VIEW (VIVBA020)                                         *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA020 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      BES_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      KTY_BES_GRND                   SMALLINT NOT NULL,
CCC004*      KNR_BES_GRND                   SMALLINT NOT NULL,
CCC004*      BES_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      BES_INFORMATION                CHAR(49) NOT NULL,
CCC004*      BES_BEZUGS_NR                  INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA020                           *
      ******************************************************************
       01  VIVBA020-GESAMT.
        05  NI-VIVBA020-TAB.
           10  NI-VIVBA020-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-VIVBA020            REDEFINES   NI-VIVBA020-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-BES-DAT-VON       PIC S9(04)  COMP.
           10 NI-KTY-BES-GRND      PIC S9(04)  COMP.
           10 NI-KNR-BES-GRND      PIC S9(04)  COMP.
           10 NI-BES-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-BES-INFORMATION   PIC S9(04)  COMP.
           10 NI-BES-BEZUGS-NR     PIC S9(04)  COMP.
        05  DCL-VIVBA020.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 BES-DAT-VON          PIC X(26).
           10 KTY-BES-GRND         PIC S9(4) USAGE COMP.
           10 KNR-BES-GRND         PIC S9(4) USAGE COMP.
           10 BES-LAST-UPDATE      PIC X(26).
           10 BES-INFORMATION      PIC X(49).
           10 BES-BEZUGS-NR        PIC S9(9) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************

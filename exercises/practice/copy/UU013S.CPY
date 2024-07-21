      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU013S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU013S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:45:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                                                    *
      *            COPY-MEMBER VB-HISTORIEN-DATEI          *
      *                                                    *
      ******************************************************
       01  UU013S.
           05  VSATZ.
               10  VSA             PIC 99.
               10  VKEY.
                   20  VVBNR       PIC 9(7)    COMP-3.
                   20  VSANR       PIC 999     COMP-3.
               10  VFOLGENR        PIC 999     COMP-3.
               10  VHIS            OCCURS 12.
                   20  VVONDAT     PIC 9(6)    COMP-3.
                   20  VBISDAT     PIC 9(6)    COMP-3.
                   20  VDIR        PIC 999     COMP-3.
                   20  VSTRUKST    PIC 999     COMP-3.
                   20  VSTATUS     PIC X.
                   20  VBETRNR     PIC 9(7)    COMP-3.
                   20  VAUSNPROV   PIC 999V9   COMP-3.
                   20  VEPROVKZ    PIC X.
                   20  VVARPROV    PIC 9(5)V9  COMP-3.
           05  CMD0013     REDEFINES   VSATZ.
               10  FILLER          PIC X(8).
               10  VDANZ           PIC 9(7)    COMP-3.
               10  VDLLDAT         PIC 9(6)    COMP-3.
               10  VDENTLDAT       PIC 9(6)    COMP-3.


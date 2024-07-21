      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU074S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU074S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:52:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    -------------------------------------------------------------
      *    COPY-MEMBER UU074S
      *    ------------------
      *
      *    D R U C K A N F O R D E R U N G S - D A T E I
      *    ---------------------------------------------
      *
      *                                    ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 24.11.82
      *    -------------------------------------------------------------
CCC009*    SKIP1
       01  UU074S.
           10  DAF-SA                  PIC 99.
           10  DAF-KEY.
               20 DAF-GES              PIC 99.
               20 DAF-VNR              PIC 9(15)    COMP-3.
           10  DAF-DIRA                PIC 9(3).
           10  DAF-VBNRA               PIC 9(7)     COMP-3.
           10  DAF-INFOA               PIC 9(7)     COMP-3.
           10  DAF-KDNRA               PIC 9(7)     COMP-3.
           10  DAF-DIRN                PIC 9(3).
           10  DAF-VBNRN               PIC 9(7)     COMP-3.
           10  DAF-INFON               PIC 9(7)     COMP-3.
           10  DAF-KDNRN               PIC 9(7)     COMP-3.
           10  DAF-KZ                  PIC 9.
           10  DAF-DYNKZ               PIC 9.
           10  DAF-STATUS              PIC 99.
           10  DAF-STATDAT             PIC 9(7)     COMP-3.
CCC009*    SKIP1
      ****************** ENDE COPY-MEMBER  UU074S **********************


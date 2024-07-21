      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU950S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU950S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:09:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *    -------------------------------------------------------------
      *
      *    UMSETZ-TABELLE FUER SYSTEM-CODE IN NUMMERISCHE WERTE
      *
      *                                    DATUM: 28.05.80
      *                                   AUTHOR: H.SCHORN (AGS)
      *
      *    AUFRUF: TAB-SYSCODE COPY UU950S.
      *
      *    -------------------------------------------------------------
CCC009*    SKIP1
       01  UU950S.
           02  FILLER            PIC X(3)       VALUE 'A01'.
           02  FILLER            PIC X(3)       VALUE 'B02'.
           02  FILLER            PIC X(3)       VALUE 'C03'.
           02  FILLER            PIC X(3)       VALUE 'D04'.
           02  FILLER            PIC X(3)       VALUE 'E05'.
           02  FILLER            PIC X(3)       VALUE 'F06'.
           02  FILLER            PIC X(3)       VALUE 'G07'.
           02  FILLER            PIC X(3)       VALUE 'H08'.
           02  FILLER            PIC X(3)       VALUE 'I09'.
           02  FILLER            PIC X(3)       VALUE 'J10'.
           02  FILLER            PIC X(3)       VALUE 'K11'.
           02  FILLER            PIC X(3)       VALUE 'L12'.
           02  FILLER            PIC X(3)       VALUE 'M13'.
           02  FILLER            PIC X(3)       VALUE 'N14'.
           02  FILLER            PIC X(3)       VALUE 'O15'.
           02  FILLER            PIC X(3)       VALUE 'P16'.
           02  FILLER            PIC X(3)       VALUE 'Q17'.
           02  FILLER            PIC X(3)       VALUE 'R18'.
           02  FILLER            PIC X(3)       VALUE 'S19'.
           02  FILLER            PIC X(3)       VALUE 'T20'.
           02  FILLER            PIC X(3)       VALUE 'U21'.
           02  FILLER            PIC X(3)       VALUE 'V22'.
           02  FILLER            PIC X(3)       VALUE 'W23'.
           02  FILLER            PIC X(3)       VALUE 'X24'.
           02  FILLER            PIC X(3)       VALUE 'Y25'.
           02  FILLER            PIC X(3)       VALUE 'Z26'.
CCC009*    SKIP1
       01  FILLER REDEFINES TAB-SYSCODE.
           02 TAB-SC     OCCURS 26.
              03 TAB-SC-A        PIC X.
              03 TAB-SC-N        PIC 99.


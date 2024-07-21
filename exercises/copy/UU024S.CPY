      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU024S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU024S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:47:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                                                    *
      *                 COPY-MEMBER AG8-SATZ               *
      *                 --------------------               *
      *                                                    *
      *                             ERSTELLT AM 16.JULI 80 *
      *                             VON E. HUEBNER / GEDO  *
      *                                                    *
      *                             AG8 VON 11 AUF 18 ERW. *
      *                             DOBSLAFF 02.03.88      *
      ******************************************************
       01  UU024S.
           05  AG8SATZ.
               10  SKEY.
                   20  AG8GES      PIC 99.
                   20  AG8VNR      PIC 9(15)      COMP-3.
               10  AG8VMNR         PIC 9(7)       COMP-3.
               10  AG8DIR          PIC 999        COMP-3.
               10  AG8NAME         PIC X(40).
               10  AG8STR          PIC X(20).
               10  AG8PLZ          PIC 9(4).
               10  AG8ORT          PIC X(16).
               10  AG8EDAT         PIC 9(6)       COMP-3.
               10  AG8ADAT         PIC 9(6)       COMP-3.
               10  AG8NA           PIC X(20).
               10  AG8PRODS        PIC XX.
               10  AG8VBNR         PIC 9(7)       COMP-3.
               10  AG8ANZ          PIC 99.
               10  AG8             OCCURS 18.
                   20  AG8MANR     PIC 9(7)       COMP-3.
                   20  AG8PRZ      PIC X.
                   20  AG8PROV     PIC 999V9      COMP-3.
               10  AG8SUMME        PIC 9(7)V99    COMP-3.
               10  AG8PRORATA      PIC 9(7)V99    COMP-3.


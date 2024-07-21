      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0177.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0177.CPv  $
      *
      *   Rev 2.1   Sep 06 1999 12:05:34   T13EX48
      *AKI00033
      *Bestandstrennung/Pauschaluebertragung
      *
      *   Rev 2.0   Feb 27 1999 12:53:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:54:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*

       01    DRKI0177-MASTER-PC-SATZ.
           05    DRKI0177-STR-HOECHSTER-VB       PIC 9(07).
           05    DRKI0177-VB-NACHNAME            PIC X(40).
           05    DRKI0177-VB-VORNAME             PIC X(20).
           05    DRKI0177-BT-GEVO                PIC 9(02).
           05    DRKI0177-KTR-SATZ-ERSTELLDAT    PIC X(10).
           05    DRKI0177-TRENN-PC-BESITZER.
            10    DRKI0177-TRENN-PC-PCNR         PIC 9(07).
            10    DRKI0177-TRENN-PC-NACHNAME     PIC X(40).
            10    DRKI0177-TRENN-PC-VORNAME      PIC X(20).
            10    DRKI0177-TRENN-PC-STRASSE      PIC X(40).
            10    DRKI0177-TRENN-PC-LAND         PIC X(04).
            10    DRKI0177-TRENN-PC-PLZ          PIC X(05).
            10    DRKI0177-TRENN-PC-ORT          PIC X(40).
            10    DRKI0177-TRENN-PC-TELEFON      PIC X(14).
           05    DRKI0177-ZUSAMF-PC-BESITZER.
            10    DRKI0177-ZUSAMF-PC-PCNR        PIC 9(07).
            10    DRKI0177-ZUSAMF-PC-NACHNAME    PIC X(40).
            10    DRKI0177-ZUSAMF-PC-VORNAME     PIC X(20).
            10    DRKI0177-ZUSAMF-PC-STRASSE     PIC X(40).
            10    DRKI0177-ZUSAMF-PC-LAND        PIC X(04).
            10    DRKI0177-ZUSAMF-PC-PLZ         PIC X(05).
            10    DRKI0177-ZUSAMF-PC-ORT         PIC X(40).
            10    DRKI0177-ZUSAMF-PC-TELEFON     PIC X(14).

      *-----    ENDE COPY-BOOK DRKI0177               --------

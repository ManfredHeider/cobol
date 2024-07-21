      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0047.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRAN0047.CPv  $
      *
      *   Rev 2.2   13 Sep 2001 17:05:16   t13ex33
      *AAN00320 - EURO-Umstellung - euro-vorbereitet
      *
      *   Rev 2.1   30 Aug 2001 19:51:02   T13EX31
      *AAN00312 - Euro-Umstellung - Datenmigration
      *
      *   Rev 2.0   Feb 27 1999 11:05:40   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:38:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
         01  DRAN0047.
           05 PK-KEY.
              10 PK-DIR      PIC 9(3).
              10 PK-GES      PIC XX.
              10 PK-PROD     PIC XX.
           05 PK-EINH-MM            PIC S9(9)V9   COMP-3.
           05 PK-STUECK-MM          PIC S9(9)     COMP-3.
           05 PK-SUM-MM             PIC S9(13)    COMP-3.
           05 PK-BEI-MM             PIC S9(13)    COMP-3.
           05 PK-EINH-KUM           PIC S9(9)V9   COMP-3.
           05 PK-STUECK-KUM         PIC S9(9)     COMP-3.
           05 PK-SUM-KUM            PIC S9(13)    COMP-3.
           05 PK-BEI-KUM            PIC S9(13)    COMP-3.
           05 PK-DATUM              PIC 9(4).

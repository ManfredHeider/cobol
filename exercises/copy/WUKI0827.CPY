      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   Wuki0827.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/Wuki0827.CPv  $
      *
      *   Rev 2.6   Jun 11 2003 17:07:46   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-ITEMCONTR.
           10 ITEMCONTRID                 PIC X(13).
           10 ICRPCNRVON                  PIC S9(9) COMP.
           10 ICRPCNRNACH                 PIC S9(9) COMP.
           10 ICRABSCHLUSSVER             PIC X(1).
           10 ICREVENTID                  PIC X(12).
           10 NIL-ICREVENTID              PIC X(01).
           10 TIMESTAMP                   PIC X(026).
           10 NIL-TIMESTAMP               PIC X(01).
      *****************************************************************

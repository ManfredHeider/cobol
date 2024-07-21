      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0349.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/MF/CPY/Wuki0349.CPv  $
      *
      *   Rev 2.6   Jul 03 2003 09:35:14   t13ex27
      *FKI00748 - DB-Änderung aus AKI00106
      *
      *   Rev 2.5   25 Jul 2001 10:33:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-VERSKLAUSELBEZ.
           10 VKBVERTRAGSID               PIC X(13).
           10 VKBVERSTARLFDNR             PIC S9(4) COMP.
           10 VKBKLAUSELID                PIC S9(9) COMP.
           10 VKBKLAUSELLFDNR             PIC S9(4) COMP.
           10 VKBKLAUSELPARTGID           PIC S9(4) COMP.
           10 NIL-VKBKLAUSELPARTGID       PIC X(01).
           10 VKBKLAUSEL                  PIC X(10).
           10 NIL-VKBKLAUSEL              PIC X(01).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
      *****************************************************************

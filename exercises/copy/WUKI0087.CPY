      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0087.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0087.CPv  $
      *
      *   Rev 2.2   Feb 16 2004 09:51:28   t13ex27
      *AKI00403 - DB-Änderung für Fix 57
      *
      *   Rev 2.1   Jun 11 2003 13:08:48   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   Jun 05 2003 10:09:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WPHG.
           10 WPHGPERSONENID              PIC X(13).
           10 WPHGPERSONENROLLE           PIC X(1).
           10 WPHGLAUFENDENNUM            PIC S9(4) COMP.
           10 WPHGWERTPAPGESCH            PIC S9(4) COMP.
           10 NIL-WPHGWERTPAPGESCH        PIC X(01).
           10 WPHGUMFANG                  PIC S9(4) COMP.
           10 NIL-WPHGUMFANG              PIC X(01).
           10 WPHGANLEGERTYP              PIC S9(4) COMP.
           10 NIL-WPHGANLEGERTYP          PIC X(01).
           10 WPHGANLAGEDAUER             PIC S9(4) COMP.
           10 NIL-WPHGANLAGEDAUER         PIC X(01).
           10 WPHGMAXRISKLASSE            PIC S9(4) COMP.
           10 NIL-WPHGMAXRISKLASSE        PIC X(01).
           10 WPHGGELDAUSKREDIT           PIC X(1).
      *****************************************************************

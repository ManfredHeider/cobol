      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0093.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0093.CPv  $
      *
      *   Rev 2.1   Jun 11 2003 13:09:12   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.0   Jun 05 2003 10:09:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-ARBEITGEBERADRESSE.
           10 AGAPERSONENID               PIC X(13).
           10 AGAADRESSEBDSLDID           PIC X(3).
           10 NIL-AGAADRESSEBDSLDID       PIC X(01).
           10 AGAADRESSELANDID            PIC X(3).
           10 AGAADRESSEORT               PIC X(30).
           10 NIL-AGAADRESSEORT           PIC X(01).
           10 AGAADRESSEPLZ               PIC X(15).
           10 NIL-AGAADRESSEPLZ           PIC X(01).
           10 AGAADRESSESTHAUSNR          PIC X(45).
           10 NIL-AGAADRESSESTHAUSNR      PIC X(01).
           10 AGAADRESSEISTVTADR          PIC X(1).
           10 LETZTEAENDERUNG             PIC X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 AGAADRESSESUCHSTR           PIC X(45).
           10 NIL-AGAADRESSESUCHSTR       PIC X(01).
           10 AGAADRESSEGEPRUEFT          PIC X(1).
           10 NIL-AGAADRESSEGEPRUEFT      PIC X(01).
      *****************************************************************

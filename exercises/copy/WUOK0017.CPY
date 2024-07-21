      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUOK0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUOK0017.CPv  $
      *
      *   Rev 2.1   02 Jun 2009 09:58:54   t13ex27
      *FKI01156: Wartungsauftrag Abgleich-Commit-Steuerung
      *
      *   Rev 2.0   28 May 2009 11:41:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-HOSTKUNDENUEBER.
           10 HKUID                       PIC  X(13).
           10 HKUTYP                      PIC  S9(4) COMP.
           10 HKUERSTELLT                 PIC  X(026).
           10 HKUERSTELLTVON              PIC  X(8).
           10 HKUSTEUERART                PIC  X(1).
           10 HKUKUNDENID                 PIC  X(13).
           10 NIL-HKUKUNDENID             PIC X(01).
           10 HKUVERTRAGID                PIC  X(13).
           10 HKUVBNRALT                  PIC  S9(9) COMP.
           10 HKUVBNRNEU                  PIC  S9(9) COMP.

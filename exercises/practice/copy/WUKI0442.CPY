      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0442.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0442.CPv  $
      *
      *   Rev 2.1   Mar 11 2010 10:00:36   t13ex14
      *AKI00935: Std.-OZP für Tabelle LEBENPERSONEIGEN 
      *
      *   Rev 2.0   Feb 25 2010 10:45:18   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0442.
           10 LPEVERTRAGSID               PIC  X(13).
           10 LPEPERSROLLEKZ              PIC  X(1).
           10 LPENACHNAME                 PIC  X(30).
           10 NIL-LPENACHNAME             PIC X(01).
           10 LPEVORNAME                  PIC  X(30).
           10 NIL-LPEVORNAME              PIC X(01).
           10 LPEGEBDATUM                 PIC  X(010).
           10 NIL-LPEGEBDATUM             PIC X(01).
           10 LPEADRESSEID                PIC  X(13).
           10 NIL-LPEADRESSEID            PIC X(01).
           10 LPEANREDE-KTY               PIC  S9(4) COMP.
           10 LPEANREDE-KNR               PIC  S9(4) COMP.
           10 LETZTEAENDERUNG             PIC  X(026).
           10 NIL-LETZTEAENDERUNG         PIC X(01).

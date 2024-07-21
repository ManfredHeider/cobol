      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1841.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1841.CPv  $
      *
      *   Rev 2.2   Dec 21 2005 10:33:08   t13ex27
      *FKI00979: DB-Bereinigung für Online-KI - Korrektur VTRBEZIEH
      *UNGSARTKZ
      *
      *   Rev 2.1   Dec 20 2005 12:31:44   t13ex27
      *FKI00979: DB-Bereinigung für Online-KI - Korrektur VTRBEZIEH
      *UNGSARTKZ
      *
      *   Rev 2.0   Nov 30 2005 16:37:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  VTRID-SATZ.
           05  DRKI1841-VTRVERTRAGSID               PIC X(13).
           05  DRKI1841-VTRVERSTARLFDNR             PIC S9(4) COMP.
           05  DRKI1841-VTRPERSONENID               PIC X(13).
           05  DRKI1841-VTRPERSONENROLLEKZ          PIC X(01).
           05  DRKI1841-VTRBEZIEHUNGSARTKZ          PIC X(01).

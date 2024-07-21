      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2260.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2260.CPv  $
      *
      *   Rev 2.1   28 Oct 2008 15:55:36   t13ex27
      *AKI00743: DTA-Kontrolle Teil 3
      *
      *   Rev 2.0   14 Aug 2008 11:28:46   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*

       01  DRKI2260.
           05  GDGDATUM          PIC 9(08).
           05  GDGNAME.
               10 GDGDATEI       PIC X(36).
               10 GDGGEN         PIC X(08).
           05  GDGGESELLSCHAFT   PIC 9(02).
           05  GDGU-NR           PIC 9(02).
           05  GDGDATENID        PIC 9(02).
           05  GDGJOBNAME        PIC X(08).


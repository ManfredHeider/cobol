      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2228.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2228.CPv  $
      *
      *   Rev 2.1   07 Feb 2008 08:15:00   t13ex39
      *AKL00664: Allfinanz:  M A N D A N T E N W E C S E L   DEUBA
      *
      *   Rev 2.0   28 Jan 2008 14:28:12   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2228  Funktion Mandant, Schreibe Vertrag *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    28.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00664!04.07.07!2.0 ! Neuerstellung             ! WHO      *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2228.
           05 DRKI2228-VERTRAGS-ID            PIC  X(13).
           05 DRKI2228-LFDNR                  PIC  S9(4) BINARY.
           05 DRKI2228-MANDANTNRNEU           PIC  S9(9) BINARY.
           05 DRKI2228-MANDANTNRALT           PIC  S9(9) BINARY.
           05 DRKI2228-VNR                    PIC  X(15).
           05 DRKI2228-PGEB-GES               PIC  S9(4) BINARY.
           05 DRKI2228-NEUE-VB-NR             PIC  9(07).

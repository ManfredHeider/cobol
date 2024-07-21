      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0027.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVD0027.CPv  $
      *
      *   Rev 2.2   28 Sep 2007 08:32:32   t13ae23
      *AAN00765: VM-Anteil im EGG für VBI bereitstellen
      *
      *   Rev 2.1   21 Sep 2007 09:32:06   t13ae23
      *AAN00765: VM-Anteil im EGG für VBI bereitstellen
      *
      *   Rev 2.0   20 Sep 2007 10:02:04   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00765!21.09.07! 2.1! NEU                       ! RAHNER   *
      * AAN00765!28.09.07! 2.2! ÄNDERUNG KEY              ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  DRVD0027.
           10  VBI-NSA                          PIC 99.
           10  VBI-NKEY.
               20  VBI-NMANR                    PIC 9(7)     COMP-3.
               20  VBI-NSTUFE                   PIC 9(3).
               20  VBI-NBMANR                   PIC 9(7)     COMP-3.
               20  VBI-NSTATDAT                 PIC 9(7)     COMP-3.
               20  VBI-NSTATSCHL.
                   30 VBI-NSTATSCHL-ERG-KZ      PIC 9(1).
                   30 VBI-NSTATSCHL-PGEB-GES    PIC 9(003)    COMP-3.
                   30 VBI-NSTATSCHL-PROD-PROD-SCHL PIC 9(003) COMP-3.
               20  VBI-KENNZ-DIVE               PIC X(1).
               20  VBI-KENNZ-VM                 PIC X(1).
280907         20  VBI-DATUM-TDS-SATZ           PIC 9(7)      COMP-3.
      *
      *    PRODUKTIONS-ERGEBNIS-SATZ FUER VBI.
           10  VBI-NSATZA.
               20  VBI-NEIGEN.
                    30  VBI-NEAEINH             PIC S9(8)V9     COMP-3.
                    30  VBI-NESTCK              PIC S9(7)       COMP-3.
                    30  VBI-NESUM               PIC S9(11)      COMP-3.
                    30  VBI-NEBEITR             PIC S9(11)V99   COMP-3.
               20  VBI-NGRUPPE.
                    30  VBI-NGRAEINH            PIC S9(8)V9     COMP-3.
                    30  VBI-NGRSTCK             PIC S9(7)       COMP-3.
                    30  VBI-NGRSUM              PIC S9(11)      COMP-3.
                    30  VBI-NGRBEITR            PIC S9(11)V99   COMP-3.
      *
280907*    10 VBI-DATUM-TDS-SATZ                PIC 9(7)        COMP-3.
      *
           10 VBI-FILLER                        PIC X(92).
      *
CCC009*    SKIP2
      ******************************************************************
      *    *** LAENGE 164 BYTES ***
      ******************************************************************


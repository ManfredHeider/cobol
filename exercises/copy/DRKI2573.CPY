      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2573.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2573.CPv  $
      *
      *   Rev 2.2   Jun 05 2012 11:08:20   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.1   Jun 05 2012 09:49:10   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.0   Mar 13 2012 17:00:06   t13ex19
      *Archiverstellung
      ****************************************************************
      *    COPY-MEMBER: DRKI2573  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STOLDT                                          *
      *    DATUM:    13.03.2012                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01124!13.03.12! 01 ! NEUERSTELLUNG             ! BST      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2573.
          05 PI-DRKI2573-PROGRAMM               PIC X(008).
          05 PI-DRKI2573-UFKT                   PIC 9(002).
          05 PI-DRKI2573-DATEN.
            10 PI-DRKI2573-VEZVERTRAGSID        PIC X(13).


       01 PO-DRKI2573.
          05 PO-DRKI2573-DATEN.
            10 PO-DRKI2573-VEZVETLFDNR          PIC S9(4) COMP.
            10 PO-DRKI2573-VEZPRODUKTID         PIC S9(4) COMP.
            10 PO-DRKI2573-VEZNJBEITRAG         PIC S9(8)V9(2) COMP-3.
            10 PO-DRKI2573-VEZBBEITRAGZAHLW     PIC S9(8)V9(2) COMP-3.
            10 PO-DRKI2573-VEZBJBEITRAG         PIC S9(8)V9(2) COMP-3.

          05 PO-DRKI2573-RC                       PIC  X(003).
             88 PO-DRKI2573-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2573-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2573-RC-LER                       VALUE 'LER'.
      *
      *---------------------------------------------------------------*

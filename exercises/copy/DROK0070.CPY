      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0070.CPv  $
      *
      *   Rev 2.1   Apr 04 2007 15:56:00   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Adressaenderungen
      *
      *   Rev 2.0   Feb 07 2007 15:28:54   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PI/PO fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!07.02.07! 1.0! NEU                       !H. Sebbach*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0070.
           05  PI-DROK0070-PROGRAMM            PIC  X(008).
           05  PI-DROK0070-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0070-DATEN.
             10 PI-DROK0070-HADTICKETID        PIC S9(18)V COMP-3.
      ***
       01 PO-DROK0070.
          05 PO-DROK0070-RC                    PIC  X(003).
             88 PO-DROK0070-RC-OK                        VALUE 'OK '.
             88 PO-DROK0070-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0070-RC-EOF                       VALUE 'EOF'.
          05 PO-DROK0070-DATEN.
           10 PO-DROK0070-HADPERSONENID        PIC X(13).
           10 PO-DROK0070-HADVERTRAGSID        PIC X(13).
           10 PO-DROK0070-HADADRESSEALTID      PIC X(13).
           10 PO-DROK0070-HADLFDNR             PIC S9(04) COMP.
           10 PO-DROK0070-HADLANDID            PIC X(03).
           10 PO-DROK0070-HADORT               PIC X(30).
           10 PO-DROK0070-HADPLZ               PIC X(15).
           10 PO-DROK0070-HADSTRHAUSNUM        PIC X(45).
           10 PO-DROK0070-HADORTSTEIL          PIC X(30).
           10 PO-DROK0070-HADVBID              PIC X(13).
           10 PO-DROK0070-HADAENDGRUNDID       PIC S9(04) COMP.
           10 PO-DROK0070-HADTICKETID          PIC S9(18) COMP-3.
           10 PO-DROK0070-HADDTAERLEDIGT       PIC X(01).
           10 PO-DROK0070-HADDTAERLDATUM       PIC X(10).
           10 PO-DROK0070-HADGEAENDERTAM       PIC X(10).
           10 PO-DROK0070-HADSTATUS            PIC S9(04) COMP.
           10 PO-DROK0070-LETZTEAENDERUNG      PIC X(10).
      *
      ******** Ende Copy-Member DROK0070 *****************************
      *

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0082.CPv  $
      *
      *   Rev 2.1   Apr 04 2007 15:56:14   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Adressaenderungen
      *
      *   Rev 2.0   Apr 03 2007 12:40:56   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PI/PO fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      * (Lesen nicht vom Partner zurueckgemeldeter Aenderungen(Status 2)
      *  zu VERTRAGSID und PERSONENID)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!03.03.07! 1.0! NEU                       !H. Sebbach*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0082.
           05  PI-DROK0082-PROGRAMM            PIC  X(008).
           05  PI-DROK0082-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0082-DATEN.
             10 PI-DROK0082-HADPERSONENID        PIC X(13).
             10 PI-DROK0082-HADVERTRAGSID        PIC X(13).
             10 PI-DROK0082-HADADRESSEALTID      PIC X(13).
      ***
       01 PO-DROK0082.
           05 PO-DROK0082-RC                    PIC  X(003).
             88 PO-DROK0082-RC-OK                        VALUE 'OK '.
             88 PO-DROK0082-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0082-RC-EOF                       VALUE 'EOF'.
           05 PO-DROK0082-DATEN.
             10 PO-DROK0082-HADLFDNR             PIC S9(04) COMP.
             10 PO-DROK0082-HADLANDID            PIC X(03).
             10 PO-DROK0082-HADORT               PIC X(30).
             10 PO-DROK0082-HADPLZ               PIC X(15).
             10 PO-DROK0082-HADSTRHAUSNUM        PIC X(45).
             10 PO-DROK0082-HADORTSTEIL          PIC X(30).
             10 PO-DROK0082-HADVBID              PIC X(13).
             10 PO-DROK0082-HADAENDGRUNDID       PIC S9(04) COMP.
             10 PO-DROK0082-HADTICKETID          PIC S9(18) COMP-3.
             10 PO-DROK0082-HADDTAERLEDIGT       PIC X(01).
             10 PO-DROK0082-HADDTAERLDATUM       PIC X(10).
             10 PO-DROK0082-HADGEAENDERTAM       PIC X(10).
             10 PO-DROK0082-HADSTATUS            PIC S9(04) COMP.
             10 PO-DROK0082-LETZTEAENDERUNG      PIC X(10).
      *
      ******** Ende Copy-Member DROK0082 *****************************
      *

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0084.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0084.CPv  $
      *
      *   Rev 2.2   05 Apr 2007 17:16:00   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB
      *
      *   Rev 2.1   05 Apr 2007 15:48:34   t13ex27
      *AKI00557: Ticketverfahren AD/ID und Kunde-VB-Beziehung und A
      *dressänderung
      *
      *   Rev 2.0   04 Apr 2007 17:33:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PI/PO fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      * (Lesen aller Zeilen einer Person)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00557!05.04.07! 1.0! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0084.
           05  PI-DROK0084-PROGRAMM            PIC  X(008).
           05  PI-DROK0084-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0084-DATEN.
             10 PI-DROK0084-HADPERSONENID       PIC X(013).
             10 PI-DROK0084-HADVERTRAGSID-VON   PIC  X(013).
             10 PI-DROK0084-HADVERTRAGSID-BIS   PIC  X(013).

      ***
       01 PO-DROK0084.
           05 PO-DROK0084-RC                    PIC  X(003).
             88 PO-DROK0084-RC-OK                        VALUE 'OK '.
             88 PO-DROK0084-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0084-RC-EOF                       VALUE 'EOF'.
           05 PO-DROK0084-DATEN.
             10 PO-DROK0084-HADLFDNR             PIC S9(04) COMP.
             10 PO-DROK0084-HADLANDID            PIC X(03).
             10 PO-DROK0084-HADORT               PIC X(30).
             10 PO-DROK0084-HADPLZ               PIC X(15).
             10 PO-DROK0084-HADSTRHAUSNUM        PIC X(45).
             10 PO-DROK0084-HADORTSTEIL          PIC X(30).
             10 PO-DROK0084-HADVBID              PIC X(13).
             10 PO-DROK0084-HADAENDGRUNDID       PIC S9(04) COMP.
             10 PO-DROK0084-HADTICKETID          PIC S9(18) COMP-3.
             10 PO-DROK0084-HADDTAERLEDIGT       PIC X(01).
             10 PO-DROK0084-HADDTAERLDATUM       PIC X(10).
             10 PO-DROK0084-HADGEAENDERTAM       PIC X(10).
             10 PO-DROK0084-HADSTATUS            PIC S9(04) COMP.
             10 PO-DROK0084-LETZTEAENDERUNG      PIC X(10).
      *
      ******** Ende Copy-Member DROK0084 *****************************
      *

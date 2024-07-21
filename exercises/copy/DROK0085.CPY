      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0085.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0085.CPv  $
      *
      *   Rev 2.1   Jan 23 2014 10:48:42   t13ex27
      *AKI01063: Wartungsauftrag-Ticketverarbeitung-Kundenlöschung
      *
      *   Rev 2.0   04 Apr 2007 15:39:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PI/PO fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      * (Lesen aller Zeilen eines Vertrages)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01063!03.12.13! 1.0! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0085.
           05  PI-DROK0085-PROGRAMM            PIC  X(008).
           05  PI-DROK0085-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0085-DATEN.
             10 PI-DROK0085-HADVERTRAGSID       PIC X(013).

      ***
       01 PO-DROK0085.
           05 PO-DROK0085-RC                    PIC  X(003).
             88 PO-DROK0085-RC-OK                        VALUE 'OK '.
             88 PO-DROK0085-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0085-RC-EOF                       VALUE 'EOF'.
           05 PO-DROK0085-DATEN.
             10 PO-DROK0085-HADPERSONENID        PIC X(13).
             10 PO-DROK0085-HADADRESSEALTID      PIC X(13).
             10 PO-DROK0085-HADLFDNR             PIC S9(4) COMP.
             10 PO-DROK0085-HADLANDID            PIC X(3).
             10 PO-DROK0085-HADORT               PIC X(30).
             10 PO-DROK0085-HADPLZ               PIC X(15).
             10 PO-DROK0085-HADSTRHAUSNUM        PIC X(45).
             10 PO-DROK0085-HADORTSTEIL          PIC X(30).
             10 PO-DROK0085-HADVBID              PIC X(13).
             10 PO-DROK0085-HADAENDGRUNDID       PIC S9(4) COMP.
             10 PO-DROK0085-HADTICKETID          PIC S9(18)V COMP-3.
             10 PO-DROK0085-HADDTAERLEDIGT       PIC X(1).
             10 PO-DROK0085-HADDTAERLDATUM       PIC X(10).
             10 PO-DROK0085-HADGEAENDERTAM       PIC X(10).
             10 PO-DROK0085-HADSTATUS            PIC S9(4) COMP.
             10 PO-DROK0085-LETZTEAENDERUNG      PIC X(26).
      *
      ******** Ende Copy-Member DROK0085 *****************************

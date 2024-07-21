      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.05.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0088.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0088.CPv  $
      *
      *   Rev 2.1   May 14 2007 10:58:26   t13ex48
      *AKI00557: Ticketverfahren OKI: RUECK-DTA Teil 2
      *
      *   Rev 2.0   May 03 2007 10:29:34   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PI/PO fuer LIST-OZP ZXOK0012 (HISTADRESSAENDERG)
      * (Lesen bereits vom RDTA verarbeiteter Aenderungen(Status 2, 5)
      *  zu einer VERTRAGSID)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
250407* AKI00557!25.04.07! 1.0! NEU                       !H. Sebbach*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DROK0088.
           05  PI-DROK0088-PROGRAMM            PIC  X(008).
           05  PI-DROK0088-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DROK0088-DATEN.
             10 PI-DROK0088-HADVERTRAGSID        PIC X(13).
      ***
       01 PO-DROK0088.
           05 PO-DROK0088-RC                    PIC  X(003).
             88 PO-DROK0088-RC-OK                        VALUE 'OK '.
             88 PO-DROK0088-RC-LEER                      VALUE 'LER'.
             88 PO-DROK0088-RC-EOF                       VALUE 'EOF'.
           05 PO-DROK0088-DATEN.
             10 PO-DROK0088-HADPERSONENID        PIC X(13).
             10 PO-DROK0088-HADVERTRAGSID        PIC X(13).
             10 PO-DROK0088-HADADRESSEALTID      PIC X(13).
             10 PO-DROK0088-HADLFDNR             PIC S9(04) COMP.
             10 PO-DROK0088-HADLANDID            PIC X(03).
             10 PO-DROK0088-HADORT               PIC X(30).
             10 PO-DROK0088-HADPLZ               PIC X(15).
             10 PO-DROK0088-HADSTRHAUSNUM        PIC X(45).
             10 PO-DROK0088-HADORTSTEIL          PIC X(30).
             10 PO-DROK0088-HADVBID              PIC X(13).
             10 PO-DROK0088-HADAENDGRUNDID       PIC S9(04) COMP.
             10 PO-DROK0088-HADTICKETID          PIC S9(18) COMP-3.
             10 PO-DROK0088-HADDTAERLEDIGT       PIC X(01).
             10 PO-DROK0088-HADDTAERLDATUM       PIC X(10).
             10 PO-DROK0088-HADGEAENDERTAM       PIC X(10).
             10 PO-DROK0088-HADSTATUS            PIC S9(04) COMP.
             10 PO-DROK0088-LETZTEAENDERUNG      PIC X(10).
      *
      ******** Ende Copy-Member DROK0088 *****************************
      *

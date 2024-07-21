      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0009.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRVD0009.CPv  $
      *
      *   Rev 2.7   Jun 05 2012 11:08:08   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.6   Jun 05 2012 09:48:10   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.5   Apr 27 2010 15:10:44   t13ex14
      *FKI01288: VBI-SST - STVD0060 - VL-Kennzeichen bei Depots kor
      *rigieren
      *
      *   Rev 2.4   Jan 11 2008 18:27:50   t13ex51
      *AKL00664: Allfinanz: VBI-Schnittstelle
      *
      *   Rev 2.3   Jan 19 2005 08:24:22   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.2   Dec 09 2004 09:43:04   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.1   Nov 23 2004 16:48:30   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 28 2004 15:09:38   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVD0009 Produkte und Beiträge aus           *
      *                          KI-Tabellen ermitteln               *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00422!09.07.02! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      * -------------------------------------------------------------*
      * AKL00664!11.01.08! 02 ! Anpassen an AMV-FK-PK     ! HRO      *
      * -------------------------------------------------------------*
130410* FKI01288!13.04.10! 03 ! neues Feld GHPDEPOTART    ! WMI      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI01124!28.02.12! 04 ! neue Felder GHPSCHLIESSDA-! BST      *
      *         !        !    ! TUM, BEITRAG-GEMZW        !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVD0009                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVD0009.
          05 PI-DRVD0009-DATEN.
             10 PI-DRVD0009-PARTGESELLSCHAFTID   PIC  S9(4) COMP.
             10 PI-DRVD0009-VERVERTRAGSID        PIC  X(013).
             10 PI-DRVD0009-VERVERTRAGSNUMMER    PIC  X(015).
             10 PI-DRVD0009-VERGESAMTBEITRAG     PIC  S9(9)V9(2) COMP-3.
             10 PI-DRVD0009-VERGESBEITRWKZ       PIC  X(001).
             10 PI-DRVD0009-VERZAHLUNGSWEISEID   PIC  S9(4) COMP.
             10 PI-DRVD0009-TAGESDATUM-EUR       PIC  X(010).
             10 PI-DRVD0009-Folgeaufruf          PIC  X(001).
             10 PI-DRVD0009-AMV-READ-FIRST       PIC  X(001).
110108       10 PI-DRVD0009-AMV-FK-PK            PIC  X(001).

      *
       01 PO-DRVD0009.
          05 PO-DRVD0009-DATEN.
             10 PO-DRVD0009-PRODUKTID            PIC 9(04).
             10 PO-DRVD0009-VERS-Summe           PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-VERS-Summe-Dyn       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-Brutto       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-Netto1       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-Netto2       PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-GEMZW        PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-D-Brutto     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-D-Netto1     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-BEITRAG-D-Netto2     PIC  S9(9)V9(2) COMP-3.
             10 PO-DRVD0009-GHPEIGENVERMITTELT   PIC  X(001).
             10 PO-DRVD0009-GHPOFFNUNGSDATUM     PIC  X(010).
             10 PO-DRVD0009-GHPSCHLIESSDATUM     PIC  X(010).
             10 PO-DRVD0009-PRODLFDNR            PIC S9(4) COMP.
             10 PO-DRVD0009-GHPABSCHLUSSVBNR     PIC S9(9) COMP.
             10 PO-DRVD0009-WKZ                  PIC  X(001).
             10 PO-DRVD0009-Folgeaufruf          PIC  X(001).
             10 PO-DRVD0009-AMV-READ-FIRST       PIC  X(001).
110108       10 PO-DRVD0009-AMV-FK-PK            PIC  X(001).
             10 PO-DRVD0009-Satz-ausgeben        PIC  X(001).
130410       10 PO-DRVD0009-GHPDEPOTART          PIC  X(001).

             10 PO-DRVD0009-Fehler-Nummer     PIC  9(4).
             10 PO-DRVD0009-Fehler-Feldname   PIC X(015).
             10 PO-DRVD0009-Fehler-Vertrag    PIC X(15) JUSTIFIED RIGHT.
             10 PO-DRVD0009-Fehler-Var-1      PIC X(15).
             10 PO-DRVD0009-Fehler-Var-2      PIC X(15).
             10 PO-DRVD0009-Fehler-Var-3      PIC X(15).
             10 PO-DRVD0009-Fehler-Var-4      PIC X(15).
             10 PO-DRVD0009-Fehler-Var-5      PIC X(15).



          05 PO-DRVD0009-RC                      PIC  X(003).
             88 PO-DRVD0009-RC-OK                         VALUE 'OK '.
             88 PO-DRVD0009-RC-NOK                        VALUE 'NOK'.
             88 PO-DRVD0009-RC-LEER                       VALUE 'LER'.

      ********* Ende Copy-Member DRVD0009 ****************************



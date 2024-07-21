      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2637.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2637.CPv  $
      *
      *   Rev 2.4   May 07 2018 18:16:28   t13ex27
      *AKI01573: FW-Zentrales-DTA-Kontrolle und VBNR-Prüfung
      *
      *   Rev 2.3   Feb 21 2018 16:23:00   t13ex27
      *AKI01573: FW-Zentrales-DTA-Kontrolle
      *
      *   Rev 2.2   Mar 27 2017 09:29:42   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.1   Oct 25 2012 15:16:42   t13ex14
      *AKI01158: MDTA-Kontrolle => Plausiprüfungen im  DTA-Vorsatz
      *
      *   Rev 2.0   Jul 10 2012 19:12:12   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * AKI01158!10.07.12! 1.0! NEU                       !H.Schlenther*
      * ---------------------------------------------------------------*
      * FKI01544!24.03.17!    ! FVD entfernt              !  HHU       *
      * AKI01573!09.02.18!2.3 ! Bezeichnungenen in PO-Ber.!  IFR       *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01 PI-DRKI2637.
          05 PI-DRKI2637-KEY.
            10 PI-DRKI2637-UFKT              PIC  X(1).
      *     '1' = prüfen DTAPARTNERDATUM
      *     '2' = prüfen DOPPELLIEFERUNG und ob DTAPARTNERDATUM
      *           in der Vergangenheit liegt
 
      * Zugriffskey fuer die Tabelle DTAKONTROLLE (TBAT0040)
            10 PI-DRKI2637-DTAMANDANTNR      PIC S9(9) COMP.
 
            10 PI-DRKI2637-DTAPARTGESID      PIC S9(4) COMP.
      *        Gesellschafts-ID Bsp. 70 = BADENIA
      *                              50 = CENTRAL usw.
 
            10 PI-DRKI2637-DTAGRUPPE         PIC S9(4) COMP.
      * Gruppe-Zusammenfassung für Dateinummern
 
            10 PI-DRKI2637-DTAPARTNERDATUM   PIC X(08).
      *        In der Form JJJJMMTT
      *
            10 PI-DRKI2637-ERSTER-DTA-DATUM  PIC X(08).
      *        In der Form JJJJMMTT
      *
          05 PI-DATEN.
             10 PI-DRKI2637-DTA-ERSTER-DS        PIC X(1000).
             10 PI-DRKI2637-ANZ-DTA-SAETZE       PIC 9(009).
 
             10 PI-DRKI2637-DTAGDGSTATUS         PIC X(1).
             10 PI-DRKI2637-DTADATEI             PIC X(36).
             10 PI-DRKI2637-DTADATEINUMMER       PIC S9(4) COMP.
      * Dateinummer des ersten DTA (z. B. Vertrags-DTA)
             10 PI-DRKI2637-DTAJOBNAME           PIC X(8).
             10 PI-DRKI2637-PRUEF-DOPPELLFG      PIC X(01).
      *      J=JA; N=NEIN; Blank nicht erlaubt
      *
       01  PO-DRKI2637.
           05  PO-DRKI2637-RC               PIC  X(03).
               88 PO-DRKI2637-RC-OK              VALUE 'OK '.
               88 PO-DRKI2637-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2637-RC-DLF             VALUE 'DLF'.
               88 PO-DRKI2637-RC-ALF             VALUE 'ALF'.

090218     05  PO-DRKI2637-DATEN.
090218       10 PO-DRKI2637-BEZEICHNUNG          PIC  X(0100).
090218       10 PO-DRKI2637-BEZEICHNUNG-KURZ     PIC  X(0020).
090218       10 PO-DRKI2637-KNR-ALPHA            PIC  X(0015).
 
      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2637-FEHLERSATZ.
               10 PO-DRKI2637-FUNKTION           PIC  X(10).
               10 PO-DRKI2637-TABELLE            PIC  X(20).
               10 PO-DRKI2637-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2637-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2637-KEY.
                  15 PO-DRKI2637-GES             PIC  9(002).
                  15 PO-DRKI2637-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER *****************************************



      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08/05/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0000.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/WUKI0000.CPv  $
      *
      *   Rev 2.5   Nov 04 2014 08:56:08   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.4   Oct 29 2014 11:00:16   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.3   May 16 2014 13:09:30   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *ZTKI0000
      *
      *   Rev 2.2   May 15 2014 11:55:20   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.1   Jul 20 2012 09:55:36   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 8 2012 11:12:19   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-KUNDENLOESCHUNG.
           10 KULPERSONENID                 PIC  X(0013).
           10 KULVERTRAGSID                 PIC  X(0013).
           10 KULTICKETID                   PIC S9(18)V COMP-3.
           10 KULVERTRAGNUMMER              PIC  X(0015).
           10 NIL-KULVERTRAGNUMMER          PIC  X(01).
           10 KULPARTGESELLSCH              PIC S9(4) COMP.
           10 NIL-KULPARTGESELLSCH          PIC  X(01).
           10 KULMANDANTNR                  PIC S9(9) COMP.
           10 KULKNDNUMMER                  PIC S9(9) COMP.
           10 KULSTATUS                     PIC  X(0001).
           10 LETZTEAENDERUNG               PIC  X(026).
           10 KULERSTELLTTST                PIC  X(026).
           10 KULVBNR                       PIC S9(9) COMP.

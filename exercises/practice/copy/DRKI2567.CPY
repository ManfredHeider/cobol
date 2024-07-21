      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2567.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2567.CPv  $
      *
      *   Rev 2.4   Sep 26 2013 11:56:20   t13ex20
      *AKI01255: SEPA: Anpassung 'Verträge mit gleichem Konto' (Leb
      *en)
      *
      *   Rev 2.3   Sep 20 2012 12:18:22   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.2   Jul 13 2012 10:02:38   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.1   Jul 12 2012 16:03:40   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.0   Feb 22 2012 17:36:24   t13ex20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2567                                     *
      *                 FUNKTION AUSGABESTRUKTUR FUER UNLOAD STVD0113*
      *    -------------------------------------------------------   *
      *    ERSTELLT: U. ZIMMERMANN                                   *
      *    DATUM:    22.02.2012                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!12.02.12! 01 ! NEUERSTELLUNG             ! UZI      *
      * -------------------------------------------------------------*
      * AKI01255!30.08.13! 01 ! Erweiterung mit IBAN      ! UZI      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  AUSGABE-SATZ.
           05  A-DATUM                     PIC  X(010).
           05  A-MITARBEITERNR             PIC S9(009) COMP.
           05  A-NI-MITARBEITERNR          PIC  X(001).
           05  A-KUNDENNUMMER              PIC S9(009) COMP.
           05  A-KUNDEN-ID                 PIC  X(013).
           05  A-KND-NAME                  PIC  X(030).
           05  A-NI-KND-NAME               PIC  X(001).
           05  A-KND-VORNAME               PIC  X(030).
           05  A-NI-KND-VORNAME            PIC  X(001).
           05  A-PART-GES                  PIC S9(004) COMP.
           05  A-VERTRAGSNUMMER            PIC  X(015).
           05  A-NI-VERTRAGSNUMMER         PIC  X(001).
           05  A-VERTRAGSID                PIC  X(013).
           05  A-VERTRAGSBEGINN            PIC  X(010).
           05  A-NI-VERTRAGSBEGINN         PIC  X(001).
           05  A-KTO-NAME                  PIC  X(030).
           05  A-NI-KTO-NAME               PIC  X(001).
           05  A-KTO-VORNAME               PIC  X(030).
           05  A-NI-KTO-VORNAME            PIC  X(001).
           05  A-KONTONUMMER               PIC  X(015).
           05  A-BLZ                       PIC S9(009) COMP.
           05  A-NI-BLZ                    PIC  X(001).
           05  A-IBAN                      PIC  X(034).



      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2566.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2566.CPv  $
      *
      *   Rev 2.6   Jan 21 2015 13:14:56   t13ex23
      *FVD00087: Programmfehler in PMKI0939
      *
      *   Rev 2.5   Jan 16 2015 10:52:48   t13ex23
      *FVD00087: Programmfehler in PMKI0939
      *
      *   Rev 2.4   Sep 26 2013 11:56:16   t13ex20
      *AKI01255: SEPA: Anpassung 'Verträge mit gleichem Konto' (Leb
      *en)
      *
      *   Rev 2.3   Sep 20 2012 12:18:16   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.2   Jul 13 2012 10:02:32   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.1   Jul 12 2012 16:03:36   t13ex20
      *AKI00886: Host - neues Design Kundenaktualisierung - PMKI093
      *9 UZI
      *
      *   Rev 2.0   Feb 22 2012 17:00:54   t13ex20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2566                                     *
      *                 FUNKTION EINGABESTRUKTUR FUER UNLOAD STVD0113*
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
      * -------------------------------------------------------------*
130115* FVD00087!13.01.15! 01 ! Loeschen der BLZ und      ! MKE      *
      *         !        !    ! Kto-Verbindung            !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  EINGABE-SATZ.
           05  E-MITARBEITERNR             PIC S9(009) COMP.
           05  E-NI-MITARBEITERNR          PIC  X(001).
           05  E-PART-GES                  PIC S9(004) COMP.
           05  E-VERTRAGSNUMMER            PIC  X(015).
           05  E-NI-VERTRAGSNUMMER         PIC  X(001).
           05  E-VERTRAGSID                PIC  X(013).
           05  E-VERTRAGSBEGINN            PIC  X(010).
           05  E-NI-VERTRAGSBEGINN         PIC  X(001).
           05  E-KTO-NAME                  PIC  X(030).
           05  E-KTO-VORNAME               PIC  X(030).
           05  E-NI-KTO-VORNAME            PIC  X(001).
130115*    05  E-KONTONUMMER               PIC  X(018).
130115*    05  E-BLZ                       PIC S9(009) COMP.
130115*    05  E-NI-BLZ                    PIC  X(001).
130115     05  E-IBAN                      PIC  X(034).
           05  E-NI-IBAN                   PIC  X(001).
 


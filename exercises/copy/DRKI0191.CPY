      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0191.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRKI0191.CPv  $
      *
      *   Rev 2.1   Nov 28 2002 08:40:06   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.0   Feb 27 1999 12:53:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:52:06   T13AE38
      *AKI00014 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:56:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG BNK (DTA)   KOMPLETTBESTAND*
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *09.06.98!NEUERSTELLUNG                           !S.Jost   ! 01 *
      *AKI00001!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *13.09.02! ISIN - Einführung                      !P. Pohl  !    *
      *AKI00294!                                        !         !    *
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI0191.
      *
               05  KOMPL-BNK-SA             PIC  X(001).
      *            SATZART        = 'C'
               05  KOMPL-BNK-GES            PIC  9(002).
      *            GESELLSCHAFT
      *
               05  KOMPL-BNK-KEY.
                   25 KOMPL-BNK-VNR            PIC  9(015).
      *               VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
190902             25 KOMPL-BNK-ISIN-NR        PIC  X(012).
190902*               WERTPAPIERKENN-NUMMER
190902*            25 KOMPL-BNK-WP-KENN-NR     PIC  X(06).
190902*               WERTPAPIERKENN-NUMMER
      *
               05  KOMPL-BNK-ANTEILE        PIC  9(06)V999.
      *            ANTEILE (BESTAND STÜCK)
               05  KOMPL-BNK-BUCH-DAT       PIC  9(08).
      *            BUCHUNGSDATUM
               05  KOMPL-BNK-BUCH-BETRAG    PIC  9(07)V99.
      *            BETRAG LETZTE BUCHUNG
               05  KOMPL-BNK-BUCH-ST-BETRAG PIC  9(07)V99.
      *            STEUERBETRAG LETZTE BUCHUNG
               05  KOMPL-BNK-BUCH-SCHL      PIC  X(02).
      *            BUCHUNGSSCHLÜSSEL
               05  KOMPL-BNK-EINZ-KUM       PIC  9(13).
      *            KUMULIERTE EINZAHLUNGEN
               05  KOMPL-BNK-AUSZ-KUM       PIC  9(13).
      *            KUMULIERTE AUSZAHLUNGEN
               05  KOMPL-BNK-WERT-GES       PIC  9(03)V99.
      *            WERTENTWICKLUNG GESAMT
               05  KOMPL-BNK-WERT-JAHR      PIC  9(03)V99.
      *            WERTENTWICKLUNG LFD. JAHR
               05  KOMPL-BNK-FREI-VERBR     PIC  9(05)V99.
      *            VERBRAUCHTER FREISTELLUNGSBETRAG
               05  KOMPL-BNK-ZINS-AB-ST     PIC  9(06)V99.
      *            ZINSABSCHLAGSTEUER
190902         05  KOMPL-BNK-ANTEILE-AKT    PIC  9(06)V999.
190902*            WERTPAPIERKENN-NUMMER
190902         05  KOMPL-BNK-WP-KENN-NR     PIC  X(06).
190902*            WERTPAPIERKENN-NUMMER
190902*        05  FILLER                   PIC  X(18).
190902         05  FILLER                   PIC  X(100).
      *            RESERVE


      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0184.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0184.CPv  $
      *
      *   Rev 2.2   Nov 28 2002 08:39:54   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.1   Apr 22 1999 13:33:28   T13EX39
      *FKI00018 Feld für  Kontoart
      *
      *   Rev 1.1   08 Feb 1999 17:51:38   T13AE38
      *AKI00014 - Neuerstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 0 1 8 4  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND DEPOTERöFF- *
      *    NUNGEN UND KUNDENDATENäNDERUNGEN BANKEN                     *
      *                                                                *
      *    LAENGE =  130  BYTES      ERSTELLT  AM 04.06.98 / S.JOST    *
190902*    Länge :   250  Bytes
      *                                                                *
      *                      DATEILÄNGE = 130                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *  DATEINAME = P130006.KI012800                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *07.04.98!NEUERSTELLUNG                           !S.JOST   ! 01 *
      *AKI00001!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *14.04.99!Feld für Kontoart (DE, VL) definieren.  !V.D.GOLTZ! 02 *
      *FKI00018!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *19.09.02! ISIN - Einführung                      !P. Pohl  !    *
      *AKI00294!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      ******************************************************************
      *
       01  DRKI0184.
               05  BNK-SA                       PIC  X(001).
      *            SATZART
               05  BNK-GES                      PIC  9(002).
      *            GESELLSCHAFT   =
               05  BNK-VNR                      PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VERARB-KZ                PIC  X(001).
      *            Verarbeitungskennzeichen
      *                I = Insert
      *                U = Update
190902         05  BNK-ISIN-NR                  PIC  X(012).
190902*            WERTPAPIERKENN-NUMMER
190902         05  BNK-AEND-ISIN-NR-KZ          PIC  X(001).
190902*            J = ISIN-NR ÄNDERUNG
               05  BNK-WP-KENN-NR               PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               05  BNK-AEND-WP-KENN-NR-KZ       PIC  X(001).
      *            J = WERTPAPIERKENNNR ÄNDERUNG
               05  BNK-ANTEILE                  PIC  9(006)V999.
      *            ANTEILE (BESTAND STÜCK)
               05  BNK-AEND-ANTEILE-KZ          PIC  X(001).
      *            J = ANTEILE ÄNDERUNG
               05  BNK-BUCH-DAT                 PIC  9(008).
      *            BUCHUNGSDATUM
               05  BNK-AEND-BUCH-DAT-KZ         PIC  X(001).
      *            J = BUCHUNGSDATUM ÄNDERUNG
               05  BNK-BUCH-BETRAG              PIC  9(007)V99.
      *            BETRAG LETZTE BUCHUNG
               05  BNK-AEND-BUCH-BETRAG-KZ      PIC  X(001).
      *            J = BUCHUNGSBETRAG ÄNDERUNG
               05  BNK-BUCH-ST-BETRAG           PIC  9(007)V99.
      *            STEUERBETRAG LETZTE BUCHUNG
               05  BNK-AEND-BUCH-ST-BETRAG-KZ   PIC  X(001).
      *            J = STEUERBETRAG   ÄNDERUNG
               05  BNK-BUCH-SCHL                PIC  X(002).
      *            BUCHUNGSSCHLÜSSEL
               05  BNK-AEND-BUCH-SCHL-KZ        PIC  X(001).
      *            J = BUCHUNGSSCHLüSSEL äNDERUNG
               05  BNK-EINZ-KUM                 PIC  9(013).
      *            KUMULIERTE EINZAHLUNGEN
               05  BNK-AEND-EINZ-KUM-KZ         PIC  X(001).
      *            J = KUMULIERTE EINZAHLUNGEN äNDERUNG
               05  BNK-AUSZ-KUM                 PIC  9(013).
      *            KUMULIERTE AUSZAHLUNGEN
               05  BNK-AEND-AUSZ-KUM-KZ         PIC  X(001).
      *            J = KUMULIERTE AUSZAHLUNGEN äNDERUNG
               05  BNK-WERT-GES                 PIC  9(003)V99.
      *            WERTENTWICKLUNG GESAMT
               05  BNK-AEND-WERT-GES-KZ         PIC  X(001).
      *            J = WERTENTWICKLUNG GESAMT  äNDERUNG
               05  BNK-WERT-JAHR                PIC  9(003)V99.
      *            WERTENTWICKLUNG LFD. JAHR
               05  BNK-AEND-WERT-JAHR-KZ        PIC  X(001).
      *            J = WERTENTWICKLUNG LFD. JAHR äNDERUNG
               05  BNK-FREI-VERBR               PIC  9(005)V99.
      *            VERBRAUCHTER FREISTELLUNGSBETRAG
               05  BNK-AEND-FREI-VERBR-KZ       PIC  X(001).
      *            J = VERBRAUCHTER FREISTELLUNGSBETRAG äNDERUNG
               05  BNK-ZINS-AB-ST               PIC  9(006)V99.
      *            ZINSABSCHLAGSTEUER
               05  BNK-AEND-ZINS-AB-ST-KZ       PIC  X(001).
      *            J = ZINSABSCHLAGSTEUER äNDERUNG
190499*        05  FILLER                  PIC  X(05).
190499         05  BNK-KTO-ART                  PIC  X(002).
      *            Kontoart (DE, VL)
190902         05  BNK-ANTEILE-AKT              PIC  9(006)V999.
190902*            WERTPAPIERKENN-NUMMER
190902         05  BNK-AEND-ANTEILE-AKT-KZ      PIC  X(001).
190902*            J = ANTEILE ÄNDERUNG
190499*        05  FILLER                       PIC  X(003).
190902         05  FILLER                       PIC  X(100).
      *            RESERVE


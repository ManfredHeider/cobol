      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2469.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2469.CPv  $
      *
      *   Rev 2.2   Jun 03 2011 08:06:30   t13ex15
      *FKI01344: Santander Fehlerbereinigung DRKI2469 Satzlänge
      *
      *   Rev 2.1   May 23 2011 11:27:34   t13ex15
      *FKI01344: Santander PGM und Copys Namensanpassungen u. Neu-C
      *opy
      *
      *   Rev 2.0   May 19 2011 10:51:30   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 2 4 6 9  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI FEHLSATZ                       *
      *          BAUFINANZIERUNG von SEB                               *
      *                                                                *
      *    LAENGE =  250 BYTES      ERSTELLT  AM 19.05.11              *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 250                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *FKI01344!19.05.11! Neuerstellung                    !SWZ        *
      *----------------------------------------------------------------*
      *FKI01344!03.06.11! Fehler im Filler (Satzlänge)     !SWZ        *
      *----------------------------------------------------------------*
      ******************************************************************
      *
       01 BNK-FEHL-SATZ.
          05 'XX'-FS-SA                 PIC  X(002).
          05 'XX'-FS-GES                PIC  9(002).
          05 'XX'-FS-VTRGNR             PIC  9(015).
          05 'XX'-FS-BLZ                PIC  9(008).
          05 'XX'-FS-KONTONR            PIC  9(010).
          05 'XX'-FS-DATUM-EROEFFNUNG   PIC  9(008).
          05 'XX'-FS-DATUM-SCHLIESSUNG  PIC  9(008).
          05 'XX'-FS-DARLEHNSSUMME      PIC  9(012)V99.
          05 'XX'-FS-DAT-ENDE-ZINSFEST  PIC  9(008).
          05 'XX'-FS-AKT-DARLEHNBETRG   PIC  9(012)V99.
          05 'XX'-FS-BEGINN-TILGUNG     PIC  9(008).
          05 'XX'-FS-TILGUNGSART        PIC  X(001).
          05 'XX'-FS-START-TILGUNGSSATZ PIC  9(002)V9999.
          05 'XX'-FS-SOLL-ZINSSATZ      PIC  9(002)V9999.
          05 'XX'-FS-ZAHLWEISE          PIC  9(001).
          05 'XX'-FS-RATE-IN-EURO       PIC  9(012)V99.
          05 'XX'-FS-DEL-KZ             PIC  X(001).
          05 'XX'-FS-NOMINALZINS        PIC  9(002)V9999.
          05 'XX'-FS-DARLEHENSART       PIC  X(001).
190511    05 'XX'-FS-NICHT-GENUTZT      PIC  X(019).
190511    05 'XX'-FS-SOLL-ZINSSATZ      PIC  9(002)V9999.
      *   05 'XX'-FS-RESERVE            PIC  X(124).
190511*   05 'XX'-FS-RESERVE            PIC  X(117).
020611*   05 'XX'-FS-RESERVE            PIC  X(091).
020611    05 'XX'-FS-RESERVE            PIC  X(092).


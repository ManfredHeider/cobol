      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1770.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1770.CPv  $
      *
      *   Rev 2.1   Apr 18 2005 12:32:52   t13ex12
      *AKI00478: Auswertung DWS-Vermögenssparplan
      *
      *   Rev 2.0   Mar 15 2005 16:30:14   t13ex12
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Verträge und VNs von DWS-Vermögenssparplänen*
      *    aus Daisy.                                                  *
      *                                                                *
      *    Eingabedatei für PMKI0740                                   *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge     = 150                                             *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *15.03.05!NEUERSTELLUNG                           !v.d.Goltz! 01 *
      *AKI00478!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKI1770.
           05  VSNR                          PIC  X(015).
           05  FILLER                        PIC  X(001).
           05  BEGINN-PRODUKT                PIC  X(010).
           05  VERTRAGSID                    PIC  X(013).
           05  VN-ID                         PIC  X(013).
           05  KUNDENNR                      PIC S9(009) COMP.
           05  FILLER                        PIC  X(001).
           05  VERTAGSBEGINN                 PIC  X(010).
           05  FILLER                        PIC  X(001).
           05  VERTAGSABLAUF                 PIC  X(010).
           05  FILLER                        PIC  X(001).


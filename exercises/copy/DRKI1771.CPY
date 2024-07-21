      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1771.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1771.CPv  $
      *
      *   Rev 2.1   Apr 18 2005 12:33:02   t13ex12
      *AKI00478: Auswertung DWS-Vermögenssparplan
      *
      *   Rev 2.0   Mar 15 2005 16:30:30   t13ex12
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Sparplandaten von Kunden mit                *
      *     DWS-Vermögenssparplänen und DIT- oder DWS-Sparplan.        *
      *                                                                *
      *    Ausgabedatei von PMKI0740                                   *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge     = 300                                             *
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
       01  DRKI1771.
           05  KUNDENNR                      PIC  9(009).
           05  KOMMA1                        PIC  X(001).
           05  KUNDENNAME                    PIC  X(030).
           05  KOMMA2                        PIC  X(001).
           05  KUNDENVORNAME                 PIC  X(030).
           05  KOMMA3                        PIC  X(001).
           05  VSP-VB-NR                     PIC  9(007).
      *          VB-Nr. vom Betreuer des DWS-Vermögenssparplans
           05  KOMMA4                        PIC  X(001).
           05  VB-NAME                       PIC  X(030).
           05  KOMMA5                        PIC  X(001).
           05  VB-VORNAME                    PIC  X(030).
           05  KOMMA6                        PIC  X(001).
           05  VSP-DIR-NR                    PIC  9(003).
      *          Dir-Nr. vom Betreuer des DWS-Vermögenssparplans
           05  KOMMA7                        PIC  X(001).
           05  DWS-VSP-VERTRAGSNR            PIC  X(015).
      *          Vertragsnr. von DWS-Vermögenssparplan
           05  KOMMA8                        PIC  X(001).
           05  DWS-VSP-EDATUM                PIC  X(010).
      *          Eröffnungsdatum von DWS-Vermögenssparplan
           05  KOMMA9                        PIC  X(001).
           05  GES                           PIC  9(002).
      *          Ges-Nr. von geschlossenen Sparplan
           05  KOMMA10                       PIC  X(001).
           05  VERTRAGSNR                    PIC  X(015).
      *          Vertragsnr. von geschlossenen Sparplan
           05  KOMMA11                       PIC  X(001).
           05  SP-EDATUM                     PIC  X(010).
      *          Eröffnungsdatum Sparplan
           05  KOMMA12                       PIC  X(001).
           05  DWS-SP-WKN                    PIC  X(006).
      *          Wertpapierkennr von DWS-Sparplan
           05  KOMMA13                       PIC  X(001).
           05  DWS-SPARRATE                  PIC  X(007).
      *          Sparrate DWS-Sparplan
           05  KOMMA14                       PIC  X(001).
           05  SP-SDATUM                     PIC  X(010).
      *          Schliessungsdatum Sparplan
           05  KOMMA15                       PIC  X(001).
           05  ABLAUFDATUM                   PIC  X(010).
      *          Vertragsablaufdatum
           05  KOMMA16                       PIC  X(001).
           05  VB-NR                         PIC  9(007).
      *          VB-Nr. vom Betreuer des geschlossenen Sparplans
           05  KOMMA17                       PIC  X(001).
           05  DIR-NR                        PIC  9(003).
      *          Dir-Nr. vom Betreuer des geschlossenen Sparplans
           05  KOMMA18                       PIC  X(001).
           05  KZ-SDATUM-VOR-NACH            PIC  X(001).
      *        V=Normaler Sparplan wurde vor neuer DWS-VSP geschlossen
      *        N=Normaler Sparplan wurde nach neuer DWS-VSP geschlossen
           05  FILLER                        PIC  X(047).



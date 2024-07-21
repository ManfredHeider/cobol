      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2505.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2505.CPv  $
      *
      *   Rev 2.1   Jul 11 2011 16:17:42   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 16 2011 09:14:32   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 5 0 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER DWS-ZULAGEN                                *
      *                                                                *
      *    SATZLÄNGE  100                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *24.06.11! neu                                   !M. Dicks ! 001 *
      *AKI01040!                                       !         !     *
      ******************************************************************
       01  DRKI2505.

      *    KEY   *******************************************************
           05  ZUL-GES                     PIC  9(002).

           05  ZUL-KEY.
               10  ZUL-VNR                 PIC  9(010).
               10  ZUL-VNR-DEPOTNR         PIC  9(002).
               10  ZUL-PORTFOLIONR         PIC  X(010).
               10  ZUL-BEITRJAHR           PIC  9(004).

      *    DATEN *******************************************************
           05  ZUL-ZULAGEN-BEITRJAHR       PIC  9(011)V9(002).
           05  ZUL-AEND-ZULAGEN-BEITRJAHR-KZ PIC  X(001).
      *        J WENN BEITRAG      GEAENDERT

      *    Vergleichsfelder ********************************************
           05  ZUL-ZULAGEN-BEITRJAHR-ALT   PIC  9(011)V9(002).

      *    Zwischenfelder **********************************************
           05  ZUL-PRODUKT-ID              PIC  9(003).
           05  ZUL-ISIN                    PIC  X(012).
           05  ZUL-WKN                     PIC  X(006).
           05  ZUL-ZAHLUNG-LFDNR           PIC  9(004).
           05  ZUL-LFDNR                   PIC  9(004).

      *    Aktualisierungs-Kennzeichen ********************************
           05  ZUL-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      ******************************************************************

           05  ZUL-RESERVE                 PIC  X(015).

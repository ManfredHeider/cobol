      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.12.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2717.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2717.CPv  $
      *
      *   Rev 2.1   Dec 18 2012 06:42:56   t13ex39
      *AKI01165: OnlineKI: Erweiterung Anzeige zu db VL-Depots
      *
      *   Rev 2.0   Dec 12 2012 13:10:56   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 1 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE   20 BYTE - 23. 10. 2012                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI01165! NEU wegen zusätzliche Attribute zu    !WHO      !     *
      *        !           VL-Sperrfristen             !         !     *
       01  DRKI2717.
           05  BA-GES                        PIC  9(002).
           05  BA-VNR                        PIC  9(015).
           05  BA-ANTEIL-LFDNR               PIC  9(002).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
           05  BA-VL-KZ                      PIC  X(01).



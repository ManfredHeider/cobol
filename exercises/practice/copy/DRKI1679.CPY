      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1679.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI1679.CPv  $
      *
      *   Rev 2.9   Jan 09 2020 13:50:40   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.8   Jan 25 2018 07:03:20   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.7   Oct 27 2014 09:59:58   t13ex39
      *AKI00936: Anpassungen DeuBa Erm. -Progr. wg. Hinweise, Warnu
      *ngen
      *
      *   Rev 2.6   Jun 20 2013 08:30:42   t13ex73
      *AKI01204: SEPA - DB-Online KFZ/SUH KSDS Bereinigung
      *
      *   Rev 2.5   Jun 19 2013 13:46:18   t13ex73
      *AKI01204: SEPA - DB-Online KFZ/SUH KSDS Bereinigung
      *
      *   Rev 2.4   Jun 17 2013 12:35:02   t13ex73
      *AKI01204: SEPA - DB-Online KFZ/SUH KSDS Bereinigung
      *
      *   Rev 2.3   Feb 21 2013 11:45:02   t13ex14
      *AKI01186: KSDS-Bereinigung (AMV) u. Erfassung SPERRLISTE
      *
      *   Rev 2.2   Jul 11 2011 16:15:58   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.1   Jul 05 2004 10:33:44   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 02 2004 09:52:38   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für UNLOAD VERTRAGSDATEN VON DB               *
      *                                                                *
      *    SATZLÄNGE   45                                              *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01040!04.07.11! NEU                              !MDI        *
      * ---------------------------------------------------------------*
      *AKI01186!13.11.12! Änderung Feldnamen               !WMI        *
      * ---------------------------------------------------------------*
170614*AKI00936!17.06.14! Redefinitionsnamen deändert      ! WHO      *
      * -------------------------------------------------------------*
      * AKI01747!16.12.19!    ! ABLAUFDATUM für TCKI0300  ! WHO      *
      *         !        !    ! mit aufgenommen           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1679.
           05 VT-VERTID             PIC X(013).
         02 VT-REST.
           05 VT-VTNR               PIC X(015).
           05 FILLER                PIC X(001).
           05 VT-GES                PIC S9(004) BINARY.
131112*    05 VT-NOTIZ              PIC X(013).
131112     05 VT-PERSID             PIC X(013).
           05 FILLER REDEFINES VT-PERSID.
              10 VT-ABLAUFDATUM     PIC X(10).
              10 NI-VT-ABLAUFDATUM  PIC X(01).
              10 FILLER             PIC X(02).
131112*    05 VT-NI-NOTIZ           PIC X(001).
131112     05 FILLER                PIC X(001).
170614*  02 DWS-ZULAGEN-KEY      REDEFINES VT-REST  PIC X(28).
170614   02 VT-DWS-ZULAGEN-KEY   REDEFINES VT-REST  PIC X(28).




      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1537.CPY  $
      *    $Revision:   2.18  $
      *
      *    $Log:   L:/mf/CPY/DRKI1537.CPv  $
      *
      *   Rev 2.18   Feb 24 2020 12:05:04   t13ex39
      *AKI01762: Deutsche Bank - Konto- & Depotwechselservice - Mic
      *roservice
      *
      *   Rev 2.17   Jul 24 2017 14:37:54   t13ex39
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en
      *
      *   Rev 2.16   Jul 05 2017 11:56:08   t13ex39
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en
      *
      *   Rev 2.15   Jun 26 2017 10:34:24   t13ex39
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en
      *
      *   Rev 2.14   Jul 21 2016 11:51:00   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.13   Jul 20 2016 10:24:34   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.12   Jul 20 2016 10:13:54   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.11   Jul 20 2016 10:00:42   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.10   Sep 02 2013 10:10:42   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.9   Sep 02 2013 10:01:50   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.8   Jan 31 2012 10:02:44   t13ex39
      *AKI01085: Zusätzliche Anzeige in der OKI bei "Junges Konto"
      *
      *   Rev 2.7   Apr 27 2010 09:35:04   t13ex39
      *FKI01292: Fehler bei DeuBa Kreditkartenaktualisierung
      *
      *   Rev 2.6   Jan 05 2010 10:22:10   t13ex14
      *FKI01267: DeuBa Kreditkarten-Aktualisierung - Aufbereitung +
      * Aktualisierung
      *
      *   Rev 2.5   Nov 05 2008 16:22:24   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.4   Nov 05 2008 10:52:32   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.3   Jun 27 2006 11:45:04   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en  OZPs
      *
      *   Rev 2.2   Jun 26 2006 09:52:12   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en  OZPs
      *
      *   Rev 2.1   Jul 10 2003 13:46:44   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *
      *   Rev 2.0   Feb 27 1999 12:53:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:54:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 5 3 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN DBPRODUKT FUER DEUTSCHE BANK       *
      *                                                                *
      *    SATZLÄNGE   058                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.02.03!NEUERSTELLUNG                          !HORNUNG  ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *01.04.03!ÄNDERUNG                               !GOLÜCKE  !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00587!16.06.06!2.2 ! Neue Felder                 !IFR        *
      *----------------------------------------------------------------*
      *AKI00609!29.08.08!2.4 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
      *        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *        !        !    !  SPARDATEN                  !           *
      *----------------------------------------------------------------*
      *AKI00609!09.09.08!2.4 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
      *        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *        !        !    !  BAUFI                      !           *
      *----------------------------------------------------------------*
      *AKI00609!22.10.08!2.4 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
      *        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *        !        !    !  PRIVATKREDIT               !           *
      *----------------------------------------------------------------*
      *AKI00609!12.11.08!2.5 !ZAHLWEISE FORMAT GEÄNDERT    !SCHLEICHER *
      *        !        !    !  (VERLÄNGERT)               !           *
      *----------------------------------------------------------------*
      *FKI01292!20.04.10!2.11! Kreditkarten-Aufbereitung   ! WHO       *
      *        !        !    ! überarbeitet, fehlende      !           *
      *        !        !    ! Attribute für GHP ergänzt   !           *
      *        !        !    !                             !           *
      *----------------------------------------------------------------*
      *AKI01085!29.11.11!3.0 !'Ende der Begünstigung'      !WHO        *
      *----------------------------------------------------------------*
      *AKI01204!13.06.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
      *        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *--------------------------------------------------------------*
      * AKI01449!24.06.16! neues Feld. Bchungsdatum Saldo ! WHO      *
      *         !        !                                !          *
      *--------------------------------------------------------------*
      * AKI01506!01.04.17! Deuba KK aus GHP und DBP nach  !          *
      *         !        ! BANKPRODUKT                    !          *
      *         !        ! nur noch BIC / allen anderer   !          *
      *         !        ! Atribute entfernt              !          *
      *---------!--------!--------------------------------!----------*
      * AKI01762!19.01.20! NEUES ATTRIBUT "GHPBLZKUNDEN"  ! T13EX39
      *         !        ! UND BPKBLZKUNDEN AUS KUNDENBLZ !
      *         !        ! VERARBEITEN                    !
      *         !        !                                !          *
     ******************************************************************
       01  DRKI1537.

           05  DB-GES                             PIC  9(02).
           05  DB-VNR                             PIC  9(15).
           05  DB-BIC                             PIC  X(011).
           05  DB-BLZ-KUNDE                       PIC  X(010).
           05  FILLER                             PIC  X(020).
      *        RESERVE
      ******************************************************************









      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0259.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0259.CPv  $
      *
      *   Rev 2.3   Apr 30 2009 08:26:38   t13ex14
      *AKI00805: Ermittlung, Aufbereitung, Aktualisierung Bedingungen
      *
      *   Rev 2.2   Feb 23 2009 07:30:26   t13ex14
      *AKI00731: Kfz-Versicherungsbedingungen
      *
      *   Rev 2.1   Apr 19 2000 12:27:40   T13EX41
      *AKI00065
      *- Vergrößerung von Schlüsselfeldern
      *
      *   Rev 2.0   Feb 27 1999 12:54:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   18 Feb 1999 18:02:38   T13EX12
      *AKI00015 - AMV-Vers.-Bedingungen, aufbereitet
      *
      *   Rev 1.0   07 Jul 1998 11:48:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 2 5 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  AUSGABE AUS AUFBEREITUNGSPROGRAMMEN AM-VERS,                  *
      *  COPYSTRECKE FUER VERSICHERUNGESBEDINGUNGEN, MAX. 15           *
      *    JE VERTRAG - FUER ALLE ZWEIGE. FUER KFZ WIRD TEXT UND       *
      *    KEIN ID GESPEICHERT:                                        *
      *                                                                *
000330*    SATZLÄNGE   39                                              *
150109*    SATZLÄNGE   50                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.06.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      * 2.09.98!'VS-BED-ID' auf 'S9(4) BINARY' geändert!KH Müller! 02  *
      *        !wg. Übertragung von Support-Tabelle    !         !     *
      *        !                                       !         !     *
      *13.10.98!'VS-BED-ZWEIG' und                     !KH Müller! 03  *
      *        !'VS-BED-ZWEIG-TEXT' aufgenommen        !         !     *
      *30.03.00!VS-BED-ID IST JETZT INTEGER            !C.HEINECK! 04  *
      *AKI00065!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
150109*AKI00731!VS-BED-BEZ-KFZ auf Länge 21 geändert   !WMI      !     *
      *15.01.09!wie in DTA                             !         !     *
      *--------!---------------------------------------!---------!-----*
260309*AKI00805!VS-BED-BEZ-KFZ in 2 Felder aufgeteilt  !WMI      !     *
      *--------!---------------------------------------!---------!-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0259.

           05  VS-BED-GES                 PIC  9(002).
           05  VS-BED-VNR                 PIC  9(015).

           05  VS-BED-PRODUKT-ID          PIC  9(002).

000330     05  VS-BED-ID                  PIC S9(009) BINARY.
150109*    05  VS-BED-BEZ-KFZ             PIC  X(010).
260309*    05  VS-BED-BEZ-KFZ             PIC  X(021).
260309     05  VS-BED-BEZ-KFZ.
260309         10  VS-BED-BEZ-KFZ-KEY     PIC  X(002).
260309         10  VS-BED-BEZ-KFZ-TXT     PIC  X(019).

           05  VS-BED-ZWEIG               PIC  9(002).
           05  VS-BED-ZWEIG-TEXT          PIC  X(003).

           05  VS-BED-AKTUAL-KZ           PIC  X(001).
      *        I = INSERT
      *        D = DELETE
      ******************************************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0174.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0174.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 11:07:12   T13EX12
      *AFKI0014 - Aufbereitete Bank-Freistellungsauftragsdaten
      *
      *   Rev 1.0   29 Sep 1997 11:54:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: FREISTELLUNGSAUFTRAG                         *
      *                                                                *
      *    SATZLÄNGE   36                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.05.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0174.

           05  FA-GES                     PIC  9(02).
           05  FA-VNR                     PIC  9(15).

           05  FA-FREI-BETRAG             PIC  9(05)V99.
      *        FREISTELLUNGAUFTRAG
           05  FA-ANZ-DEPOT               PIC  9(01).
      *        ANZAHL DEPOTS FUER FREISTELLUNGSAUFTRAG
           05  FA-FREI-ABLAUF-DAT         PIC  X(10).
      *        ABLAUFDATUM FUER FREISTELLUNGSAUFTRAG

           05  FA-AKTUAL-KZ               PIC  X(01).
      *        I = INSERT, NUR FUER NEUE VERTRAEGE
      *        U = UPDATE
      *        D = DELETE
      ******************************************************************

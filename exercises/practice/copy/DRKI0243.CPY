      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0243.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0243.CPv  $
      *
      *   Rev 2.4   Feb 28 2012 09:31:02   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.3   Feb 27 2012 13:23:38   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.2   Jul 10 2003 18:30:54   t13ex12
      *AKI00354 Ges-Nr. nun 3-stellig wg. IT-Future
      *
      *   Rev 2.1   Jun 25 2002 17:01:50   t13ex12
      *AKI00274 geänderte Längen für DWS-Preisfelder
      *
      *   Rev 2.0   Feb 27 1999 12:54:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Feb 1999 13:40:34   T13EX12
      *AKI00014 - Aufbereitete Preise
      *
      *   Rev 1.0   23 Jun 1998 13:50:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 2 4 3    ########                    *
      *              #####################                             *
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM FUER DIE              *
      *      RüCKNAHMEPREISE BANKEN                                    *
      *                                                                *
      *    SATZLÄNGE   43                                              *
      *    SATZLÄNGE   100 (ab Juni 2002)                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *26.06.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *----------------------------------------------------------------*
      *04.06.02!Felder Rücknahme- und Ausgabe-Preis wg.!v.d.Goltz! 02  *
      *AKI00274!DWS erweitert.                         !         !     *
      *----------------------------------------------------------------*
      *12.06.03! Ges.-Nr. wegen IT-Future 3-stellig.   !v.d.Goltz! 03  *
      *AKI00354!                                       !         !     *
      * ---------------------------------------------------------------*
      *27.02.12! Attributvergrösserung Preis           !SWZ      ! 04  *
      *AKI01121!                                       !         !     *
      * ---------------------------------------------------------------*
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0243.
      *
120603*    05  R-GES                PIC  9(002).
120603     05  R-GES                PIC  9(003).
           05  R-WP-KENN-NR         PIC  X(006).
      *
040602*    05  R-RUECK-PREIS        PIC  9(003)V99.
270212*    05  R-RUECK-PREIS        PIC  9(005)V99.
270212     05  R-RUECK-PREIS        PIC  9(007)V99.
      *        Rücknahmepreis
040602*    05  R-AUS-PREIS          PIC  9(003)V99.
270212*    05  R-AUS-PREIS          PIC  9(005)V99.
270212     05  R-AUS-PREIS          PIC  9(007)V99.
      *        Ausgabepreis
           05  R-FONDSTAND-DAT      PIC  X(010).
      *        Datum Rücknahmepreis
           05  R-WERT-ENTW          PIC S9(003)V99.
      *        Wertentwicklung + / -
           05  R-WERT-ENTW-DAT      PIC  X(010).
      *        Datum Wertentwicklung
040602     05  R-ISIN-NR            PIC  X(012).
270212     05  FILLER               PIC  X(036).
270212*    05  FILLER               PIC  X(040).
120603*    05  FILLER               PIC  X(041).

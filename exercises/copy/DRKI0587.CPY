      * P13SP99   Stand: 19/09/98    16:30:38
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0587.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0587.CPv  $
      *
      *   Rev 2.3   Jun 12 2017 18:04:34   t13ex27
      *AKI01519: Bereinigung OZP für Tabelle KUNDENUEBERTRAGUNG
      *
      *   Rev 2.2   Jul 27 2004 13:10:16   t13ex27
      *AKI00427: Ladedaten für die Tabelle KUNDENUEBERTRAGUNG
      *
      *   Rev 2.1   Aug 19 2003 12:34:50   t13ex39
      *FKI00769.UE001 Abschluss-VB
      *
      *   Rev 2.0   Apr 16 1999 12:50:34   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:50:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0587  FUNKTIONSAUFRUFE FUER              *
      *                           KUNDENUEBERTRAGUNG                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    17.08.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.08.98! 01 ! NEUERSTELLUNG             !BRENNECKE *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00769!06.08.03! 03 ! zus.SELECT DRKI1570       !          *
      * AKI00427!07.07.04!2.2 ! zus.SELECT DRKI1701       !I. Frenzel*
      * AKI01519!29.05.17!2.3 ! DRKI_1570 und DRKI_1701   !I. Frenzel*
      *         !        !    ! entfernt                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXKI0077.
           05 PI-ZXKI0077-FUNKTION            PIC X(008).
              88 PI-ZXKI0077-DRKI0588-LIST        VALUE 'DRKI0588'.
290517*       88 PI-ZXKI0077-DRKI1570-LIST        VALUE 'DRKI1570'.
290517*       88 PI-ZXKI0077-DRKI1701-LIST        VALUE 'DRKI1701'.



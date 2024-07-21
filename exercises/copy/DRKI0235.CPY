      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0235.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0235.CPv  $
      *
      *   Rev 2.3   Dec 07 2009 18:21:50   t13ex48
      *AKI00667: Bereinigung der KI-DB: Neue Tablespaces für Tabs. 
      *der VB-SST
      *
      *   Rev 2.2   Feb 11 2002 10:12:22   t13ex49
      *AKI00246 - WV-PC-Kennwort
      *
      *   Rev 2.1   Apr 29 1999 10:30:04   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:54:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:32   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jun 1998 13:50:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0235  FUNKTIONEN STANDARD-OZP            *
      *                           BENUTZER                           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!14.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      * fki00029!22.04.99! 02 ! Update neu                ! VZG      *
      *---------!--------!----!---------------------------!----------*
071209* AKI00667!07.12.09!    ! OZP ZTKI0016 ersetzt      !HSE       *
      *         !             ! ZTKI0609. Funktionskonst. !          *
      *         !             ! entsprechend umbenannt    !          *
      *---------!--------!----!---------------------------!----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0016.
           05 PI-ZTKI0016-FUNKTION             PIC X(008).
              88 PI-ZTKI0016-DRKI0334              VALUE 'DRKI0334'.
              88 PI-ZTKI0016-DRKI0236              VALUE 'DRKI0236'.
              88 PI-ZTKI0016-DRKI0237              VALUE 'DRKI0237'.
              88 PI-ZTKI0016-DRKI0836              VALUE 'DRKI0836'.
              88 PI-ZTKI0016-DRKI1303              VALUE 'DRKI1303'.

      * P13SP99   Stand: 17/11/98    15:12:10
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0540.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0540.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 15:45:42   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Sonstige 
      *Programme
      *
      *   Rev 2.1   Sep 07 1999 22:20:56   T13EX19
      * 
      *
      *   Rev 2.0   Apr 16 1999 13:17:08   T13EX19
      *aki00028
      *
      *   Rev 1.0   07 Aug 1998 14:46:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0540 FUNKTION:ADRESSID ERMITTELN         *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.08.98! 01 !NEU                        !BRENNECKE *
      * AKI00886!29.11.11!2.1 ! Neue Kundenaktualisierung ! IFR      *
      *         !        !    ! Funktion nach ZWKI0345    !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0540                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: Geschäftsstellen-Id ermitteln                   *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0540.
           05  PI-DRKI0540-KEY.
              10 PI-DRKI0540-KRTVERTRAGSID        PIC  X(013).

      ***
       01 PO-DRKI0540.
          05 PO-DRKI0540-RC                       PIC  X(003).
             88 PO-DRKI0540-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0540-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRKI0540-DATEN.
             10 PO-DRKI0540-KRTGESCHAEFTSSTID     PIC S9(004) COMP.
      *
      ******** Ende Copy-Member **************************************

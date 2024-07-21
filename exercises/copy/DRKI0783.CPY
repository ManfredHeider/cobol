      *---------------------------------------------------------------*
      *    Standard f•r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0783.CPY  $
 
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0783.CPv  $
      *
      *   Rev 2.8   Jun 28 2016 12:13:06   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.7   Jun 28 2016 11:46:46   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.6   Jun 28 2016 10:40:30   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.5   Apr 28 2016 08:05:52   t13ex78
      *AKI01427: OZP's & DCLGEN's
      *
      *   Rev 2.4   Nov 21 2013 10:13:26   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.3   Nov 12 2013 12:00:52   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.2   Nov 24 2010 11:34:18   t13ae31
      *AKI00995: KFZ dynamische Vertragsanzeige
      *
      *   Rev 2.1   Mar 10 1999 12:06:50   I13PC48
      *Vorbereitung NTPut
      *
      *   Rev 2.0   Mar 02 1999 11:25:58   I13PC48
      *Version 2.0 nach Umstellung auf NT
      *
      *   Rev 1.0   06 Oct 1998 15:39:52   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0783                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0783                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.06.2016 / 12:10:29 / T13EX78
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0783
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0783.
           05  PI-DRKI0783-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0783-KFZVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI0783.
           05  PO-DRKI0783-RC                     PIC  X(003).
               88 PO-DRKI0783-RC-OK               VALUE 'OK '.
               88 PO-DRKI0783-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************


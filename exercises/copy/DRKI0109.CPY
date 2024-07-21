      * T13EX48   Stand: 21/01/99    12:00:56
      * T13EX31   Stand: 02/12/97    14:19:21
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0109.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0109.CPv  $
      *
      *   Rev 2.2   26 Jun 2001 10:04:34   t13ex13
      * 
      *
      *   Rev 2.1   Apr 16 1999 18:40:50   T13EX48
      *AKI00028
      *PI/PO Funktion DRKI0109
      *
      *   Rev 2.0   Feb 27 1999 12:52:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:47:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0109                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0109                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  25.04.01/13.55.34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00165!23.03.01! 2.1!Neuerstellt über Generator ! A.Blaess *
      *         !        !    !Ergänzung DRKI1209 (DELETE)!          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0109
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0109.
           05 PI-DRKI0109-KEY.
              10 PI-DRKI0109-DOKUMENTID         PIC  X(013).
           05 PI-DRKI0109-DATEN.
              10 PI-DRKI0109-ERSTELLDATUM       PIC  X(010).
              10 PI-DRKI0109-ERSTELLERID        PIC  X(013).
              10 PI-DRKI0109-ERSTELLERROLLE     PIC  X(001).
              10 PI-DRKI0109-SUBTYPEDOKUMENT    PIC  X(004).
      *
       01  PO-DRKI0109.
           05 PO-DRKI0109-RC                    PIC  X(003).
              88 PO-DRKI0109-RC-OK                   VALUE 'OK '.
              88 PO-DRKI0109-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

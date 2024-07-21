      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAS0011.CPv  $
      *
      *   Rev 2.1   20 Mar 2001 10:02:38   T13AE13
      *AAS00264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.0   06 Mar 2001 11:47:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!07.03.01! 01 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0011
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0011.
           05 PI-DRAS0011-MA-ST-NR            PIC  S9(009) COMP.
           05 PI-DRAS0011-STICHTAG            PIC  X(010).
           05 PI-DRAS0011-ERM-MONAT           PIC  X(010).
           05 PI-DRAS0011-DAUER-MONATE        PIC  9(004).
           05 PI-DRAS0011-VM-MONATE           PIC  9(004).
           05 PI-DRAS0011-EH-GRENZE           PIC  S9(009)V9 COMP-3.
           05 PI-DRAS0011-EH-ART              PIC  9(004).
           05 PI-DRAS0011-VERARB-ART          PIC  9(004).

      *
       01  PO-DRAS0011.
           05  PO-DRAS0011-RC                 PIC  X(003).
               88 PO-DRAS0011-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0011-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0011-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0011-RC-ERR                       VALUE 'ERR'.
      *
           05 PO-DRAS0011-DATEN.
              10  PO-DRAS0011-EH-SUMME        PIC  S9(009)V9 COMP-3.
              10  PO-DRAS0011-ANZ-NEUE-VB     PIC  9(004).

      ******* ENDE COPY-MEMBER **************************************

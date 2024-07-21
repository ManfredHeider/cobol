      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1351.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1351.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:29:54   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:54:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:20:30   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   12 Jul 2000 11:48:44   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1351                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1351                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    16.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00088!16.03.00!2.1 ! NEU                       ! STORCH   *
      * APA00622!12.02.08!2.2 ! PI-KNR-VGES HINZU         ! STORCH   *
      * APA01101!14.03.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1351
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UMBUCHUNGSANZAHL ERMITTELN (UPPA0117) <<<
      *
      ****************************************************************
      *
       01  PI-DRPA1351.
           05  PI-DRPA1351-PGEB-GES           PIC S9(004) COMP.
           05  PI-DRPA1351-STICHTAG           PIC X(010).
           05  PI-DRPA1351-S-MANDANT          PIC X(001).
           05  PI-DRPA1351-KNR-MANDANT        PIC S9(004) COMP.
      *
       01  PO-DRPA1351.
           05  PO-DRPA1351-RC                 PIC  X(003).
               88 PO-DRPA1351-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1351-RC-NFD                       VALUE 'NFD'.
      *
           05  PO-DRPA1351-ANZ-UMBUCH         PIC S9(009) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

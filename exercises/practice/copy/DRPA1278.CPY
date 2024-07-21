      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1278.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1278.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 09:18:08   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Feb 17 2000 16:00:54   T13EX44
      *
      *
      *   Rev 2.1   Aug 23 1999 11:45:22   T13EX21
      *APA00049 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:22   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1278  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.07.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * APA00081!25.01.00!2.02! FUER PGEB-GES UM ZWEI     ! SCHEIDEL *
      *         !        !    ! STELLEN ERGAENZT          !          *
      * APA01932!05.08.15!2.03! FUER PDIF-UMSATZSTEUERBE- ! KÖPRÜCÜ  *
      *         !        !    ! TRÄGE IN LÄNGE ERGÄNZT    !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1278                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1278.
          05 PI-DRPA1278-UFKT               PIC  9(002).
250100*   05 PI-DRPA1278-DATEN              PIC  X(068).
250100*   05 PI-DRPA1278-DATEN              PIC  X(070).
050815    05 PI-DRPA1278-DATEN              PIC  X(082).
      *
       01 PO-DRPA1278.
          05 PO-DRPA1278-RC                 PIC  X(003).
             88 PO-DRPA1278-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************


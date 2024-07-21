      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0349.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0349.CPv  $
      *
      *   Rev 2.1   06 Aug 2001 16:26:44   t13ae46
      *FPA00281: PI-BEREICH UM GESELLSCHAFT
      * ERWEITERN
      *
      *   Rev 2.0   Feb 27 1999 14:28:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 16:20:46   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   May 26 1997 15:22:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0349                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Schrieber                                        *
      *    DATUM:    02.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !02.07.97!    ! NEU                       ! SCHRIEBER*
      * FPA00281!06.08.01! 2.1! PI-BEREICH UM GESELLSCHFT ! WANG     *
      *         !        !    ! ERWEITERN                 !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0349
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0349.
           05  PI-DRPA0349-EINGABE.
               10 PI-DRPA0349-DTA-ID           PIC S9(15)  USAGE COMP-3.
060801         10 PI-DRPA0349-PGEB-GES         PIC S9(04) COMP.
       01  PO-DRPA0349.
           05  PO-DRPA0349-RC                  PIC  X(003).
               88 PO-DRPA0349-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0349-RC-NOT-OK                    VALUE 'NOK'.
      *
           05  PO-DRPA0349-ERGEBNIS.
               10 PO-DRPA0349-ANZAHL-BEWEGUNGEN PIC S9(9)  USAGE COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

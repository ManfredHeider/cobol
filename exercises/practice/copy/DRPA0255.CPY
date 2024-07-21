      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0255.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0255.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:55:36   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Apr 08 1997 09:10:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0255  LETZTE PROV. ERMITTELN FüR ALLE    *
      *                           STELLENTYPEN                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Giese                                           *
      *    DATUM:    05.04.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.04.97! 01 ! Neuerstellung             ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0255                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0255.
          05 PI-DRPA0255-DATEN.
             10 PI-DRPA0255-ST-NR               PIC S9(09) BINARY.

       01 PO-DRPA0255.
          05 PO-DRPA0255-RC                     PIC  X(003).
             88 PO-DRPA0255-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0255-DIFF-PROV              PIC S9(03)V99 COMP-3.
          05 PO-DRPA0255-ST-TYP                 PIC S9(04).
      *
      ******** Ende Copy-Member **************************************

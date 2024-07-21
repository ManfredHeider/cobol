      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0253.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0253.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:55:18   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Apr 03 1997 16:23:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0253                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0253                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    02.04.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !02.04.97!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0253
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    SELECT AUF DUMMY-TABELLE VIAT0006
      ****************************************************************
      *
       01  PI-DRPA0253                        PIC  X(001).
      *
       01  PO-DRPA0253.
           05  PO-DRPA0253-RC                 PIC  X(003).
               88 PO-DRPA0253-RC-OK                        VALUE 'OK '.
      *
           05  PO-DRPA0253-DATEN.
               10 PO-DRPA0253-TIMESTAMP        PIC  X(026).
               10 PO-DRPA0253-TAGESDATUM-EUR   PIC  X(010).
               10 PO-DRPA0253-TAGESDATUM-ISO   PIC  X(010).
               10 PO-DRPA0253-UHRZEIT-ISO      PIC  X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
